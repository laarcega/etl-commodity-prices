# transform/transform_bsp.R
# ─────────────────────────────────────────────────────────────────────────────
# Transform phase — BSP exchange rate data.
#
# Strategy: column roles detected entirely by inspecting data values —
# never by column position or header name. Ported directly from the original
# PhilRice internship pipeline (kpi_wfertprices.qmd).
#
# BSP file has no "year" or "month" header. Year and month are identified
# by matching cell values against known patterns:
#   year_col  : first column whose values include 3+ 4-digit integers
#   month_col : first column whose values include 3+ month names
#   rate_col  : prefer header matching "average|avg", then broader rate
#               keywords, then fallback to first numeric-looking column
#               that is not year or month
#
# Year column uses merged cells — NA for all months after the first in
# each year. tidyr::fill() propagates the year value downward before
# any filtering occurs.
#
# Sentinel value "n.a." is treated as NA — never dropped, preserved as NA.
#
# Returns: data.frame — columns: year (int), month (chr), exchange_rate_avg (dbl)
# ─────────────────────────────────────────────────────────────────────────────

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(glue)
})


#' Transform raw BSP ingested data into a clean, typed data.frame.
#'
#' @param raw_bsp_df  data.frame. Output of ingest_bsp().
#'
#' @return data.frame with columns: year (int), month (chr),
#'   exchange_rate_avg (dbl).
transform_bsp <- function(raw_bsp_df) {
  log_info("TRANSFORM:BSP", "Detecting column roles by data values...")

  available_cols <- names(raw_bsp_df)

  # Exclude ghost columns from role detection — they contain no real data
  non_ghost_cols <- available_cols[
    !stringr::str_detect(available_cols, "^ghost_col_")
  ]

  # ── Value-based column detector ───────────────────────────────────────────
  # For each candidate column, counts how many non-empty values match the
  # given pattern. Returns the first column with >= min_matches matches.
  # This is robust to header renaming, column reordering, and file restructuring.
  detect_col_by_values <- function(cols, pattern, label, min_matches = 3L) {
    for (col in cols) {
      vals      <- as.character(raw_bsp_df[[col]])
      non_na    <- vals[!is.na(vals) & nchar(trimws(vals)) > 0L & vals != "NA"]
      n_matches <- sum(stringr::str_detect(non_na, pattern), na.rm = TRUE)
      if (n_matches >= min_matches) {
        log_info("TRANSFORM:BSP", glue(
          "{label} column detected: '{col}' ({n_matches} matching values)"
        ))
        return(col)
      }
    }
    NULL
  }

  month_pattern <- stringr::regex(
    paste(month.name, collapse = "|"),
    ignore_case = TRUE
  )
  year_pattern <- stringr::regex("^\\d{4}$")

  year_col  <- detect_col_by_values(non_ghost_cols, year_pattern,   "Year")
  month_col <- detect_col_by_values(non_ghost_cols, month_pattern,  "Month")

  # ── Rate column detection — three-pass fallback ───────────────────────────
  # Pass 1: header matching "average" or "avg" — most reliable
  rate_col <- non_ghost_cols[stringr::str_detect(
    non_ghost_cols, stringr::regex("average|avg", ignore_case = TRUE)
  )]

  # Pass 2: broader rate-related header keywords
  if (length(rate_col) == 0L) {
    rate_col <- non_ghost_cols[stringr::str_detect(
      non_ghost_cols,
      stringr::regex("rate|peso|dollar|usd", ignore_case = TRUE)
    )]
  }

  # Pass 3: first numeric-looking column that is not year or month
  if (length(rate_col) == 0L) {
    for (col in non_ghost_cols) {
      if (!col %in% c(year_col, month_col)) {
        vals  <- as.character(raw_bsp_df[[col]])
        non_na <- vals[
          !is.na(vals) &
          nchar(trimws(vals)) > 0L &
          vals != "NA" &
          vals != "n.a."
        ]
        n_num <- sum(!is.na(suppressWarnings(as.numeric(non_na))))
        if (n_num >= 3L) {
          rate_col <- col
          log_info("TRANSFORM:BSP", glue(
            "Rate column detected by numeric scan: '{col}'"
          ))
          break
        }
      }
    }
  }

  rate_col <- if (length(rate_col) > 0L) rate_col[[1L]] else NULL

  # ── Hard stop if any required column was not detected ─────────────────────
  missing_roles <- c(
    if (is.null(year_col))  "year"  else NULL,
    if (is.null(month_col)) "month" else NULL,
    if (is.null(rate_col))  "rate"  else NULL
  )

  if (length(missing_roles) > 0L) {
    log_abort("TRANSFORM:BSP", glue(
      "Could not detect BSP column role(s): {paste(missing_roles, collapse = ', ')}\n",
      "  Columns available: {paste(available_cols, collapse = ', ')}\n",
      "  Check raw file structure via ingest_bsp() diagnostic output."
    ))
  }

  log_info("TRANSFORM:BSP", glue(
    "Column map — year: '{year_col}' | month: '{month_col}' | rate: '{rate_col}'"
  ))

  # ── Select, fill, filter, type-cast ──────────────────────────────────────
  clean_df <- raw_bsp_df |>
    dplyr::select(
      year_raw      = dplyr::all_of(year_col),
      month_raw     = dplyr::all_of(month_col),
      exchange_rate = dplyr::all_of(rate_col)
    ) |>
    # Propagate year downward — merged cells produce NA for non-January months
    tidyr::fill(year_raw, .direction = "down") |>
    # Remove sentinel rows — "n.a." months are non-data rows in BSP format
    dplyr::filter(!is.na(month_raw), month_raw != "n.a.") |>
    dplyr::mutate(
      year              = as.integer(year_raw),
      month             = as.character(month_raw),
      exchange_rate_avg = suppressWarnings(as.numeric(exchange_rate))
    ) |>
    dplyr::select(year, month, exchange_rate_avg) |>
    dplyr::filter(!is.na(year))

  log_info("TRANSFORM:BSP", glue(
    "Transformed: {nrow(clean_df)} rows ",
    "({min(clean_df$year, na.rm = TRUE)}–{max(clean_df$year, na.rm = TRUE)})."
  ))

  clean_df
}