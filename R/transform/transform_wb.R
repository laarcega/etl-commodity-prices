# transform/transform_wb.R
# ─────────────────────────────────────────────────────────────────────────────
# Transform phase — World Bank commodity data.
#
# Strategy:
#   All column selection is name-based via regex — never positional.
#   WB_COLUMNS keys are resolved against ingested column names at runtime,
#   handling unit suffixes (e.g. "phosphate_rock_usd_mt" → "phosphate_rock").
#   Sentinel values "..." and "…" are coerced to NA — rows are never dropped.
#   Month is parsed from WB date format "YYYY M##" into numeric year + month.
#
# Returns: data.frame — columns: year (int), month (int), [WB_COLUMNS] (dbl)
# ─────────────────────────────────────────────────────────────────────────────

suppressPackageStartupMessages({
  library(dplyr)
  library(stringr)
  library(purrr)
  library(glue)
})


#' Transform raw World Bank commodity data into a clean, typed data.frame.
#'
#' @param raw_wb_df   data.frame. Output of ingest_wb().
#' @param wb_columns  Character vector. Commodity column keys from PIPELINE_CONFIG.
#'
#' @return data.frame with columns: year, month, and one column per commodity.
transform_wb <- function(raw_wb_df, wb_columns) {
  log_info("TRANSFORM:WB", glue(
    "Resolving {length(wb_columns)} commodity column(s): ",
    "{paste(wb_columns, collapse = ', ')}"
  ))

  available_cols <- names(raw_wb_df)

  # ── Resolve each commodity key to its ingested column name ────────────────
  # Regex match handles unit suffixes — e.g. "phosphate_rock" matches
  # "phosphate_rock_usd_mt" without any .env changes.
  col_map <- purrr::map(
    purrr::set_names(wb_columns),
    function(col_key) {
      matches <- available_cols[stringr::str_detect(
        available_cols,
        stringr::regex(col_key, ignore_case = TRUE)
      )]

      if (length(matches) == 0L) {
        log_abort("TRANSFORM:WB", glue(
          "No column matching '{col_key}' in ingested WB data.\n",
          "Available columns: {paste(available_cols, collapse = ', ')}"
        ))
      }

      if (length(matches) > 1L) {
        log_warn("TRANSFORM:WB", glue(
          "'{col_key}' matched {length(matches)} columns: ",
          "{paste(matches, collapse = ', ')} — using first: {matches[[1L]]}"
        ))
      }

      matches[[1L]]
    }
  )

  log_info("TRANSFORM:WB", glue(
    "Column map:\n{paste(paste0('  ', names(col_map), ' → ', unlist(col_map)), collapse = '\n')}"
  ))

  # ── Resolve date column ───────────────────────────────────────────────────
  date_col <- available_cols[stringr::str_detect(
    available_cols,
    stringr::regex("^date|^period|^time", ignore_case = TRUE)
  )]
  date_col <- if (length(date_col) > 0L) date_col[[1L]] else available_cols[[1L]]

  # ── Select, rename, parse, coerce ────────────────────────────────────────
  select_expr        <- c(date_raw = date_col, unlist(col_map))
  clean_df           <- raw_wb_df |> dplyr::select(dplyr::all_of(select_expr))
  names(clean_df)[names(clean_df) %in% unlist(col_map)] <- names(col_map)

  clean_df <- clean_df |>
    dplyr::mutate(
      year  = as.integer(stringr::str_extract(date_raw, "\\d{4}")),
      month = as.integer(stringr::str_extract(date_raw, "\\d{2}$"))
    ) |>
    # Sentinel coercion: "..." and "…" → NA. Never drop the row.
    dplyr::mutate(dplyr::across(
      dplyr::all_of(wb_columns),
      ~ as.numeric(
          dplyr::na_if(
            dplyr::na_if(as.character(.x), "\u2026"),
            "..."
          )
        )
    )) |>
    dplyr::select(year, month, dplyr::all_of(wb_columns)) |>
    dplyr::filter(!is.na(year), !is.na(month))

  log_info("TRANSFORM:WB", glue(
    "Transformed: {nrow(clean_df)} rows × {ncol(clean_df)} cols."
  ))

  clean_df
}
