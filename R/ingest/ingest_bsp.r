# ingest/ingest_bsp.R
# ─────────────────────────────────────────────────────────────────────────────
# Ingest phase — BSP Philippine Peso per US Dollar XLSX.
#
# Strategy: fully automated structure discovery — no hardcoded sheet names,
# skip values, row numbers, or column positions. Ported directly from the
# original PhilRice internship pipeline (kpi_wfertprices.qmd).
#
#   Sheet  : auto-detected via regex "^monthly$" with "monthly" fallback
#   Rows   : entire sheet read as raw text matrix (col_types = "text")
#             scans all cells row-by-row for first row containing a 4-digit year
#             upward scan from data_start finds last non-empty row = header
#             handles any number of blank rows between title block and headers
#   Columns: detected by data values in transform phase — not header names,
#             not positional. Year and month identified by inspecting values.
#   Ghost columns: trailing empty cols beyond header count padded automatically
#
# BSP file structure (confirmed from live file inspection):
#   col1 = empty
#   col2 = year (4-digit integer, merged cells — NA for non-January months)
#   col3 = month name (January, February, ...)
#   col4 = Average PHP/USD rate  ← this is what we want
#   col5 = End-of-Period rate    ← not needed
#
# Returns: data.frame — raw ingested BSP data, column names cleaned
# ─────────────────────────────────────────────────────────────────────────────

suppressPackageStartupMessages({
  library(readxl)
  library(janitor)
  library(stringr)
  library(glue)
})


#' Ingest the BSP peso/dollar XLSX into a raw data.frame.
#'
#' @param bsp_file_path  Character. Absolute path to the downloaded XLSX.
#'
#' @return data.frame. Raw ingested data — column names cleaned, structure
#'   preserved. Column role detection and transformation handled in
#'   transform/transform_bsp.R.
ingest_bsp <- function(bsp_file_path) {
  log_info("INGEST:BSP", glue("Ingesting: {bsp_file_path}"))

  # ── Auto-detect sheet ──────────────────────────────────────────────────────
  all_sheets <- readxl::excel_sheets(bsp_file_path)
  log_info("INGEST:BSP", glue("Sheets found: {paste(all_sheets, collapse = ', ')}"))

  bsp_sheet <- all_sheets[stringr::str_detect(
    all_sheets, stringr::regex("^monthly$", ignore_case = TRUE)
  )]

  if (length(bsp_sheet) == 0L) {
    bsp_sheet <- all_sheets[stringr::str_detect(
      all_sheets, stringr::regex("monthly", ignore_case = TRUE)
    )]
  }

  if (length(bsp_sheet) == 0L) {
    log_abort("INGEST:BSP", glue(
      "No 'monthly' sheet found. Available: {paste(all_sheets, collapse = ', ')}"
    ))
  }

  bsp_sheet <- bsp_sheet[[1L]]
  log_info("INGEST:BSP", glue("Sheet selected: '{bsp_sheet}'"))

  # ── Read entire sheet as raw text matrix ───────────────────────────────────
  bsp_matrix <- suppressMessages(
    readxl::read_excel(
      bsp_file_path,
      sheet     = bsp_sheet,
      col_names = FALSE,
      col_types = "text"
    )
  )

  # ── Scan all cells row-by-row for first 4-digit year ──────────────────────
  # BSP uses merged year cells — year may appear in any column of the row.
  # We scan every cell in every row until we find a 4-digit integer.
  year_pattern <- stringr::regex("^\\d{4}$")
  data_start   <- NA_integer_

  for (i in seq_len(nrow(bsp_matrix))) {
    row_vals <- as.character(bsp_matrix[i, ])
    if (any(stringr::str_detect(row_vals, year_pattern), na.rm = TRUE)) {
      data_start <- i
      break
    }
  }

  if (is.na(data_start)) {
    log_abort("INGEST:BSP", paste0(
      "Could not find data start row.\n",
      "  Expected a 4-digit year value in any column.\n",
      "  First 10 rows scanned — check raw file structure."
    ))
  }

  # ── Upward scan from data_start — find last non-empty row = header ─────────
  # BSP files have title blocks and blank rows above the headers.
  # data_start - 1 often lands on a blank row. Walk upward and take the
  # last row with at least one non-NA, non-empty cell.
  header_row <- NA_integer_

  for (i in seq(data_start - 1L, 1L, by = -1L)) {
    row_vals  <- as.character(bsp_matrix[i, ])
    non_empty <- row_vals[!is.na(row_vals) & nchar(trimws(row_vals)) > 0L]
    if (length(non_empty) > 0L) {
      header_row <- i
      break
    }
  }

  if (is.na(header_row)) {
    log_abort("INGEST:BSP", glue(
      "No non-empty header row found above data row {data_start}."
    ))
  }

  # ── Diagnostic: print raw matrix layout for visibility in logs ────────────
  log_info("INGEST:BSP", "Raw matrix preview (first 10 rows):")
  for (i in seq_len(min(10L, nrow(bsp_matrix)))) {
    row_preview <- paste(as.character(bsp_matrix[i, ]), collapse = " | ")
    marker <- dplyr::case_when(
      i == header_row ~ " <-- HEADERS",
      i == data_start ~ " <-- DATA START",
      TRUE            ~ ""
    )
    message(glue("  row {formatC(i, width = 2)}: {row_preview}{marker}"))
  }

  log_info("INGEST:BSP", glue(
    "Structure detected — header: row {header_row} | data from: row {data_start}"
  ))

  # ── Build column names from header row ────────────────────────────────────
  header_vals      <- as.character(bsp_matrix[header_row, ])
  actual_col_count <- ncol(bsp_matrix)
  header_count     <- length(header_vals)

  # Ghost column handler — BSP has trailing empty cols beyond header count.
  # Without this, colnames() assignment fails silently or errors.
  if (header_count < actual_col_count) {
    header_vals <- c(
      header_vals,
      paste0("ghost_col_", (header_count + 1L):actual_col_count)
    )
  } else if (header_count > actual_col_count) {
    header_vals <- header_vals[seq_len(actual_col_count)]
  }

  # ── Slice data rows and assign names ──────────────────────────────────────
  bsp_data_raw           <- bsp_matrix[data_start:nrow(bsp_matrix), ]
  colnames(bsp_data_raw) <- header_vals

  bsp_clean <- bsp_data_raw |>
    janitor::clean_names() |>
    janitor::remove_empty(c("rows", "cols"))

  log_info("INGEST:BSP", glue(
    "Ingested: {nrow(bsp_clean)} rows × {ncol(bsp_clean)} cols."
  ))

  bsp_clean
}