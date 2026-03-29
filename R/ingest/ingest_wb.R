# ingest/ingest_wb.R
# ─────────────────────────────────────────────────────────────────────────────
# Ingest phase — World Bank commodity XLSX.
#
# Strategy: fully automated structure discovery — no hardcoded sheet names,
# skip values, row numbers, or column positions.
#
#   Sheet  : auto-detected via regex "monthly.*price|price.*monthly",
#             fallback to any sheet containing "monthly"
#   Rows   : entire sheet read as raw text matrix (col_types = "text")
#             scans col 1 for first value matching WB date format "YYYY M##"
#             header_row = date_row - 2, unit_row = date_row - 1
#   Columns: header + unit rows concatenated into unique clean names via
#             janitor::clean_names()
#
# Returns: data.frame — raw ingested WB data, column names cleaned
# ─────────────────────────────────────────────────────────────────────────────

suppressPackageStartupMessages({
  library(readxl)
  library(janitor)
  library(stringr)
  library(dplyr)
  library(glue)
})


#' Ingest the World Bank commodity XLSX into a raw data.frame.
#'
#' @param wb_file_path  Character. Absolute path to the downloaded XLSX.
#'
#' @return data.frame. Raw ingested data — column names cleaned, structure
#'   preserved. Transformation is handled in transform/transform_wb.R.
ingest_wb <- function(wb_file_path) {
  log_info("INGEST:WB", glue("Ingesting: {wb_file_path}"))

  # ── Auto-detect sheet ──────────────────────────────────────────────────────
  all_sheets <- readxl::excel_sheets(wb_file_path)
  log_info("INGEST:WB", glue("Sheets found: {paste(all_sheets, collapse = ', ')}"))

  wb_sheet <- all_sheets[stringr::str_detect(
    all_sheets,
    stringr::regex("monthly.*price|price.*monthly", ignore_case = TRUE)
  )]

  if (length(wb_sheet) == 0L) {
    wb_sheet <- all_sheets[stringr::str_detect(
      all_sheets, stringr::regex("monthly", ignore_case = TRUE)
    )]
  }

  if (length(wb_sheet) == 0L) {
    log_abort("INGEST:WB", glue(
      "No 'Monthly Prices' sheet found. Available: {paste(all_sheets, collapse = ', ')}"
    ))
  }

  wb_sheet <- wb_sheet[[1L]]
  log_info("INGEST:WB", glue("Sheet selected: '{wb_sheet}'"))

  # ── Read entire sheet as raw text matrix ───────────────────────────────────
  wb_matrix <- suppressMessages(
    readxl::read_excel(
      wb_file_path,
      sheet     = wb_sheet,
      col_names = FALSE,
      col_types = "text"
    )
  )

  # ── Scan col 1 for WB date pattern: "YYYY M##" ────────────────────────────
  date_pattern <- stringr::regex("^\\d{4}\\s*M\\d{2}$", ignore_case = TRUE)
  col1_values  <- as.character(wb_matrix[[1]])
  match_idx    <- which(stringr::str_detect(col1_values, date_pattern))

  if (length(match_idx) == 0L) {
    log_abort("INGEST:WB", paste0(
      "Could not find data start row. Expected 'YYYY M##' in column 1.\n",
      "First 10 values: ", paste(head(col1_values, 10L), collapse = " | ")
    ))
  }

  data_start <- match_idx[[1L]]
  header_row <- data_start - 2L
  unit_row   <- data_start - 1L

  if (header_row < 1L) {
    log_abort("INGEST:WB", glue(
      "Data starts at row {data_start} — insufficient rows above ",
      "for header ({header_row}) and unit ({unit_row}) rows."
    ))
  }

  log_info("INGEST:WB", glue(
    "Structure detected — header: row {header_row} | ",
    "units: row {unit_row} | data from: row {data_start}"
  ))

  # ── Build column names: header + unit concatenation ───────────────────────
  headers <- as.character(wb_matrix[header_row, ])
  units   <- as.character(wb_matrix[unit_row,   ])

  clean_cols <- dplyr::case_when(
    is.na(headers)                          ~ paste0("col_", seq_along(headers)),
    is.na(units) | units %in% c("NA", "")  ~ headers,
    TRUE                                    ~ paste(headers, units, sep = "_")
  )
  clean_cols[[1L]] <- "date_raw"

  # ── Slice data rows and assign names ──────────────────────────────────────
  wb_data_raw           <- wb_matrix[data_start:nrow(wb_matrix), ]
  colnames(wb_data_raw) <- clean_cols

  wb_clean <- wb_data_raw |>
    janitor::clean_names() |>
    janitor::remove_empty(c("rows", "cols"))

  log_info("INGEST:WB", glue(
    "Ingested: {nrow(wb_clean)} rows × {ncol(wb_clean)} cols."
  ))

  wb_clean
}
