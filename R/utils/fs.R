# utils/fs.R
# ─────────────────────────────────────────────────────────────────────────────
# Filesystem utilities shared across pipeline phases.
#   - ensure_dir()        : create directory if absent, log result
#   - assert_file()       : abort if a file is missing or zero-byte
#   - report_file_size()  : log file size in KB after download
# ─────────────────────────────────────────────────────────────────────────────


#' Create a directory if it does not exist; log the outcome either way.
#'
#' @param path  Character. Directory path to ensure exists.
ensure_dir <- function(path) {
  if (!dir.exists(path)) {
    dir.create(path, recursive = TRUE)
    log_info("FS", glue::glue("Created directory: {path}"))
  } else {
    log_info("FS", glue::glue("Directory ready: {path}"))
  }
  invisible(path)
}


#' Abort if a file does not exist or is zero bytes.
#'
#' @param path   Character. File path to validate.
#' @param label  Character. Human-readable label used in log/abort messages.
assert_file <- function(path, label) {
  if (!file.exists(path) || file.info(path)$size == 0L) {
    log_abort("FS", glue::glue("{label} file missing or empty: {path}"))
  }
  invisible(path)
}


#' Log the size of a downloaded file in KB.
#'
#' @param path   Character. File path.
#' @param label  Character. Human-readable label for the log line.
report_file_size <- function(path, label) {
  size_kb <- round(file.info(path)$size / 1024, 1L)
  log_info("FS", glue::glue("{label}: {size_kb} KB"))
  invisible(size_kb)
}
