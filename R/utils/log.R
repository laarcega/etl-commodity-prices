# utils/log.R
# ─────────────────────────────────────────────────────────────────────────────
# Structured log helpers.
# All pipeline output follows the format: [ LABEL ] message
# Use these functions exclusively — never bare message() calls in pipeline code.
# ─────────────────────────────────────────────────────────────────────────────

#' Emit a structured info log line.
#'
#' @param label  Character. Phase or module label, e.g. "EXTRACT", "TRANSFORM".
#' @param msg    Character. Message body. Supports glue interpolation if
#'               pre-interpolated by the caller.
log_info <- function(label, msg) {
  message(glue::glue("[ {label} ] {msg}"))
}


#' Emit a structured warning log line.
#'
#' @inheritParams log_info
log_warn <- function(label, msg) {
  message(glue::glue("[ {label} ] WARNING — {msg}"))
}


#' Emit a structured separator line (60 "=" characters).
#'
#' @param label  Character. Label printed alongside the separator.
#' @param msg    Character. Optional message after separator.
log_sep <- function(label = "PIPELINE", msg = "") {
  message(paste(rep("=", 60), collapse = ""))
  if (nchar(trimws(msg)) > 0L) {
    message(glue::glue("  {label}: {msg}"))
  }
}


#' Abort the pipeline with a structured fatal error message.
#'
#' Wraps stop() so all fatal exits produce consistent formatting.
#'
#' @inheritParams log_info
log_abort <- function(label, msg) {
  stop(glue::glue("[ {label} ] ABORT — {msg}"), call. = FALSE)
}
