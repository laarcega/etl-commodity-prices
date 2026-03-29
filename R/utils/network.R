# utils/network.R
# ─────────────────────────────────────────────────────────────────────────────
# Network utilities shared across extract phases.
#   - preflight_network()  : abort early if no internet connection
#   - polite_delay()       : randomised throttle between outbound requests
#   - with_retry()         : uniform retry wrapper for any expression
# ─────────────────────────────────────────────────────────────────────────────

suppressPackageStartupMessages(library(curl))


#' Assert internet connectivity before the pipeline attempts any outbound call.
#'
#' Calls log_abort() (hard stop) if no connection is detected.
#' Call once in run_pipeline.R after config is loaded.
preflight_network <- function() {
  if (!curl::has_internet()) {
    log_abort("NETWORK", "No internet connection detected. Pipeline aborted.")
  }
  log_info("NETWORK", "Internet connectivity confirmed.")
}


#' Sleep a random interval to throttle outbound requests politely.
#'
#' @param min_sec  Numeric. Lower bound of sleep range in seconds. Default 2.
#' @param max_sec  Numeric. Upper bound of sleep range in seconds. Default 4.
polite_delay <- function(min_sec = 2, max_sec = 4) {
  wait <- round(runif(1, min_sec, max_sec), 2L)
  log_info("NETWORK", glue::glue("Throttling — pausing {wait}s..."))
  Sys.sleep(wait)
}


#' Execute an expression with exponential-backoff retry on error.
#'
#' @param expr        Expression to evaluate (wrapped in {}).
#' @param max_tries   Integer. Maximum attempts before re-throwing. Default 3.
#' @param backoff_sec Numeric. Base sleep seconds; doubles each attempt. Default 5.
#' @param label       Character. Log label for retry messages.
#'
#' @return Value of expr on success.
#' @examples
#' result <- with_retry({ httr2::req_perform(req) }, label = "EXTRACT")
with_retry <- function(expr, max_tries = 3L, backoff_sec = 5, label = "NETWORK") {
  attempt <- 1L
  repeat {
    result <- tryCatch(
      expr,
      error = function(e) e
    )
    if (!inherits(result, "error")) return(result)
    if (attempt >= max_tries) stop(result)
    wait <- backoff_sec * (2 ^ (attempt - 1L))
    log_warn(label, glue::glue(
      "Attempt {attempt}/{max_tries} failed: {conditionMessage(result)} ",
      "— retrying in {wait}s"
    ))
    Sys.sleep(wait)
    attempt <- attempt + 1L
  }
}
