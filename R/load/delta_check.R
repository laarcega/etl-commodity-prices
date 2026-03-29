# load/delta_check.R
# ─────────────────────────────────────────────────────────────────────────────
# Load phase — delta check.
#
# Strategy:
#   Anti-join the integrated dataset against existing records in the load
#   target on the composite key (year, month).
#   Only net-new rows proceed to the load phase.
#   This makes the pipeline idempotent: re-running it never produces
#   duplicate records regardless of how many times it executes.
#
#   Existing records are read via the same connection used for loading —
#   no second connection, no separate probe.
#
# Returns:
#   data.frame of net-new rows — empty data.frame signals nothing to load.
# ─────────────────────────────────────────────────────────────────────────────

suppressPackageStartupMessages({
  library(dplyr)
  library(DBI)
  library(glue)
})

TABLE_NAME <- "commodity_prices"


#' Compute the net-new rows not yet present in the load target.
#'
#' @param integrated_df  data.frame. Full output of integrate_data().
#' @param con            DBI connection. Active connection to the load target.
#'
#' @return data.frame. Rows in integrated_df not present in the target table.
#'   Returns integrated_df unchanged if the target table does not yet exist
#'   (first run / fresh deployment).
delta_check <- function(integrated_df, con) {
  log_info("DELTA", "Starting delta check...")

  # ── Check if target table exists ──────────────────────────────────────────
  table_exists <- DBI::dbExistsTable(con, TABLE_NAME)

  if (!table_exists) {
    log_info("DELTA", glue(
      "Table '{TABLE_NAME}' does not exist — first run. ",
      "All {nrow(integrated_df)} rows are net-new."
    ))
    return(integrated_df)
  }

  # ── Fetch existing composite keys ─────────────────────────────────────────
  existing_keys <- DBI::dbGetQuery(
    con,
    glue::glue_sql(
      "SELECT year, month FROM {`TABLE_NAME`}",
      .con = con
    )
  ) |>
    dplyr::mutate(
      year  = as.integer(year),
      month = as.integer(month)
    )

  log_info("DELTA", glue(
    "{nrow(existing_keys)} existing record(s) in '{TABLE_NAME}'."
  ))

  # ── Anti-join on composite key ────────────────────────────────────────────
  net_new <- integrated_df |>
    dplyr::mutate(
      year  = as.integer(year),
      month = as.integer(month)
    ) |>
    dplyr::anti_join(existing_keys, by = c("year", "month"))

  if (nrow(net_new) == 0L) {
    log_info("DELTA", "All records already present. Nothing to load.")
    return(net_new)
  }

  month_labels <- net_new |>
    dplyr::arrange(year, month) |>
    dplyr::mutate(label = glue(
      "{year}-{formatC(month, width = 2L, flag = '0')}"
    )) |>
    dplyr::pull(label)

  log_info("DELTA", glue(
    "{nrow(net_new)} net-new row(s): {paste(month_labels, collapse = ', ')}"
  ))

  # ── Schema validation gate — loud fail before touching the database ────────
  if (!is.integer(net_new$year) || !is.integer(net_new$month)) {
    log_abort("DELTA", "Temporal keys (year, month) must be integer — validation failed.")
  }

  log_info("DELTA", "Delta check complete.")
  net_new
}
