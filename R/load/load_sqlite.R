# load/load_sqlite.R
# ─────────────────────────────────────────────────────────────────────────────
# Load phase — write net-new rows to the target database.
#
# Strategy:
#   DBI abstraction layer — the same load logic runs against SQLite or
#   PostgreSQL. Swap LOAD_TARGET in .env, no code changes.
#
#   SQLite  : default, zero server setup, reviewers can run immediately
#   PostgreSQL: set LOAD_TARGET=postgres and PG_CONNECTION_STRING in .env
#
#   Write mode: append only — delta_check() guarantees no duplicates reach here.
#   Transaction-wrapped: all rows commit atomically or none do.
#
# Returns: list(loaded = int, table = chr)
# ─────────────────────────────────────────────────────────────────────────────

suppressPackageStartupMessages({
  library(DBI)
  library(RSQLite)
  library(glue)
})

TABLE_NAME <- "commodity_prices"


#' Open a DBI connection to the configured load target.
#'
#' @param config  List. PIPELINE_CONFIG from config.R.
#'
#' @return DBI connection object.
open_connection <- function(config) {
  if (config$LOAD_TARGET == "sqlite") {
    log_info("LOAD", glue("Connecting to SQLite: {config$SQLITE_PATH}"))
    DBI::dbConnect(RSQLite::SQLite(), dbname = config$SQLITE_PATH)

  } else if (config$LOAD_TARGET == "postgres") {
    # RPostgres must be installed: install.packages("RPostgres")
    if (!requireNamespace("RPostgres", quietly = TRUE)) {
      log_abort("LOAD", "RPostgres is not installed. Run: install.packages('RPostgres')")
    }
    log_info("LOAD", "Connecting to PostgreSQL...")
    DBI::dbConnect(
      RPostgres::Postgres(),
      dbname   = sub(".*/(\\w+)$", "\\1", config$PG_CONNECTION),
      host     = sub(".*@([^:/]+).*", "\\1", config$PG_CONNECTION),
      port     = as.integer(sub(".*:(\\d+)/.*", "\\1", config$PG_CONNECTION)),
      user     = sub(".*://([^:]+):.*", "\\1", config$PG_CONNECTION),
      password = sub(".*://[^:]+:([^@]+)@.*", "\\1", config$PG_CONNECTION)
    )

  } else {
    log_abort("LOAD", glue("Unknown LOAD_TARGET: '{config$LOAD_TARGET}'"))
  }
}


#' Write net-new rows to the target database table.
#'
#' Creates the table on first run. Appends on subsequent runs.
#' Wrapped in a transaction — atomically committed or fully rolled back.
#'
#' @param net_new_df  data.frame. Output of delta_check() — must be non-empty.
#' @param config      List. PIPELINE_CONFIG from config.R.
#'
#' @return Named list: loaded (int rows written), table (chr table name).
load_to_db <- function(net_new_df, config) {
  if (nrow(net_new_df) == 0L) {
    log_info("LOAD", "No net-new rows to write. Load skipped.")
    return(invisible(list(loaded = 0L, table = TABLE_NAME)))
  }

  log_info("LOAD", glue("Writing {nrow(net_new_df)} row(s) to '{TABLE_NAME}'..."))

  con <- open_connection(config)
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  tryCatch({
    DBI::dbBegin(con)

    DBI::dbWriteTable(
      con,
      name      = TABLE_NAME,
      value     = net_new_df,
      append    = TRUE,
      row.names = FALSE
    )

    DBI::dbCommit(con)

    log_info("LOAD", glue(
      "Committed {nrow(net_new_df)} row(s) to '{TABLE_NAME}'."
    ))

  }, error = function(e) {
    DBI::dbRollback(con)
    log_abort("LOAD", glue(
      "Transaction rolled back. Error: {conditionMessage(e)}"
    ))
  })

  # ── Post-write verification ───────────────────────────────────────────────
  con2 <- open_connection(config)
  on.exit(DBI::dbDisconnect(con2), add = TRUE)

  total_rows <- DBI::dbGetQuery(
    con2,
    glue::glue_sql("SELECT COUNT(*) AS n FROM {`TABLE_NAME`}", .con = con2)
  )$n

  log_info("LOAD", glue("Table '{TABLE_NAME}' now contains {total_rows} total row(s)."))

  invisible(list(loaded = nrow(net_new_df), table = TABLE_NAME))
}
