# run_pipeline.R
# ─────────────────────────────────────────────────────────────────────────────
# Master controller — ETL Commodity Prices Pipeline.
#
# Data sources:
#   World Bank CMO Historical Data Monthly XLSX  — commodity prices (USD/MT)
#   BSP Philippine Peso per US Dollar XLSX       — monthly PHP/USD rates
#
# Execution sequence:
#   0. Bootstrap  — config, utils, module sources
#   1. Extract    — download WB commodity XLSX + BSP exchange rate XLSX
#   2. Ingest     — parse both XLSX files into raw data.frames
#   3. Transform  — clean, type-cast, sentinel handling on both sources
#   4. Integrate  — join on year+month, compute PHP equivalents
#   5. Delta      — anti-join against existing DB records (idempotency)
#   6. Load       — write net-new rows to SQLite / PostgreSQL
#
# To run:
#   Rscript run_pipeline.R
#   make run
# ─────────────────────────────────────────────────────────────────────────────

pipeline_start <- proc.time()

# ── 0. Bootstrap ──────────────────────────────────────────────────────────────
# Config sourced first — all modules depend on PIPELINE_CONFIG and glue.
source(here::here("R", "config.R"))

# Utils sourced after config — depend on glue loaded by config
source(here::here("R", "utils", "log.R"))
source(here::here("R", "utils", "network.R"))
source(here::here("R", "utils", "fs.R"))

# Pipeline modules
source(here::here("R", "extract",   "extract_wb_commodity.R"))
source(here::here("R", "extract",   "extract_bsp.R"))
source(here::here("R", "ingest",    "ingest_wb.R"))
source(here::here("R", "ingest",    "ingest_bsp.R"))
source(here::here("R", "transform", "transform_wb.R"))
source(here::here("R", "transform", "transform_bsp.R"))
source(here::here("R", "integrate", "integrate.R"))
source(here::here("R", "load",      "delta_check.R"))
source(here::here("R", "load",      "load_sqlite.R"))

log_sep("PIPELINE", "ETL Commodity Prices — Starting")

# Pre-flight
preflight_network()
ensure_dir(PIPELINE_CONFIG$DIR_RAW_DATA)
ensure_dir(PIPELINE_CONFIG$DIR_OUTPUTS)


# ── 1. Extract ────────────────────────────────────────────────────────────────
log_sep("EXTRACT")

wb_file <- tryCatch(
  extract_wb_commodity(PIPELINE_CONFIG),
  error = function(e) log_abort("MASTER", glue::glue("Extract WB failed: {conditionMessage(e)}"))
)

polite_delay()

bsp_file <- tryCatch(
  extract_bsp(PIPELINE_CONFIG),
  error = function(e) log_abort("MASTER", glue::glue("Extract BSP failed: {conditionMessage(e)}"))
)


# ── 2. Ingest ─────────────────────────────────────────────────────────────────
log_sep("INGEST")

raw_wb_df <- tryCatch(
  ingest_wb(wb_file),
  error = function(e) log_abort("MASTER", glue::glue("Ingest WB failed: {conditionMessage(e)}"))
)

raw_bsp_df <- tryCatch(
  ingest_bsp(bsp_file),
  error = function(e) log_abort("MASTER", glue::glue("Ingest BSP failed: {conditionMessage(e)}"))
)


# ── 3. Transform ──────────────────────────────────────────────────────────────
log_sep("TRANSFORM")

clean_wb_df <- tryCatch(
  transform_wb(raw_wb_df, PIPELINE_CONFIG$WB_COLUMNS),
  error = function(e) log_abort("MASTER", glue::glue("Transform WB failed: {conditionMessage(e)}"))
)

clean_bsp_df <- tryCatch(
  transform_bsp(raw_bsp_df),
  error = function(e) log_abort("MASTER", glue::glue("Transform BSP failed: {conditionMessage(e)}"))
)


# ── 4. Integrate ──────────────────────────────────────────────────────────────
log_sep("INTEGRATE")

integrated_df <- tryCatch(
  integrate_data(
    clean_wb_df,
    clean_bsp_df,
    PIPELINE_CONFIG$WB_COLUMNS,
    PIPELINE_CONFIG$TARGET_CURRENCY
  ),
  error = function(e) log_abort("MASTER", glue::glue("Integration failed: {conditionMessage(e)}"))
)


# ── 5. Delta check ────────────────────────────────────────────────────────────
log_sep("DELTA")

con <- open_connection(PIPELINE_CONFIG)

net_new_df <- tryCatch(
  delta_check(integrated_df, con),
  error = function(e) {
    DBI::dbDisconnect(con)
    log_abort("MASTER", glue::glue("Delta check failed: {conditionMessage(e)}"))
  }
)

DBI::dbDisconnect(con)

# Write delta CSV for inspection before any load occurs
delta_path <- file.path(PIPELINE_CONFIG$DIR_OUTPUTS, "delta_output.csv")
utils::write.csv(net_new_df, delta_path, row.names = FALSE)
log_info("MASTER", glue::glue("Delta CSV saved: {delta_path}"))

if (nrow(net_new_df) == 0L) {
  log_sep("PIPELINE", "All records already loaded. Pipeline complete.")
  quit(save = "no", status = 0L)
}


# ── 6. Load ───────────────────────────────────────────────────────────────────
log_sep("LOAD")

load_summary <- tryCatch(
  load_to_db(net_new_df, PIPELINE_CONFIG),
  error = function(e) log_abort("MASTER", glue::glue("Load failed: {conditionMessage(e)}"))
)


# ── Summary ───────────────────────────────────────────────────────────────────
pipeline_elapsed <- round((proc.time() - pipeline_start)[["elapsed"]], 1L)

log_sep("PIPELINE", glue::glue("Complete in {pipeline_elapsed}s"))
log_info("MASTER", glue::glue("WB rows ingested   : {nrow(clean_wb_df)}"))
log_info("MASTER", glue::glue("BSP rows ingested  : {nrow(clean_bsp_df)}"))
log_info("MASTER", glue::glue("Integrated rows    : {nrow(integrated_df)}"))
log_info("MASTER", glue::glue("Net new loaded     : {load_summary$loaded}"))
log_info("MASTER", glue::glue("Commodities        : {paste(PIPELINE_CONFIG$WB_COLUMNS, collapse = ', ')}"))
log_info("MASTER", glue::glue("Load target        : {PIPELINE_CONFIG$LOAD_TARGET}"))
log_sep()