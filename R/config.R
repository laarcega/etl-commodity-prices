# config.R
# ─────────────────────────────────────────────────────────────────────────────
# Central pipeline configuration.
# All values sourced from .env — zero hardcoded constants anywhere in the pipeline.
# Source this file first. Every subsequent module reads from PIPELINE_CONFIG.
# ─────────────────────────────────────────────────────────────────────────────

suppressPackageStartupMessages({
  library(dotenv)
  library(here)
  library(glue)
})

dotenv::load_dot_env(here::here(".env"))


# ── Validate required .env keys ───────────────────────────────────────────────
required_keys <- c(
  "URL_WB_DOM",
  "URL_BSP",
  "WB_COLUMNS",
  "TARGET_CURRENCY",
  "LOAD_TARGET",
  "SQLITE_PATH",
  "MAX_RETRIES",
  "TIMEOUT_SEC"
)

missing_keys <- Filter(
  function(k) nchar(trimws(Sys.getenv(k))) == 0L,
  required_keys
)

if (length(missing_keys) > 0L) {
  stop(
    "[ CONFIG ] ABORT — Missing or empty .env key(s): ",
    paste(missing_keys, collapse = ", "), "\n",
    "           Copy .env.example → .env and fill in all values."
  )
}


# ── Parse WB_COLUMNS ──────────────────────────────────────────────────────────
wb_columns_raw <- Sys.getenv("WB_COLUMNS")
wb_columns     <- trimws(unlist(strsplit(wb_columns_raw, ",")))

if (length(wb_columns) == 0L || any(nchar(wb_columns) == 0L)) {
  stop(
    "[ CONFIG ] ABORT — WB_COLUMNS is malformed in .env.\n",
    "           Expected: WB_COLUMNS=phosphate_rock,dap,tsp,urea,potassium_chloride"
  )
}


# ── Validate LOAD_TARGET ──────────────────────────────────────────────────────
load_target <- tolower(trimws(Sys.getenv("LOAD_TARGET")))

if (!load_target %in% c("sqlite", "postgres")) {
  stop(
    "[ CONFIG ] ABORT — LOAD_TARGET must be 'sqlite' or 'postgres'.\n",
    "           Got: '", load_target, "'"
  )
}

if (load_target == "postgres") {
  pg_conn <- trimws(Sys.getenv("PG_CONNECTION_STRING"))
  if (nchar(pg_conn) == 0L) {
    stop(
      "[ CONFIG ] ABORT — LOAD_TARGET=postgres but PG_CONNECTION_STRING is empty.\n",
      "           Set PG_CONNECTION_STRING in .env."
    )
  }
}


# ── Build PIPELINE_CONFIG ─────────────────────────────────────────────────────
PIPELINE_CONFIG <- list(

  # Source URLs
  URL_WB_DOM   = Sys.getenv("URL_WB_DOM"),
  URL_BSP      = Sys.getenv("URL_BSP"),

  # Commodity columns — driven entirely by .env
  WB_COLUMNS   = wb_columns,

  # Currency conversion target
  TARGET_CURRENCY = toupper(trimws(Sys.getenv("TARGET_CURRENCY"))),

  # Load target
  LOAD_TARGET  = load_target,
  SQLITE_PATH  = here::here(Sys.getenv("SQLITE_PATH")),
  PG_CONNECTION = if (load_target == "postgres") Sys.getenv("PG_CONNECTION_STRING") else NULL,

  # Staging directories — cross-platform via here::here()
  DIR_RAW_DATA = here::here("raw_data"),
  DIR_OUTPUTS  = here::here("outputs"),

  # Safety constraints
  MAX_RETRIES  = as.integer(Sys.getenv("MAX_RETRIES")),
  TIMEOUT_SEC  = as.integer(Sys.getenv("TIMEOUT_SEC"))
)

message(glue(
  "[ CONFIG ] PIPELINE_CONFIG built — {length(PIPELINE_CONFIG)} keys | ",
  "commodities: {paste(PIPELINE_CONFIG$WB_COLUMNS, collapse = ', ')} | ",
  "load target: {PIPELINE_CONFIG$LOAD_TARGET}"
))
