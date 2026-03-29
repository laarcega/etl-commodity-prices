# extract/extract_bsp.R
# ─────────────────────────────────────────────────────────────────────────────
# Extract phase — BSP Philippine Peso per US Dollar Rate (monthly).
#
# Source : Bangko Sentral ng Pilipinas (BSP) — central bank of the Philippines
# URL    : https://www.bsp.gov.ph/statistics/external/pesodollar.xlsx
# Access : Public, no credentials required
#
# Strategy:
#   Direct download from stable BSP URL stored in .env as URL_BSP.
#   No scraping, no link discovery — the URL is institutionally stable.
#   File is downloaded to raw_data/bsp_pesodollar.xlsx for staging.
#   Ingest and transformation are handled in subsequent phases.
#
# Why BSP over World Bank API:
#   BSP is the primary source — the authoritative institution that sets
#   and publishes PHP/USD rates for the Philippines. The World Bank
#   PA.NUS.FCRF indicator is itself derived from BSP data. Going to
#   the primary source is the stronger engineering decision.
#   Monthly granularity matches the World Bank commodity price data exactly.
#
# Returns: character — absolute path to downloaded XLSX in raw_data/
# ─────────────────────────────────────────────────────────────────────────────

suppressPackageStartupMessages(library(glue))


#' Download the BSP peso/dollar XLSX from the public BSP statistics page.
#'
#' @param config  List. PIPELINE_CONFIG from config.R.
#'
#' @return Character. Absolute path to the downloaded XLSX file.
extract_bsp <- function(config) {
  log_info("EXTRACT:BSP", "Starting BSP exchange rate extraction...")
  log_info("EXTRACT:BSP", glue("Source: {config$URL_BSP}"))

  dest <- file.path(config$DIR_RAW_DATA, "bsp_pesodollar.xlsx")

  success <- tryCatch({
    download.file(
      url      = config$URL_BSP,
      destfile = dest,
      mode     = "wb",
      method   = "libcurl",
      quiet    = TRUE
    )
    TRUE
  }, error = function(e) {
    log_warn("EXTRACT:BSP", glue("Download failed: {conditionMessage(e)}"))
    FALSE
  })

  if (!success) {
    log_abort("EXTRACT:BSP", paste0(
      "BSP XLSX download failed.\n",
      "  URL: ", config$URL_BSP, "\n",
      "  Check network connectivity and URL_BSP in .env."
    ))
  }

  assert_file(dest, "BSP exchange rate XLSX")
  report_file_size(dest, "BSP exchange rate XLSX")
  log_info("EXTRACT:BSP", "BSP extraction complete.")

  dest
}