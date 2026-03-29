# extract/extract_wb_commodity.R
# ─────────────────────────────────────────────────────────────────────────────
# Extract phase — World Bank Commodity Price XLSX.
#
# Strategy:
#   Automated link discovery — no hardcoded filename, version, or CSS selector.
#   Two-pass regex over all hrefs on the WB commodity markets page:
#     Pass 1 — hrefs ending in .xlsx containing both CMO/commodity AND monthly
#     Pass 2 — fallback: any xlsx href containing "historical"
#   Resolves relative URLs to absolute before download.
#   This approach survives World Bank file renaming or page restructuring
#   without any code or .env changes.
#
# Returns: character path to downloaded XLSX in raw_data/
# ─────────────────────────────────────────────────────────────────────────────

suppressPackageStartupMessages({
  library(rvest)
  library(httr2)
  library(stringr)
  library(purrr)
  library(glue)
})


#' Download the World Bank CMO commodity price XLSX via automated link discovery.
#'
#' @param config  List. PIPELINE_CONFIG from config.R.
#'
#' @return Character. Absolute path to the downloaded XLSX file.
extract_wb_commodity <- function(config) {
  log_info("EXTRACT:WB", "Starting World Bank commodity extraction...")

  dest <- file.path(config$DIR_RAW_DATA, "wb_commodity.xlsx")

  # ── Scrape DOM for all xlsx hrefs ─────────────────────────────────────────
  log_info("EXTRACT:WB", glue("Scraping DOM: {config$URL_WB_DOM}"))

  wb_page <- tryCatch(
    rvest::read_html(config$URL_WB_DOM),
    error = function(e) log_abort("EXTRACT:WB", glue(
      "Could not read World Bank DOM: {conditionMessage(e)}"
    ))
  )

  all_xlsx_hrefs <- wb_page |>
    rvest::html_nodes("a") |>
    rvest::html_attr("href") |>
    purrr::keep(~ !is.na(.x) &&
                  stringr::str_detect(.x, stringr::regex("\\.xlsx$", ignore_case = TRUE)))

  log_info("EXTRACT:WB", glue("{length(all_xlsx_hrefs)} xlsx href(s) found on page."))

  if (length(all_xlsx_hrefs) == 0L) {
    log_abort("EXTRACT:WB", glue(
      "No xlsx links found on World Bank page. ",
      "Page DOM may have changed. Inspect: {config$URL_WB_DOM}"
    ))
  }

  # ── Pass 1: CMO/commodity + monthly ───────────────────────────────────────
  primary_pattern <- stringr::regex(
    "(?=.*(?:CMO|commodity))(?=.*monthly)",
    ignore_case = TRUE
  )
  candidates <- purrr::keep(all_xlsx_hrefs, ~ stringr::str_detect(.x, primary_pattern))

  # ── Pass 2 fallback: historical ───────────────────────────────────────────
  if (length(candidates) == 0L) {
    log_warn("EXTRACT:WB", "Primary pattern matched 0 results — trying 'historical' fallback.")
    candidates <- purrr::keep(
      all_xlsx_hrefs,
      ~ stringr::str_detect(.x, stringr::regex("historical", ignore_case = TRUE))
    )
  }

  if (length(candidates) == 0L) {
    log_abort("EXTRACT:WB", paste0(
      "Could not identify the World Bank commodity file.\n",
      "All xlsx hrefs found:\n",
      paste0("  ", all_xlsx_hrefs, collapse = "\n"), "\n",
      "Update URL_WB_DOM in .env or inspect the page manually."
    ))
  }

  wb_link <- candidates[[1L]]

  if (length(candidates) > 1L) {
    log_info("EXTRACT:WB", glue(
      "{length(candidates)} candidate(s) found. Using: {wb_link}"
    ))
    log_info("EXTRACT:WB", glue(
      "Other candidates (not used): {paste(candidates[-1], collapse = ', ')}"
    ))
  } else {
    log_info("EXTRACT:WB", glue("Link identified: {wb_link}"))
  }

  # ── Resolve relative URLs ─────────────────────────────────────────────────
  if (!stringr::str_starts(wb_link, "http")) {
    base_url <- stringr::str_extract(config$URL_WB_DOM, "^https?://[^/]+")
    wb_link  <- paste0(base_url, wb_link)
    log_info("EXTRACT:WB", glue("Resolved to absolute URL: {wb_link}"))
  }

  # ── Download ──────────────────────────────────────────────────────────────
  log_info("EXTRACT:WB", "Downloading World Bank commodity XLSX...")

  success <- tryCatch({
    download.file(
      url      = wb_link,
      destfile = dest,
      mode     = "wb",
      method   = "libcurl",
      quiet    = TRUE
    )
    TRUE
  }, error = function(e) {
    log_warn("EXTRACT:WB", glue("Download failed: {conditionMessage(e)}"))
    FALSE
  })

  if (!success) log_abort("EXTRACT:WB", "World Bank XLSX download failed.")

  assert_file(dest, "World Bank commodity XLSX")
  report_file_size(dest, "World Bank commodity XLSX")
  log_info("EXTRACT:WB", "World Bank commodity extraction complete.")

  dest
}
