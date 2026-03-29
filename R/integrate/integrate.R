# integrate/integrate.R
# ─────────────────────────────────────────────────────────────────────────────
# Integration phase — join World Bank commodity data with BSP exchange rates
# and compute PHP equivalents for all commodities.
#
# Strategy:
#   Left join on composite key (year, month) — WB facts are preserved even
#   when BSP exchange rate history does not cover the full date range.
#
#   Type alignment before join:
#     WB  produces month as integer (1–12)
#     BSP produces month as character ("January", "February", ...)
#     BSP month names are converted to integer via match(month, month.name)
#     before the join — consistent types, clean join, no silent mismatches.
#
#   PHP conversion columns are generated programmatically from WB_COLUMNS —
#   adding a new commodity in .env produces its _php column automatically,
#   zero script changes required.
#
# Returns: data.frame — columns: year, month, [WB_COLUMNS], [WB_COLUMNS]_php,
#          exchange_rate_avg
# ─────────────────────────────────────────────────────────────────────────────

suppressPackageStartupMessages({
  library(dplyr)
  library(glue)
})


#' Join WB commodity data with BSP exchange rates and compute PHP equivalents.
#'
#' @param clean_wb_df   data.frame. Output of transform_wb() —
#'   columns: year (int), month (int), [WB_COLUMNS] (dbl).
#' @param clean_bsp_df  data.frame. Output of transform_bsp() —
#'   columns: year (int), month (chr month name), exchange_rate_avg (dbl).
#' @param wb_columns    Character vector. Commodity column keys from PIPELINE_CONFIG.
#' @param target_ccy    Character. Target currency code (e.g. "PHP").
#'
#' @return data.frame with commodity USD prices, PHP equivalents,
#'   and the BSP exchange rate used for conversion.
integrate_data <- function(clean_wb_df, clean_bsp_df, wb_columns, target_ccy = "PHP") {
  log_info("INTEGRATE", glue(
    "Joining WB ({nrow(clean_wb_df)} rows) x BSP ({nrow(clean_bsp_df)} rows) ",
    "on year + month..."
  ))

  # ── Type alignment: BSP month name -> integer ─────────────────────────────
  # WB month is integer (1-12). BSP month is character ("January"...).
  # Convert BSP month names to integer using base R month.name vector.
  # match("January", month.name) = 1, match("December", month.name) = 12.
  # Rows with unrecognised month names produce NA and are dropped.
  bsp_keyed <- clean_bsp_df |>
    dplyr::mutate(month = match(month, month.name)) |>
    dplyr::filter(!is.na(month))

  unmatched <- nrow(clean_bsp_df) - nrow(bsp_keyed)
  if (unmatched > 0L) {
    log_warn("INTEGRATE", glue(
      "{unmatched} BSP row(s) dropped — month name did not match month.name vector."
    ))
  }

  # ── Left join on year + month ─────────────────────────────────────────────
  merged_df <- clean_wb_df |>
    dplyr::left_join(bsp_keyed, by = c("year", "month"))

  log_info("INTEGRATE", glue(
    "Joined: {nrow(merged_df)} rows x {ncol(merged_df)} cols."
  ))

  # Warn on missing exchange rates — rows exist but no BSP rate for that period
  missing_rate <- sum(is.na(merged_df$exchange_rate_avg))
  if (missing_rate > 0L) {
    log_warn("INTEGRATE", glue(
      "{missing_rate} row(s) have no BSP exchange rate — ",
      "PHP columns will be NA for those rows."
    ))
  }

  # ── Programmatic PHP conversion ───────────────────────────────────────────
  # Generates {commodity}_php for every commodity in WB_COLUMNS.
  # Driven entirely by wb_columns — adding a commodity to .env
  # automatically produces its PHP equivalent column here.
  local_col_suffix <- tolower(target_ccy)

  final_df <- merged_df |>
    dplyr::mutate(dplyr::across(
      .cols  = dplyr::all_of(wb_columns),
      .fns   = ~ .x * exchange_rate_avg,
      .names = glue("{{.col}}_{local_col_suffix}")
    )) |>
    dplyr::select(
      year,
      month,
      dplyr::all_of(wb_columns),
      dplyr::ends_with(glue("_{local_col_suffix}")),
      exchange_rate_avg
    ) |>
    dplyr::arrange(year, month)

  log_info("INTEGRATE", glue(
    "Integration complete: {nrow(final_df)} rows x {ncol(final_df)} cols."
  ))
  log_info("INTEGRATE", glue(
    "Columns: {paste(names(final_df), collapse = ', ')}"
  ))

  final_df
}