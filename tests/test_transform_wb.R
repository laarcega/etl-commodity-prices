# tests/test_transform_wb.R
# ─────────────────────────────────────────────────────────────────────────────
# testthat suite — transform_wb()
#
# Run: Rscript -e "testthat::test_file('tests/test_transform_wb.R')"
#      make test
# ─────────────────────────────────────────────────────────────────────────────

library(testthat)
library(dplyr)
library(glue)

# Source dependencies — minimal bootstrap, no .env required for tests
source(here::here("R", "utils", "log.R"))
source(here::here("R", "transform", "transform_wb.R"))


# ── Fixtures ──────────────────────────────────────────────────────────────────

# Minimal raw WB data.frame mimicking ingest_wb() output
make_raw_wb <- function() {
  data.frame(
    date_raw             = c("2023 M01", "2023 M02", "2023 M03"),
    phosphate_rock_usd_mt = c("200.5",   "...",      "210.0"),
    dap_usd_mt           = c("550.0",    "560.0",    "\u2026"),
    tsp_usd_mt           = c("430.0",    "435.0",    "440.0"),
    urea_usd_mt          = c("310.0",    "315.0",    "320.0"),
    potassium_chloride_usd_mt = c("280.0", "285.0",  "290.0"),
    stringsAsFactors     = FALSE
  )
}

wb_columns <- c("phosphate_rock", "dap", "tsp", "urea", "potassium_chloride")


# ── Tests ─────────────────────────────────────────────────────────────────────

test_that("transform_wb returns correct column names", {
  result <- transform_wb(make_raw_wb(), wb_columns)
  expect_true(all(c("year", "month", wb_columns) %in% names(result)))
})

test_that("transform_wb produces correct row count", {
  result <- transform_wb(make_raw_wb(), wb_columns)
  expect_equal(nrow(result), 3L)
})

test_that("transform_wb parses year correctly", {
  result <- transform_wb(make_raw_wb(), wb_columns)
  expect_equal(unique(result$year), 2023L)
})

test_that("transform_wb parses month correctly", {
  result <- transform_wb(make_raw_wb(), wb_columns)
  expect_equal(result$month, c(1L, 2L, 3L))
})

test_that("transform_wb coerces '...' sentinel to NA — does not drop row", {
  result <- transform_wb(make_raw_wb(), wb_columns)
  expect_true(is.na(result$phosphate_rock[2L]))
  expect_equal(nrow(result), 3L)   # row retained
})

test_that("transform_wb coerces unicode ellipsis sentinel to NA — does not drop row", {
  result <- transform_wb(make_raw_wb(), wb_columns)
  expect_true(is.na(result$dap[3L]))
  expect_equal(nrow(result), 3L)
})

test_that("transform_wb commodity columns are numeric", {
  result <- transform_wb(make_raw_wb(), wb_columns)
  for (col in wb_columns) {
    expect_true(is.numeric(result[[col]]), info = glue("Column '{col}' should be numeric"))
  }
})

test_that("transform_wb aborts on missing commodity key", {
  expect_error(
    transform_wb(make_raw_wb(), c(wb_columns, "nonexistent_commodity")),
    regexp = "ABORT"
  )
})
