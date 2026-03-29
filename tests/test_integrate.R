# tests/test_integrate.R
# ─────────────────────────────────────────────────────────────────────────────
# testthat suite — integrate_data()
#
# Run: Rscript -e "testthat::test_file('tests/test_integrate.R')"
#      make test
# ─────────────────────────────────────────────────────────────────────────────

library(testthat)
library(dplyr)
library(glue)

source(here::here("R", "utils", "log.R"))
source(here::here("R", "integrate", "integrate.R"))


# ── Fixtures ──────────────────────────────────────────────────────────────────

wb_columns <- c("phosphate_rock", "dap", "tsp", "urea", "potassium_chloride")

make_clean_wb <- function() {
  data.frame(
    year             = c(2023L, 2023L, 2023L),
    month            = c(1L,    2L,    3L),
    phosphate_rock   = c(200.5, NA,    210.0),
    dap              = c(550.0, 560.0, NA),
    tsp              = c(430.0, 435.0, 440.0),
    urea             = c(310.0, 315.0, 320.0),
    potassium_chloride = c(280.0, 285.0, 290.0)
  )
}

make_clean_fx <- function() {
  data.frame(
    year              = c(2023L, 2023L, 2023L),
    month             = c(1L,    2L,    3L),
    exchange_rate_avg = c(55.5,  56.0,  56.5)
  )
}


# ── Tests ─────────────────────────────────────────────────────────────────────

test_that("integrate_data returns expected column count", {
  result <- integrate_data(make_clean_wb(), make_clean_fx(), wb_columns)
  # year + month + 5 USD cols + 5 local cols + exchange_rate_avg = 13
  expect_equal(ncol(result), 13L)
})

test_that("integrate_data produces correct local currency columns", {
  result <- integrate_data(make_clean_wb(), make_clean_fx(), wb_columns, "PHP")
  local_cols <- paste0(wb_columns, "_php")
  expect_true(all(local_cols %in% names(result)))
})

test_that("integrate_data computes peso conversion correctly", {
  result <- integrate_data(make_clean_wb(), make_clean_fx(), wb_columns, "PHP")
  # Row 1: phosphate_rock 200.5 × exchange_rate_avg 55.5 = 11127.75
  expect_equal(result$phosphate_rock_php[1L], 200.5 * 55.5, tolerance = 1e-6)
})

test_that("integrate_data preserves NA in commodity — does not drop row", {
  result <- integrate_data(make_clean_wb(), make_clean_fx(), wb_columns)
  expect_equal(nrow(result), 3L)
  expect_true(is.na(result$phosphate_rock[2L]))
})

test_that("integrate_data propagates NA to local column when USD value is NA", {
  result <- integrate_data(make_clean_wb(), make_clean_fx(), wb_columns, "PHP")
  expect_true(is.na(result$phosphate_rock_php[2L]))
})

test_that("integrate_data preserves all WB rows when FX has full coverage", {
  result <- integrate_data(make_clean_wb(), make_clean_fx(), wb_columns)
  expect_equal(nrow(result), nrow(make_clean_wb()))
})

test_that("integrate_data left-joins — WB rows kept when FX coverage is partial", {
  fx_partial <- make_clean_fx() |> dplyr::filter(month != 3L)  # drop March FX
  result <- integrate_data(make_clean_wb(), fx_partial, wb_columns, "PHP")
  expect_equal(nrow(result), 3L)                   # all 3 WB rows preserved
  expect_true(is.na(result$exchange_rate_avg[3L])) # March rate is NA
  expect_true(is.na(result$phosphate_rock_php[3L]))# March local col is NA
})

test_that("integrate_data output is sorted by year then month", {
  wb <- make_clean_wb() |> dplyr::arrange(dplyr::desc(month))  # reverse order
  result <- integrate_data(wb, make_clean_fx(), wb_columns)
  expect_equal(result$month, c(1L, 2L, 3L))
})
