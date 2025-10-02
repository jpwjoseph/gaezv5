# Tests for validation functions

test_that("validate_climate_ssp works for historical periods", {
  result <- validate_climate_ssp("HP0120", NULL, "SSP370")

  expect_type(result, "list")
  expect_equal(result$climate_model, "AGERA5")
  expect_equal(result$ssp, "HIST")
})

test_that("validate_climate_ssp works for future periods", {
  result <- validate_climate_ssp("FP4160", NULL, "SSP370")

  expect_type(result, "list")
  expect_equal(result$climate_model, "ENSEMBLE")
  expect_equal(result$ssp, "SSP370")
})

test_that("validate_climate_ssp rejects AGERA5 for future periods", {
  expect_error(
    validate_climate_ssp("FP4160", "AGERA5", "SSP370"),
    "only works with historical"
  )
})

test_that("validate_climate_ssp rejects non-AGERA5 for historical", {
  expect_error(
    validate_climate_ssp("HP0120", "ENSEMBLE", "HIST"),
    "only work with AGERA5"
  )
})

test_that("validate_climate_ssp requires valid time period format", {
  expect_error(
    validate_climate_ssp("INVALID", NULL, "SSP370"),
    "must start with"
  )
})

test_that("validate_climate_ssp validates SSPs for future periods", {
  expect_error(
    validate_climate_ssp("FP4160", "ENSEMBLE", "INVALID_SSP"),
    "require SSP scenarios"
  )
})
