# Tests for URL building functions

test_that("build_gaez_url constructs valid URL with default parameters", {
  url <- build_gaez_url()

  expect_type(url, "character")
  expect_true(nchar(url) > 0)
  expect_true(grepl("googleapis", url))  # Should contain googleapis.com
  expect_true(grepl("RES05-YX", url))   # Default variable
  expect_true(grepl("FP4160", url))     # Default time period
})

test_that("build_gaez_url includes custom variable", {
  url1 <- build_gaez_url(variable = "RES05-YX")
  url2 <- build_gaez_url(variable = "RES02-YLD")

  expect_true(grepl("RES05-YX", url1))
  expect_true(grepl("RES02-YLD", url2))
  expect_false(url1 == url2)
})

test_that("build_gaez_url handles different crops", {
  options(gaez_testing_mode = TRUE)
  url_mz <- build_gaez_url(crop = "MZE", climate_model = "AGERA5", ssp = "HIST",
                           time_period = "HP0120")
  url_wh <- build_gaez_url(crop = "MZE", climate_model = "AGERA5", ssp = "HIST",
                           time_period = "HP0120")

  expect_true(grepl("MZE", url_mz))
  expect_false(url_mz == "")
  options(gaez_testing_mode = NULL)
})

test_that("build_gaez_url handles different time periods", {
  url_hist <- build_gaez_url(time_period = "HP0120", climate_model = "AGERA5", ssp = "HIST")
  url_fut <- build_gaez_url(time_period = "FP4160", climate_model = "ENSEMBLE", ssp = "SSP370")

  expect_true(grepl("HP0120", url_hist))
  expect_true(grepl("FP4160", url_fut))
  expect_false(url_hist == url_fut)
})

test_that("build_gaez_url requires valid climate_model for historical periods", {
  expect_error(
    build_gaez_url(time_period = "HP0120", climate_model = "ENSEMBLE", ssp = "HIST"),
    "only work with AGERA5"
  )
})

test_that("build_gaez_url corrects SSP for historical periods", {
  # Historical periods auto-correct SSP to HIST
  url <- build_gaez_url(time_period = "HP0120", climate_model = "AGERA5", ssp = "SSP370")
  expect_true(grepl("HIST", url))
})

test_that("build_gaez_url requires valid combination for future periods", {
  expect_error(
    build_gaez_url(time_period = "FP4160", climate_model = "AGERA5", ssp = "SSP370"),
    "only works with historical"
  )
})

test_that("build_gaez_url rejects invalid time period", {
  expect_error(
    build_gaez_url(time_period = "INVALID"),
    "must start with HP|FP"
  )
})

test_that("build_gaez_url handles different water management levels", {
  url_hrlm <- build_gaez_url(water_management_level = "HRLM")
  url_hilm <- build_gaez_url(water_management_level = "HILM")

  expect_true(grepl("HRLM", url_hrlm))
  expect_true(grepl("HILM", url_hilm))
})

test_that("build_gaez_url with year lookup for historical", {
  url <- build_gaez_url(start_year = 2010, end_year = 2020,
                        climate_model = "AGERA5", ssp = "HIST",
                        interactive = FALSE)

  expect_true(grepl("HP0120", url))  # 2010-2020 should map to HP0120 (2001-2020)
})

test_that("build_gaez_url with year lookup for future", {
  url <- build_gaez_url(start_year = 2050, end_year = 2060,
                        climate_model = "ENSEMBLE", ssp = "SSP370",
                        interactive = FALSE)

  expect_true(grepl("FP4160", url))  # 2050-2060 should map to FP4160 (2041-2060)
})
