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
  url2 <- build_gaez_url(variable = "RES02-YLD", climate_model = "GFDL-ESM4")

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

root <- "https://storage.googleapis.com/fao-gismgr-gaez-v5-data/DATA/GAEZ-V5"

test_that("theme 1 maps sit directly under MAP/", {
  expect_equal(suppressMessages(build_gaez_url("AEZ57")), paste0(root, "/MAP/GAEZ-V5.AEZ57.tif"))
  expect_equal(suppressMessages(build_gaez_url("SLOPE-MED")), paste0(root, "/MAP/GAEZ-V5.SLOPE-MED.tif"))
})

test_that("per-class theme 1 mapsets require a valid class", {
  expect_error(suppressMessages(build_gaez_url("LR-LCC")), "one raster per class")
  expect_error(suppressMessages(build_gaez_url("LR-LCC", class = "LC99")), "Invalid class")
  expect_equal(
    suppressMessages(build_gaez_url("LR-LCC", class = "lc02")),
    paste0(root, "/MAPSET/LR-LCC/GAEZ-V5.LR-LCC.LC02.tif")
  )
})

test_that("theme 2 needs no crop and no ENSEMBLE for future periods", {
  expect_equal(
    suppressMessages(build_gaez_url("RES01-PRC", time_period = "HP0120")),
    paste0(root, "/MAPSET/RES01-PRC/GAEZ-V5.RES01-PRC.HP0120.AGERA5.HIST.tif")
  )
  expect_error(suppressMessages(build_gaez_url("RES01-PRC", time_period = "FP4160")), "no ENSEMBLE")
  expect_match(
    suppressMessages(build_gaez_url("RES01-PRC", time_period = "FP4160", climate_model = "MRI-ESM2-0")),
    "RES01-PRC.FP4160.MRI-ESM2-0.SSP370.tif$"
  )
})

test_that("theme 3 rejects ENSEMBLE for future periods", {
  expect_error(
    suppressMessages(build_gaez_url("RES02-YLD", crop = "maize", climate_model = "ENSEMBLE")),
    "no ENSEMBLE"
  )
})

test_that("time-series variables take a single year", {
  expect_equal(
    suppressMessages(build_gaez_url("RES01-PRC-TS", start_year = 2010)),
    paste0(root, "/MAPSET/RES01-PRC-TS/GAEZ-V5.RES01-PRC-TS.2010.tif")
  )
  expect_error(suppressMessages(build_gaez_url("RES01-PRC-TS")), "cover 1980-2022")
  expect_error(suppressMessages(build_gaez_url("RES01-PRC-TS", start_year = 2001, end_year = 2005)),
               "annual")
})

test_that("resolution selects the ~1 km layer and rejects future periods", {
  url <- suppressMessages(build_gaez_url("RES05-SX", crop = "maize", time_period = "HP0120",
                                         resolution = "1km"))
  expect_match(url, "/RES05-SXX30AS/GAEZ-V5.RES05-SXX30AS.HP0120.AGERA5.HIST.MZE.HRLM.tif$")
  expect_match(suppressMessages(build_gaez_url("RES05-SX", crop = "maize")), "/RES05-SXX/")
  expect_error(suppressMessages(build_gaez_url("RES05-SX", crop = "maize", resolution = "1km")),
               "HP8100 and HP0120 only")
  expect_error(suppressMessages(build_gaez_url("RES06-YLD", crop = "maize", resolution = "1km")),
               "No 1km layer")
})

test_that("themes 5-6 use crop and water supply only", {
  expect_equal(
    suppressMessages(build_gaez_url("RES06-YLD", crop = "maize", water_supply = "wst")),
    paste0(root, "/MAPSET/RES06-YLD/GAEZ-V5.RES06-YLD.MZE.WST.tif")
  )
})

test_that("the bucket root can be overridden", {
  withr::local_options(gaezv5.gcs_root = "https://example.org/GAEZ-V5")
  expect_equal(suppressMessages(build_gaez_url("AEZ57")), "https://example.org/GAEZ-V5/MAP/GAEZ-V5.AEZ57.tif")
})
