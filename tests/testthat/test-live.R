# Checks against the FAO bucket. Skipped on CRAN and without network access.

skip_if_no_bucket <- function() {
  skip_on_cran()
  skip_if_offline("storage.googleapis.com")
}

test_that("one layer per theme exists in the bucket", {
  skip_if_no_bucket()
  urls <- suppressMessages(c(
    build_gaez_url("AEZ57"),
    build_gaez_url("LR-LCC", class = "LC02"),
    build_gaez_url("RES01-PRC", time_period = "HP0120"),
    build_gaez_url("RES01-PRC-TS", start_year = 2010),
    build_gaez_url("RES02-YLD", crop = "maize", climate_model = "GFDL-ESM4", interactive = FALSE),
    build_gaez_url(crop = "maize", interactive = FALSE),
    build_gaez_url("RES05-SX", crop = "maize", time_period = "HP0120", resolution = "1km",
                   interactive = FALSE),
    build_gaez_url("RES06-YLD", crop = "maize", interactive = FALSE),
    build_gaez_url("RES07-YGA-CL", crop = "maize", interactive = FALSE)
  ))
  exists <- vapply(urls, check_url_exists, logical(1), timeout = 30)
  expect_true(all(exists), info = paste(urls[!exists], collapse = "\n"))
})

test_that("failed downloads leave no file and successful ones are GeoTIFFs", {
  skip_if_no_bucket()
  dir <- withr::local_tempdir()
  root <- "https://storage.googleapis.com/fao-gismgr-gaez-v5-data/DATA/GAEZ-V5"
  urls <- paste0(root, c("/MAP/GAEZ-V5.AEZ33.tif", "/MAP/GAEZ-V5.DOES-NOT-EXIST.tif"))
  dest <- file.path(dir, basename(urls))

  res <- fetch_gaez_files(urls, dest)

  expect_equal(res$success, c(TRUE, FALSE))
  expect_equal(res$status_code[2], 404)
  expect_true(is_tiff_file(dest[1]))
  expect_equal(list.files(dir), basename(dest[1]))
})

test_that("previews read a coarse overview of the remote file", {
  skip_if_no_bucket()
  url <- suppressMessages(build_gaez_url("AEZ33"))
  x <- read_gaez_preview(url, width = 500)
  expect_s4_class(x, "SpatRaster")
  expect_true(terra::ncol(x) >= 500 && terra::ncol(x) < 4320)
})
