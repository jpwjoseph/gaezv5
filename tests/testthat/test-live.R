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

test_that("point values are read at full resolution from the remote file", {
  skip_if_no_bucket()
  url <- suppressMessages(build_gaez_url("AEZ33"))
  value <- read_gaez_point(url, 2.1, 13.5)
  local <- withr::local_tempfile(fileext = ".tif")
  curl::curl_download(url, local, quiet = TRUE)
  expect_equal(value, terra::extract(terra::rast(local), matrix(c(2.1, 13.5), ncol = 2))[[1]])
})

test_that("WMTS templates use the dimension members FAO publishes", {
  skip_if_no_bucket()
  url <- suppressMessages(build_gaez_url(crop = "maize", time_period = "HP0120", interactive = FALSE))
  tiles <- gaez_wmts_url(url)
  expect_match(tiles, "dim_PERIOD=HP0120&dim_CLIM=AGERA5&dim_SSP=HIST&dim_CROP-RES05=MZE&dim_WSIM-RES05=HRLM",
               fixed = TRUE)
})

test_that("FAO WMTS serves a tile for a GAEZ v5 layer", {
  skip_on_cran()
  url <- suppressMessages(build_gaez_url(crop = "maize", time_period = "HP0120", interactive = FALSE))
  tiles <- tryCatch(gaez_wmts_url(url), error = function(e) skip("bucket metadata not reachable"))
  tile <- sub("{z}", "3", sub("{y}", "3", sub("{x}", "4", tiles, fixed = TRUE), fixed = TRUE), fixed = TRUE)
  res <- tryCatch(
    curl::curl_fetch_memory(tile, handle = curl::new_handle(timeout = 20)),
    error = function(e) skip(paste("data.apps.fao.org not reachable:", conditionMessage(e)))
  )
  expect_equal(res$status_code, 200)
  expect_match(res$type, "^image/")
})

test_that("fetch_wmts_tile returns a georeferenced image from the FAO WMTS", {
  skip_on_cran()
  reachable <- tryCatch(
    curl::curl_fetch_memory("https://data.apps.fao.org/map/wmts/wmts?service=WMTS&request=GetCapabilities",
                            handle = curl::new_handle(timeout = 20))$status_code,
    error = function(e) NA
  )
  if (is.na(reachable)) skip("data.apps.fao.org not reachable")

  path <- fetch_wmts_tile(
    build_wmts_url(4, "RES05-YX"), c(30, -5, 42, 5), 400, 300,
    crop = map_crop_code_to_wmts("MZE", 4),
    time_period = map_time_period_to_wmts("HP0120"),
    climate_model = map_climate_model_to_wmts("AGERA5"),
    ssp = map_ssp_to_wmts("HIST"),
    input_level = map_input_level_to_wmts("HRLM", 4),
    verbose = FALSE
  )
  expect_true(!is.null(path) && file.exists(path))
  image <- terra::rast(path)
  expect_equal(dim(image), c(300, 400, 4))
  expect_true(any(terra::values(image[[4]]) > 0, na.rm = TRUE))
})
