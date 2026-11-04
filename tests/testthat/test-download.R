# Tests for download and remote-read helpers (no network access)

test_that("is_tiff_file recognises TIFF signatures only", {
  tif <- tempfile(fileext = ".tif")
  writeBin(as.raw(c(0x49, 0x49, 0x2a, 0x00, 0x08, 0x00, 0x00, 0x00)), tif)
  xml <- tempfile(fileext = ".tif")
  writeLines("<?xml version='1.0'?><Error><Code>NoSuchKey</Code></Error>", xml)

  expect_true(is_tiff_file(tif))
  expect_false(is_tiff_file(xml))
  expect_false(is_tiff_file(tempfile()))
})

test_that("expected_file_size reads Content-Range before Content-Length", {
  expect_equal(expected_file_size(c("HTTP/2 200", "content-length: 1234")), 1234)
  expect_equal(
    expected_file_size(c("HTTP/2 206", "Content-Length: 34", "Content-Range: bytes 100-133/134")),
    134
  )
  expect_equal(expected_file_size(c("HTTP/2 416", "content-range: bytes */5000")), 5000)
  expect_true(is.na(expected_file_size(character(0))))
})

test_that("overview_level keeps at least the requested width", {
  expect_equal(overview_level(43200, 800), 4)   # 43200 / 2^5 = 1350
  expect_equal(overview_level(4320, 800), 1)    # 4320 / 2^2 = 1080
  expect_equal(overview_level(1400, 800), -1)
  expect_equal(overview_level(190, 800), -1)
})

test_that("normalize_resolution accepts common spellings", {
  expect_equal(normalize_resolution("1 km"), "1km")
  expect_equal(normalize_resolution("10KM"), "10km")
  expect_true(is.na(normalize_resolution(NA)))
  expect_error(normalize_resolution("5km"), "resolution must be")
})

test_that("location_bbox builds a window around the point and validates input", {
  b <- location_bbox(c(10, 0), 111.32)
  expect_equal(b, c(9, -1, 11, 1))
  expect_equal(location_bbox(c(179.9, 89.9), 50)[3:4], c(180, 90))
  expect_error(location_bbox(c(200, 0), 50), "location must be")
  expect_error(location_bbox(c(10, 0), -5), "radius_km")
})

test_that("wmts_template selects mapset rasters through dimension parameters", {
  meta <- list(
    workspaceCode = "GAEZ-V5",
    mapsetCode = "RES05-YXX",
    code = "GAEZ-V5.RES05-YXX.HP0120.AGERA5.HIST.MZE.HRLM",
    dimensionMembers = data.frame(
      dimensionCode = c("PERIOD", "CLIM", "SSP", "CROP-RES05", "WSIM-RES05"),
      code = c("HP0120", "AGERA5", "HIST", "MZE", "HRLM")
    ),
    styleCode = NULL
  )
  url <- wmts_template(meta)
  expect_match(url, "^https://data.apps.fao.org/map/wmts/wmts\\?service=WMTS")
  expect_match(url, "layer=fao-gismgr/GAEZ-V5/mapsets/RES05-YXX&style=default", fixed = TRUE)
  expect_match(url, "&dim_CROP-RES05=MZE&dim_WSIM-RES05=HRLM", fixed = TRUE)
  expect_match(url, "tilematrixset=EPSG:3857", fixed = TRUE)
  expect_true(endsWith(url, "&tilematrix={z}&tilerow={y}&tilecol={x}"))
})

test_that("wmts_template addresses single maps by map and raster code", {
  meta <- list(workspaceCode = "GAEZ-V5", mapCode = "AEZ57", code = "GAEZ-V5.AEZ57",
               styleCode = "AEZ57")
  url <- wmts_template(meta)
  expect_match(url, "layer=fao-gismgr/GAEZ-V5/maps/AEZ57/GAEZ-V5.AEZ57&style=AEZ57", fixed = TRUE)
  expect_false(grepl("dim_", url))
})
