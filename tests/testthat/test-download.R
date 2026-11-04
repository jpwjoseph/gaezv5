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

test_that("build_wmts_url maps variables to WMTS layers", {
  withr::local_options(gaezv5.wmts_root = "https://example.org/wmts")
  expect_match(build_wmts_url(4, "RES05-YX"), "layer=fao-gismgr/GAEZ-V5/mapsets/RES05-YXX&", fixed = TRUE)
  expect_match(build_wmts_url(4, "RES05-YC", resolution = "1km"), "mapsets/RES05-YCX30AS&", fixed = TRUE)
  expect_match(build_wmts_url(1, "AEZ57"), "layer=fao-gismgr/GAEZ-V5/maps/AEZ57/GAEZ-V5.AEZ57&", fixed = TRUE)
  expect_true(is.na(build_wmts_url(variable_code = "NOT-A-VARIABLE")))
  expect_true(is.na(build_wmts_url(3, "RES05-YX")))
})

test_that("check_wmts_availability covers all themes and honours the option", {
  expect_true(all(vapply(c(1:6, 9), function(t) check_wmts_availability(theme_number = t), logical(1))))
  expect_false(check_wmts_availability(theme_number = 7))
  withr::local_options(gaezv5.wmts = FALSE)
  expect_false(check_wmts_availability(theme_number = 4))
})

test_that("map_*_to_wmts return FAO dimension members", {
  expect_equal(map_crop_code_to_wmts("MAIZ", 3), c("CROP-RES02" = "MAIZ"))
  expect_equal(map_crop_code_to_wmts("MZE", 6), c("CROP-RES07" = "MZE"))
  expect_equal(map_time_period_to_wmts("HP0120"), c(PERIOD = "HP0120"))
  expect_equal(map_climate_model_to_wmts("MRI-ESM2-0"), c(CLIM = "MRI-ESM2-0"))
  expect_equal(map_ssp_to_wmts("SSP370"), c(SSP = "SSP370"))
  expect_equal(map_water_supply_to_wmts("WST"), c(WTS = "WST"))
  expect_equal(map_input_level_to_wmts("LILM", 4), c("WSIM-RES05" = "LILM"))
  expect_equal(map_year_to_wmts("2010"), c(YEAR = "2010"))
  expect_equal(map_class_to_wmts("SC03", "LR-SLC"), c(SLC = "SC03"))
  expect_null(map_crop_code_to_wmts(NA, 1))
  expect_null(map_input_level_to_wmts("HRLM", 5))
})

test_that("wmts_tile_grid picks a zoom that covers the requested width", {
  global <- wmts_tile_grid(c(-180, -90, 180, 90), 800)
  expect_equal(unique(global$z), 2)
  expect_equal(nrow(global), 16)

  local <- wmts_tile_grid(c(30, -5, 42, 5), 400)
  expect_equal(unique(local$z), 6)
  expect_true(256 * 2^6 * 12 / 360 >= 400)

  withr::local_options(gaezv5.wmts_max_tiles = 4)
  expect_lte(nrow(wmts_tile_grid(c(30, -5, 42, 5), 400)), 4)
})

test_that("wmts_tile_extent returns web-mercator tile bounds", {
  half <- 20037508.342789244
  expect_equal(unname(as.vector(wmts_tile_extent(0, 0, 0))), c(-half, half, -half, half))
  expect_equal(unname(as.vector(wmts_tile_extent(1, 0, 1))), c(0, half, 0, half))
})

test_that("decode_png_rgba handles palette and RGBA PNGs", {
  pal <- terra::rast(nrows = 2, ncols = 2, vals = c(0, 1, 1, 0))
  terra::coltab(pal) <- data.frame(value = 0:1, red = c(255, 0), green = c(0, 0),
                                   blue = c(0, 255), alpha = c(255, 128))
  f1 <- withr::local_tempfile(fileext = ".png")
  suppressWarnings(terra::writeRaster(pal, f1, datatype = "INT1U"))
  x1 <- decode_png_rgba(readBin(f1, "raw", file.size(f1)))
  expect_equal(terra::nlyr(x1), 4)
  expect_equal(unname(terra::values(x1)[1, ]), c(255, 0, 0, 255))
  expect_equal(unname(terra::values(x1)[2, ]), c(0, 0, 255, 128))

  rgba <- terra::rast(nrows = 2, ncols = 2, nlyrs = 4, vals = rep(c(10, 20, 30, 40), each = 4))
  f2 <- withr::local_tempfile(fileext = ".png")
  suppressWarnings(terra::writeRaster(rgba, f2, datatype = "INT1U"))
  x2 <- decode_png_rgba(readBin(f2, "raw", file.size(f2)))
  expect_equal(unname(terra::values(x2)[1, ]), c(10, 20, 30, 40))
})

test_that("parse_wmts_response reports server exceptions", {
  xml <- charToRaw(paste0(
    "<ows:ExceptionReport><ows:Exception><ows:ExceptionText>Unknown layer</ows:ExceptionText>",
    "</ows:Exception></ows:ExceptionReport>"
  ))
  expect_equal(parse_wmts_response(list(status_code = 400L, type = "application/xml", content = xml)),
               "HTTP 400: Unknown layer")
  expect_equal(parse_wmts_response(list(error = "timeout")), "timeout")
  expect_equal(parse_wmts_response(NULL), "no response")
})
