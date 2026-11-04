# Offline check of the WMTS client against 06_wmts_mock_server.py.
# The source image encodes position: red = (lon + 180) / 360 * 255, green = (lat + 90) / 180 * 255,
# so the georeferencing error of fetched images can be measured directly.
# Run from the package root:
#   Rscript dev/data-access-review/06_wmts_mock_source.R mock_src.tif
#   python3 dev/data-access-review/06_wmts_mock_server.py mock_src.tif 8765 wmts.log &
#   MOCK_LOG=wmts.log Rscript dev/data-access-review/06_wmts_mock_check.R
source("dev/data-access-review/harness.R")
suppressPackageStartupMessages(library(terra))
grDevices::pdf(NULL)
options(gaezv5.wmts_root = "http://127.0.0.1:8765/wmts")
mock_log <- Sys.getenv("MOCK_LOG")
last_request <- function() utils::URLdecode(tail(readLines(mock_log), 1))

expected_error <- function(img) {
  xy <- xyFromCell(img, 1:ncell(img)); v <- values(img)
  keep <- abs(xy[, 2]) < 80 & !is.na(v[, 1])
  er <- abs(v[keep, 1] - round((xy[keep, 1] + 180) / 360 * 255))
  eg <- abs(v[keep, 2] - round((xy[keep, 2] + 90) / 180 * 255))
  sprintf("cells=%d  R err mean %.2f max %d | G err mean %.2f max %d | alpha %s",
          sum(keep), mean(er), max(er), mean(eg), max(eg), paste(range(v[keep, 4]), collapse = "-"))
}

cat("## build_wmts_url\n")
for (v in c("RES05-YX", "AEZ57", "LR-LCC", "RES01-PRC-TS", "NOPE")) {
  cat(sprintf("%-14s %s\n", v, build_wmts_url(variable_code = v)))
}
cat("check_wmts_availability:", check_wmts_availability(theme_number = 4),
    check_wmts_availability(theme_number = 7), check_wmts_availability(variable_code = "AEZ57"), "\n")

cat("\n## map_*_to_wmts\n")
print(c(map_crop_code_to_wmts("MZE", 4), map_crop_code_to_wmts("MAIZ", 3),
        map_input_level_to_wmts("HRLM", 4), map_water_supply_to_wmts("WSR"), map_year_to_wmts("2010"),
        map_class_to_wmts("LC02", "LR-LCC"), map_time_period_to_wmts(NA), map_crop_code_to_wmts(NA, 1)))

cat("\n## fetch_wmts_tile: East Africa window\n")
u <- build_wmts_url(4, "RES05-YX")
f <- fetch_wmts_tile(u, c(30, -5, 42, 5), 400, 300,
                     crop = map_crop_code_to_wmts("MZE", 4),
                     time_period = map_time_period_to_wmts("HP0120"),
                     climate_model = map_climate_model_to_wmts("AGERA5"),
                     ssp = map_ssp_to_wmts("HIST"),
                     input_level = map_input_level_to_wmts("HRLM", 4), verbose = TRUE)
img <- rast(f)
cat("dims", dim(img), "| ext", as.vector(ext(img)), "\n")
cat(expected_error(img), "\n")
cat("last request:", last_request(), "\n")

cat("\n## global 800x600\n")
g <- rast(fetch_wmts_tile(u, c(-180, -90, 180, 90), 800, 600, verbose = FALSE))
cat("dims", dim(g), "\n"); cat(expected_error(g), "\n")
cat("rows beyond 86N are NA:", all(is.na(values(g)[xyFromCell(g, 1:ncell(g))[, 2] > 86, 1])), "\n")

cat("\n## geometry mask\n")
poly <- vect("POLYGON ((32 -3, 40 -3, 40 3, 32 3, 32 -3))", crs = "EPSG:4326")
m <- rast(fetch_wmts_tile(u, c(30, -5, 42, 5), 400, 300, geometry = poly, verbose = FALSE))
xy <- xyFromCell(m, 1:ncell(m))
inside <- xy[, 1] > 32.1 & xy[, 1] < 39.9 & abs(xy[, 2]) < 2.9
outside <- xy[, 1] < 31.9 | xy[, 1] > 40.1
cat("inside non-NA:", mean(!is.na(values(m)[inside, 1])), " outside NA:", mean(is.na(values(m)[outside, 1])), "\n")

cat("\n## server exception\n")
bad <- sub("mapsets/RES05-YXX", "mapsets/BAD", u)
r <- withCallingHandlers(fetch_wmts_tile(bad, c(30, -5, 42, 5), 200, 150, verbose = FALSE),
                         warning = function(w) { cat("warning:", conditionMessage(w), "\n"); invokeRestart("muffleWarning") })
cat("returned:", deparse(r), "\n")

cat("\n## preview_gaez_map(source = 'wmts') through the mock; values from the real bucket\n")
p <- suppressMessages(preview_gaez_map(crop = "maize", time_period = "HP0120", location = c(36.8, -1.3),
                                       radius_km = 150, viewer = "static", source = "wmts", return_data = TRUE,
                                       verbose = FALSE, interactive = FALSE))
cat("source:", p$source, "| image:", dim(p$image), "| data:", dim(p$data), "| value at point:", p$location_value, "\n")
cat(expected_error(p$image), "\n")
p2 <- suppressMessages(preview_gaez_map(variable = "LR-LCC", class = "LC02", viewer = "static", source = "wmts",
                                        return_data = TRUE, verbose = FALSE, interactive = FALSE,
                                        bbox = c(-10, 30, 30, 50)))
cat("LR-LCC source:", p2$source, "\nrequest:", last_request(), "\n")
p3 <- suppressMessages(preview_gaez_map(variable = "RES01-PRC-TS", start_year = 2015, viewer = "static",
                                        source = "wmts", return_data = TRUE, verbose = FALSE, interactive = FALSE,
                                        bbox = c(-60, -20, -40, 0)))
cat("TS source:", p3$source, "\nrequest:", last_request(), "\n")

cat("\n## source = 'auto' against the real FAO host (blocked here)\n")
options(gaezv5.wmts_root = NULL)
p4 <- suppressMessages(suppressWarnings(preview_gaez_map(crop = "maize", time_period = "HP0120", bbox = c(30, -5, 42, 5),
                       viewer = "static", return_data = TRUE, verbose = FALSE, interactive = FALSE)))
cat("source:", p4$source, "| data:", dim(p4$data), "| image NULL:", is.null(p4$image), "\n")
r <- tryCatch(suppressMessages(suppressWarnings(preview_gaez_map(crop = "maize", time_period = "HP0120",
              viewer = "static", source = "wmts", verbose = FALSE, interactive = FALSE))),
              error = function(e) conditionMessage(e))
cat("source = 'wmts' without access:", r, "\n")
