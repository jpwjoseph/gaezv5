#' FAO WMTS tile URL for a GAEZ v5 layer
#'
#' Returns an XYZ tile URL template for the FAO GISMGR Web Map Tile Service
#' (\url{https://data.apps.fao.org/map/wmts/wmts}), which renders GAEZ v5
#' layers on the server. The template can be passed to
#' \code{leaflet::addTiles(urlTemplate = ...)} or any other web map client, so
#' a layer can be browsed at any zoom level without transferring raster data.
#'
#' The layer name and its dimension members (period, climate model, scenario,
#' crop, input level) are read from the metadata file FAO publishes next to
#' each GeoTIFF in the bucket (\code{<file>.json}).
#'
#' @param url Character - Download URL of a GAEZ v5 GeoTIFF, as returned by
#'   \code{build_gaez_url()}.
#' @param tilematrixset Character - Tile matrix set identifier. Defaults to
#'   \code{getOption("gaezv5.wmts_tilematrixset", "EPSG:3857")}, which matches
#'   web-mercator maps such as leaflet's default projection.
#' @param style Character - Optional style code. NULL uses the style published
#'   with the layer, or the service default when none is published.
#'
#' @return Character URL template with \code{{z}}, \code{{x}} and \code{{y}}
#'   placeholders.
#'
#' @details
#' Maps (theme 1) use the layer \code{fao-gismgr/GAEZ-V5/maps/<map>/<raster>}.
#' Mapsets use \code{fao-gismgr/GAEZ-V5/mapsets/<mapset>} and select one raster
#' with \code{dim_<DIMENSION>=<member>} parameters. Service settings can be
#' changed with the options \code{gaezv5.wmts_root},
#' \code{gaezv5.wmts_tilematrixset}, \code{gaezv5.wmts_tilematrix_prefix} and
#' \code{gaezv5.wmts_style}.
#'
#' @examples
#' \dontrun{
#' url <- build_gaez_url(crop = "maize", time_period = "HP0120")
#' tiles <- gaez_wmts_url(url)
#'
#' leaflet::leaflet() |>
#'   leaflet::addTiles() |>
#'   leaflet::addTiles(urlTemplate = tiles, options = leaflet::tileOptions(opacity = 0.8)) |>
#'   leaflet::setView(lng = 8, lat = 13, zoom = 6)
#' }
#'
#' @seealso \code{\link{preview_gaez_map}}
#'
#' @export
gaez_wmts_url <- function(url, tilematrixset = wmts_setting("tilematrixset"), style = NULL) {
  if (missing(url) || length(url) != 1 || !grepl("\\.tif$", url)) {
    stop("url must be a single GAEZ GeoTIFF URL from build_gaez_url()", call. = FALSE)
  }
  meta <- jsonlite::fromJSON(sub("\\.tif$", ".json", url), simplifyVector = TRUE)
  wmts_template(meta, tilematrixset = tilematrixset, style = style)
}


#' WMTS service settings, overridable through options(gaezv5.wmts_*)
#' @noRd
wmts_setting <- function(name) {
  defaults <- list(
    root = "https://data.apps.fao.org/map/wmts/wmts",
    tilematrixset = "EPSG:3857",
    tilematrix_prefix = "",
    style = "default",
    max_zoom = 12,
    max_tiles = 64
  )
  getOption(paste0("gaezv5.wmts_", name), defaults[[name]])
}


#' Assemble the WMTS GetTile template from a raster's FAO metadata record
#' @noRd
wmts_template <- function(meta, tilematrixset = wmts_setting("tilematrixset"), style = NULL) {
  if (!is.null(meta$mapCode)) {
    layer <- paste("fao-gismgr", meta$workspaceCode, "maps", meta$mapCode, meta$code, sep = "/")
    dims <- character(0)
  } else {
    layer <- paste("fao-gismgr", meta$workspaceCode, "mapsets", meta$mapsetCode, sep = "/")
    members <- meta$dimensionMembers
    dims <- stats_setnames(members$code, members$dimensionCode)
  }
  if (is.null(style)) style <- meta$styleCode
  paste0(
    wmts_base_url(layer, style = style, tilematrixset = tilematrixset),
    wmts_dimension_query(dims),
    wmts_tile_query("{z}", "{y}", "{x}")
  )
}


#' Named character vector without relying on the stats package
#' @noRd
stats_setnames <- function(values, names) {
  names(values) <- names
  values
}


#' GetTile request for a layer, without dimension and tile parameters
#' @noRd
wmts_base_url <- function(layer, style = NULL, tilematrixset = wmts_setting("tilematrixset")) {
  if (is.null(style) || is.na(style)) style <- wmts_setting("style")
  paste0(
    wmts_setting("root"),
    "?service=WMTS&version=1.0.0&request=GetTile",
    "&layer=", layer,
    "&style=", style,
    "&tilematrixset=", tilematrixset,
    "&format=image/png"
  )
}


#' @noRd
wmts_dimension_query <- function(dims) {
  dims <- dims[!is.na(dims)]
  if (length(dims) == 0) return("")
  paste0("&dim_", names(dims), "=", dims, collapse = "")
}


#' @noRd
wmts_tile_query <- function(z, row, col) {
  paste0("&tilematrix=", wmts_setting("tilematrix_prefix"), z, "&tilerow=", row, "&tilecol=", col)
}


#' Build WMTS GetTile URL for GAEZ data
#'
#' Counterpart of the former ImageServer endpoint builder. Maps a GAEZ variable
#' to its layer on the FAO GISMGR WMTS: theme-1 maps are addressed by map and
#' raster code, all other variables by their mapset code.
#'
#' @param theme_number Numeric - GAEZ theme number (1-6, 9 for time series)
#' @param variable_code Character - Variable or data code (e.g., "RES05-YX",
#'   "RES05-YXX", "AEZ57")
#' @param resolution Character - "1km", "10km" or NA, as in build_gaez_url()
#'
#' @return GetTile URL without dimension and tile parameters, or NA when the
#'   variable cannot be mapped
#' @noRd
build_wmts_url <- function(theme_number = NULL, variable_code = NULL, resolution = NA) {
  if (is.null(variable_code) || is.na(variable_code)) {
    return(NA_character_)
  }
  var_info <- tryCatch(
    suppressMessages(resolve_gaez_variable(variable_code, resolution)),
    error = function(e) NULL
  )
  if (is.null(var_info)) {
    return(NA_character_)
  }
  if (!is.null(theme_number) && !is.na(theme_number) && var_info$theme_number != theme_number) {
    return(NA_character_)
  }

  code <- var_info$data_code
  layer <- if (var_info$data_folder == "MAP") {
    paste("fao-gismgr/GAEZ-V5/maps", code, paste0("GAEZ-V5.", code), sep = "/")
  } else {
    paste("fao-gismgr/GAEZ-V5/mapsets", code, sep = "/")
  }
  wmts_base_url(layer)
}


#' Check if the WMTS is available for a dataset
#'
#' All GAEZ v5 maps and mapsets are published through the FAO GISMGR WMTS.
#' Set options(gaezv5.wmts = FALSE) to skip the WMTS in previews.
#'
#' @return Logical
#' @noRd
check_wmts_availability <- function(theme_number = NULL, variable_code = NULL) {
  if (identical(getOption("gaezv5.wmts", TRUE), FALSE)) {
    return(FALSE)
  }
  if (!is.null(theme_number)) {
    return(theme_number %in% c(1, 2, 3, 4, 5, 6, 9))
  }
  if (!is.null(variable_code)) {
    return(!is.na(build_wmts_url(variable_code = variable_code)))
  }
  FALSE
}


#' Fetch a georeferenced image from the FAO WMTS
#'
#' Counterpart of the former ImageServer export. Requests the web-mercator
#' tiles covering `bbox` at a zoom level that gives at least `width` pixels,
#' mosaics them, resamples the mosaic to a `width` x `height` grid in WGS84
#' and masks it to `geometry` when given.
#'
#' @param wmts_url GetTile URL from build_wmts_url()
#' @param bbox Numeric c(xmin, ymin, xmax, ymax) in WGS84
#' @param width,height Output size in pixels
#' @param crop,time_period,climate_model,ssp,water_supply,input_level,year,class
#'   Named dimension members from the map_*_to_wmts() functions
#' @param geometry Optional terra SpatVector used to mask the image
#' @param verbose Logical - print progress messages
#'
#' @return Path to a temporary four-band (RGBA) GeoTIFF, or NULL on failure
#' @noRd
fetch_wmts_tile <- function(wmts_url, bbox, width, height,
                            crop = NULL,
                            time_period = NULL,
                            climate_model = NULL,
                            ssp = NULL,
                            water_supply = NULL,
                            input_level = NULL,
                            year = NULL,
                            class = NULL,
                            geometry = NULL,
                            verbose = TRUE) {
  dims <- c(time_period, climate_model, ssp, crop, input_level, water_supply, year, class)
  request <- paste0(wmts_url, wmts_dimension_query(dims))
  if (verbose && length(dims[!is.na(dims)]) > 0) {
    cat("  Using layer filter:", paste0(names(dims), "=", dims, collapse = ", "), "\n")
    cat("Querying WMTS...\n")
  }

  tiles <- wmts_tile_grid(bbox, width)
  urls <- paste0(request, wmts_tile_query(tiles$z, tiles$row, tiles$col))

  responses <- tryCatch(fetch_wmts_responses(urls), error = function(e) {
    warning("WMTS request failed: ", conditionMessage(e), call. = FALSE)
    NULL
  })
  if (is.null(responses)) {
    return(NULL)
  }

  images <- lapply(responses, parse_wmts_response)
  ok <- vapply(images, inherits, logical(1), what = "SpatRaster")
  if (!any(ok)) {
    warning("WMTS returned no image: ", images[[1]], call. = FALSE)
    return(NULL)
  }
  if (verbose) {
    cat("Downloading image from server... (", sum(ok), " tiles, zoom ", tiles$z[1], ")\n", sep = "")
  }

  tile_rasters <- Map(function(img, i) {
    terra::ext(img) <- wmts_tile_extent(tiles$col[i], tiles$row[i], tiles$z[i])
    terra::crs(img) <- "EPSG:3857"
    img
  }, images[ok], which(ok))
  mosaic <- if (length(tile_rasters) == 1) {
    tile_rasters[[1]]
  } else {
    terra::merge(terra::sprc(tile_rasters))
  }

  template <- terra::rast(
    terra::ext(bbox[1], bbox[3], bbox[2], bbox[4]),
    nrows = height, ncols = width, nlyrs = 4, crs = "EPSG:4326"
  )
  image <- terra::project(mosaic, template, method = "near")

  if (!is.null(geometry)) {
    if (!terra::same.crs(geometry, image)) geometry <- terra::project(geometry, terra::crs(image))
    image <- terra::mask(image, geometry)
  }
  names(image) <- c("red", "green", "blue", "alpha")

  temp_file <- tempfile(fileext = ".tif")
  terra::writeRaster(image, temp_file, datatype = "INT2U", overwrite = TRUE)

  if (verbose) {
    cat("Image downloaded successfully\n")
  }
  temp_file
}


#' Request several WMTS tiles concurrently
#' @return List of curl responses (status_code, type, content)
#' @noRd
fetch_wmts_responses <- function(urls) {
  pool <- curl::new_pool(host_con = 6)
  responses <- vector("list", length(urls))
  for (i in seq_along(urls)) {
    local({
      j <- i
      curl::multi_add(
        curl::new_handle(url = urls[j], timeout = 60),
        done = function(res) responses[[j]] <<- res,
        fail = function(msg) responses[[j]] <<- list(status_code = NA_integer_, type = NA, error = msg),
        pool = pool
      )
    })
  }
  curl::multi_run(pool = pool)
  responses
}


#' Parse and validate a WMTS tile response
#'
#' @param response curl response (status_code, type, content)
#' @return Four-band (RGBA) SpatRaster without georeference, or a character
#'   string giving the reason for failure (HTTP status or OWS exception text)
#' @noRd
parse_wmts_response <- function(response) {
  if (is.null(response)) {
    return("no response")
  }
  if (!is.null(response$error)) {
    return(response$error)
  }
  is_image <- isTRUE(grepl("^image/", response$type))
  if (!isTRUE(response$status_code == 200) || !is_image) {
    text <- tryCatch(rawToChar(response$content), error = function(e) "")
    exception <- regmatches(text, regexpr("<ows:ExceptionText>[^<]*", text))
    return(paste0("HTTP ", response$status_code,
                  if (length(exception)) paste0(": ", sub("<ows:ExceptionText>", "", exception))))
  }
  decode_png_rgba(response$content)
}


#' Decode PNG bytes into a four-band RGBA SpatRaster held in memory
#' @noRd
decode_png_rgba <- function(bytes) {
  png <- tempfile(fileext = ".png")
  on.exit(unlink(png))
  writeBin(bytes, png)
  x <- suppressWarnings(terra::rast(png))

  table <- terra::coltab(x)[[1]]
  n <- terra::nlyr(x)
  if (n == 1 && !is.null(table)) {
    index <- match(terra::values(x, mat = FALSE), table$value)
    alpha <- if (!is.null(table$alpha)) table$alpha[index] else 255
    vals <- cbind(table$red[index], table$green[index], table$blue[index], alpha)
  } else {
    v <- terra::values(x)
    vals <- switch(as.character(n),
      "1" = cbind(v, v, v, 255),
      "2" = cbind(v[, 1], v[, 1], v[, 1], v[, 2]),
      "3" = cbind(v, 255),
      v[, 1:4]
    )
  }
  terra::rast(nrows = terra::nrow(x), ncols = terra::ncol(x), nlyrs = 4,
              ext = terra::ext(0, terra::ncol(x), 0, terra::nrow(x)), crs = "", vals = vals)
}


#' Web-mercator tiles covering a WGS84 bbox at the zoom that gives `width` pixels
#' @return data.frame with z, row, col
#' @noRd
wmts_tile_grid <- function(bbox, width) {
  lat_limit <- 85.05112878
  span <- max(bbox[3] - bbox[1], 1e-6)
  zoom <- ceiling(log2(width * 360 / (span * 256)))
  zoom <- min(max(zoom, 0), wmts_setting("max_zoom"))

  repeat {
    n <- 2^zoom
    col <- floor((c(bbox[1], bbox[3]) + 180) / 360 * n)
    lat <- pmin(pmax(c(bbox[4], bbox[2]), -lat_limit), lat_limit) * pi / 180
    row <- floor((1 - log(tan(lat) + 1 / cos(lat)) / pi) / 2 * n)
    col <- pmin(pmax(col, 0), n - 1)
    row <- pmin(pmax(row, 0), n - 1)
    count <- (diff(col) + 1) * (diff(row) + 1)
    if (count <= wmts_setting("max_tiles") || zoom == 0) break
    zoom <- zoom - 1
  }

  grid <- expand.grid(col = seq(col[1], col[2]), row = seq(row[1], row[2]))
  data.frame(z = zoom, row = grid$row, col = grid$col)
}


#' Web-mercator extent of tile (col, row) at zoom z
#' @noRd
wmts_tile_extent <- function(col, row, z) {
  half <- 20037508.342789244
  size <- 2 * half / 2^z
  xmin <- -half + col * size
  ymax <- half - row * size
  terra::ext(xmin, xmin + size, ymax - size, ymax)
}


#' Map GAEZ crop code to its WMTS dimension (CROP-RES02/05/06/07)
#' @noRd
map_crop_code_to_wmts <- function(crop_code, theme_number) {
  dimension <- c("3" = "CROP-RES02", "4" = "CROP-RES05", "5" = "CROP-RES06", "6" = "CROP-RES07")
  wmts_member(crop_code, dimension[as.character(theme_number)])
}


#' Map GAEZ time period to the WMTS PERIOD dimension
#' @noRd
map_time_period_to_wmts <- function(time_period) {
  wmts_member(time_period, "PERIOD")
}


#' Map GAEZ climate model to the WMTS CLIM dimension
#' @noRd
map_climate_model_to_wmts <- function(climate_model) {
  wmts_member(climate_model, "CLIM")
}


#' Map GAEZ SSP to the WMTS SSP dimension
#' @noRd
map_ssp_to_wmts <- function(ssp) {
  wmts_member(ssp, "SSP")
}


#' Map GAEZ water supply code (themes 5-6) to the WMTS WTS dimension
#' @noRd
map_water_supply_to_wmts <- function(water_supply) {
  wmts_member(water_supply, "WTS")
}


#' Map GAEZ input level code (themes 3-4) to the WMTS WSIM-RES02/05 dimension
#' @noRd
map_input_level_to_wmts <- function(input_level, theme_number) {
  dimension <- c("3" = "WSIM-RES02", "4" = "WSIM-RES05")
  wmts_member(input_level, dimension[as.character(theme_number)])
}


#' Map a time-series year to the WMTS YEAR dimension
#' @noRd
map_year_to_wmts <- function(year) {
  wmts_member(year, "YEAR")
}


#' Map a theme-1 class code to its WMTS dimension (LCC for LR-LCC, SLC for LR-SLC)
#' @noRd
map_class_to_wmts <- function(class, data_code) {
  wmts_member(class, sub("^LR-", "", data_code))
}


#' One named dimension member, or NULL when the value or dimension is missing
#' @noRd
wmts_member <- function(value, dimension) {
  if (is.null(value) || length(value) == 0 || is.na(value[1]) ||
      length(dimension) == 0 || is.na(dimension[1])) {
    return(NULL)
  }
  stats_setnames(toupper(as.character(value[1])), unname(dimension[1]))
}
