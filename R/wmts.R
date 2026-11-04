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
#' @param tilematrixset Character - Tile matrix set identifier. "EPSG:3857"
#'   (default) matches web-mercator maps such as leaflet's default projection.
#' @param style Character - Optional style code. NULL uses the style published
#'   with the layer, or the service default when none is published.
#'
#' @return Character URL template with \code{{z}}, \code{{x}} and \code{{y}}
#'   placeholders.
#'
#' @details
#' Maps (theme 1) use the layer \code{fao-gismgr/GAEZ-V5/maps/<map>/<raster>}.
#' Mapsets use \code{fao-gismgr/GAEZ-V5/mapsets/<mapset>} and select one raster
#' with \code{dim_<DIMENSION>=<member>} parameters. The service root can be
#' changed with \code{options(gaezv5.wmts_root = "<url>")}.
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
#' @seealso \code{\link{preview_gaez_map}} with \code{tiles = TRUE}
#'
#' @export
gaez_wmts_url <- function(url, tilematrixset = "EPSG:3857", style = NULL) {
  if (missing(url) || length(url) != 1 || !grepl("\\.tif$", url)) {
    stop("url must be a single GAEZ GeoTIFF URL from build_gaez_url()", call. = FALSE)
  }
  meta <- jsonlite::fromJSON(sub("\\.tif$", ".json", url), simplifyVector = TRUE)
  wmts_template(meta, tilematrixset = tilematrixset, style = style)
}


#' Assemble the WMTS GetTile template from a raster's FAO metadata record
#' @noRd
wmts_template <- function(meta, tilematrixset = "EPSG:3857", style = NULL) {
  if (!is.null(meta$mapCode)) {
    layer <- paste("fao-gismgr", meta$workspaceCode, "maps", meta$mapCode, meta$code, sep = "/")
    dims <- character(0)
  } else {
    layer <- paste("fao-gismgr", meta$workspaceCode, "mapsets", meta$mapsetCode, sep = "/")
    members <- meta$dimensionMembers
    dims <- paste0("&dim_", members$dimensionCode, "=", members$code)
  }
  if (is.null(style)) {
    style <- if (is.null(meta$styleCode)) "default" else meta$styleCode
  }

  paste0(
    getOption("gaezv5.wmts_root", "https://data.apps.fao.org/map/wmts/wmts"),
    "?service=WMTS&version=1.0.0&request=GetTile",
    "&layer=", layer,
    "&style=", style,
    "&tilematrixset=", tilematrixset,
    "&format=image/png",
    paste(dims, collapse = ""),
    "&tilematrix={z}&tilerow={y}&tilecol={x}"
  )
}
