#' Preview GAEZ Data as Interactive Map
#'
#' Creates an interactive leaflet map preview of GAEZ v5 data without requiring
#' full dataset downloads. Requests a server-rendered image from the FAO GISMGR
#' Web Map Tile Service (WMTS) first, reads the Cloud-Optimized GeoTIFF in the
#' FAO bucket when the WMTS is unavailable, and downloads the full file as a
#' last resort. Supports global, country-level, location and bounding-box
#' previews.
#'
#' @inheritParams build_gaez_url
#' @inheritParams load_gaez_data
#' @param country Character, SpatVector, or NULL - Geographic extent for preview.
#'   \itemize{
#'     \item NULL (default): Global extent
#'     \item Character: Country name (e.g., "Niger") or ISO3 code (e.g., "NER")
#'     \item SpatVector: Custom boundary object
#'   }
#' @param bbox Numeric vector - Custom bounding box as c(xmin, ymin, xmax, ymax)
#'   in WGS84 coordinates. Overridden by country or location.
#' @param width Numeric - Preview image width in pixels (default: 800)
#' @param height Numeric - Preview image height in pixels (default: 600)
#' @param viewer Character - Display method for preview. Options:
#'   \itemize{
#'     \item "leaflet" (default): Interactive leaflet map with pan/zoom
#'     \item "static": Static plot using terra::plot()
#'     \item "auto": Automatically select based on environment
#'   }
#' @param display Logical - Whether to display the map (default: TRUE). Set to
#'   FALSE to return map object without displaying.
#' @param save_path Character - Optional file path to save the preview image.
#'   If provided, saves as PNG for static plots or HTML for leaflet maps.
#' @param return_data Logical - Whether to return the underlying raster data
#'   along with the map object (default: FALSE)
#' @param color_palette Character - Color palette name for visualization. Default
#'   is "YlOrRd". Other options: "viridis", "magma", "plasma", "RdYlGn", etc.
#'   See ?leaflet::colorNumeric for more options.
#' @param na_color Character - Color for NA values (default: "transparent")
#' @param opacity Numeric - Overlay opacity (0-1, default: 0.7)
#' @param class Character - Class code for per-class theme-1 variables
#'   ("LR-LCC", "LR-SLC"). See \code{build_gaez_url()}.
#' @param location Numeric - Optional point \code{c(lon, lat)} in WGS84. The
#'   preview covers a window of \code{radius_km} around the point, marks it,
#'   and reports the full-resolution value at the point. Overridden by country.
#' @param radius_km Numeric - Half-width of the window around \code{location}
#'   in kilometres (default: 50).
#' @param tiles Logical - Add the live FAO WMTS tile layer
#'   (see \code{\link{gaez_wmts_url}}) as an extra overlay in the leaflet
#'   viewer, for zooming beyond the preview resolution. Default FALSE.
#' @param source Character - Data source for the preview:
#'   \itemize{
#'     \item "auto" (default): WMTS image, then Cloud-Optimized GeoTIFF, then
#'       full download
#'     \item "wmts": WMTS image only (error if unavailable)
#'     \item "cog": data values read from the Cloud-Optimized GeoTIFF, coloured
#'       with \code{color_palette} and shown with a numeric legend
#'   }
#'
#' @return Depends on viewer type:
#'   \itemize{
#'     \item "leaflet": A leaflet map object
#'     \item "static": Returns NULL invisibly (displays plot)
#'   }
#'   If return_data = TRUE, returns a list with the map, \code{data} (data
#'   values at preview resolution, read from the Cloud-Optimized GeoTIFF),
#'   \code{image} (the WMTS RGBA image, or NULL), \code{source} ("wmts",
#'   "cog" or "download") and descriptive labels. Use \code{load_gaez_data()}
#'   for full resolution. With \code{location}, the list also holds
#'   \code{location_value}, the full-resolution value at the point.
#'
#' @details
#' ## Preview Strategy
#'
#' The function uses a three-tier strategy:
#' 1. **WMTS** (server-rendered, no data transfer): the FAO GISMGR Web Map
#'    Tile Service renders the layer with FAO's styles. The function requests
#'    the web-mercator tiles covering the extent, mosaics them, resamples the
#'    mosaic to \code{width} x \code{height} pixels in WGS84 and masks it to the
#'    country boundary. The image carries colours, not data values.
#' 2. **Cloud-Optimized GeoTIFF**: when the WMTS fails or returns an empty
#'    image, the function reads the coarsest overview of the GeoTIFF that still
#'    provides about \code{width} pixels across the extent (a global preview of
#'    a ~1 km layer transfers under 1 MB).
#' 3. **Download**: the full file is downloaded with \code{load_gaez_data()}.
#'
#' WMTS settings can be changed with the options \code{gaezv5.wmts_root},
#' \code{gaezv5.wmts_tilematrixset}, \code{gaezv5.wmts_tilematrix_prefix},
#' \code{gaezv5.wmts_style}, \code{gaezv5.wmts_max_zoom} and
#' \code{gaezv5.wmts_max_tiles}; \code{options(gaezv5.wmts = FALSE)} skips
#' the WMTS.
#'
#' ## Geographic Extents
#'
#' Four ways to specify extent:
#' 1. **Global** (default): Shows entire world coverage
#' 2. **Country**: Automatically retrieves boundary and crops to extent
#' 3. **Location**: Window of \code{radius_km} around a point
#' 4. **Custom bbox**: Manually specify rectangular bounds
#'
#' Priority: country > location > bbox > global
#'
#' ## Visualization Options
#'
#' **Leaflet viewer** (recommended):
#' - Interactive pan and zoom
#' - Multiple basemap options (OpenStreetMap, Satellite)
#' - Toggle layers on/off
#' - Hover coordinates
#'
#' **Static viewer**:
#' - Simple quick plot
#' - Useful for scripting/automation
#' - Less memory intensive
#'
#' ## Color Palettes
#'
#' The function automatically handles data ranges and creates appropriate color
#' scales. Negative and zero values are filtered out for yield data.
#'
#' @examples
#' \dontrun{
#' # Global preview of maize yield (historical)
#' preview_gaez_map(
#'   crop = "maize",
#'   time_period = "HP0120"
#' )
#'
#' # Country-level preview
#' preview_gaez_map(
#'   crop = "wheat",
#'   time_period = "FP4160",
#'   ssp = "SSP370",
#'   country = "Niger"
#' )
#'
#' # Custom bounding box (West Africa)
#' preview_gaez_map(
#'   crop = "sorghum",
#'   time_period = "HP0120",
#'   bbox = c(-20, 5, 20, 25)
#' )
#'
#' # Window around a location, with the value at the point
#' preview_gaez_map(
#'   crop = "maize",
#'   time_period = "HP0120",
#'   location = c(36.8, -1.3),
#'   radius_km = 100
#' )
#'
#' # Add server-rendered FAO WMTS tiles as a second overlay
#' preview_gaez_map(crop = "maize", time_period = "HP0120", tiles = TRUE)
#'
#' # Static plot for quick inspection
#' preview_gaez_map(
#'   crop = "rice",
#'   country = "NER",
#'   viewer = "static"
#' )
#'
#' # Save preview to file
#' preview_gaez_map(
#'   crop = "maize",
#'   country = "Niger",
#'   save_path = "niger_maize_preview.html"
#' )
#'
#' # Return data for further analysis
#' result <- preview_gaez_map(
#'   crop = "wheat",
#'   country = "Niger",
#'   return_data = TRUE
#' )
#' terra::plot(result$data)
#' print(result$map)
#' }
#'
#' @seealso
#' \code{\link{load_gaez_data}} for loading full datasets,
#' \code{\link{build_gaez_url}} for URL construction,
#' \code{\link{get_country_boundary}} for country boundaries
#'
#' @export
#' @importFrom leaflet leaflet addTiles addProviderTiles addRasterImage addLegend addLayersControl leafletOptions providers
preview_gaez_map <- function(variable = "RES05-YX",
                              time_period = "FP4160",
                              start_year = NULL,
                              end_year = NULL,
                              climate_model = NULL,
                              ssp = "SSP370",
                              crop = "WHEA",
                              water_management_level = "HRLM",
                              water_supply = "WSR",
                              resolution = NA,
                              country = NULL,
                              bbox = NULL,
                              width = 800,
                              height = 600,
                              viewer = c("leaflet", "static", "auto"),
                              display = TRUE,
                              save_path = NULL,
                              return_data = FALSE,
                              interactive = TRUE,
                              verbose = TRUE,
                              color_palette = "YlOrRd",
                              na_color = "transparent",
                              opacity = 0.7,
                              class = NULL,
                              location = NULL,
                              radius_km = 50,
                              tiles = FALSE,
                              source = c("auto", "wmts", "cog")) {
  
  # Match viewer and source arguments
  viewer <- match.arg(viewer)
  source <- match.arg(source)
  
  # Auto-select viewer based on environment
  if (viewer == "auto") {
    if (requireNamespace("leaflet", quietly = TRUE) && interactive()) {
      viewer <- "leaflet"
    } else {
      viewer <- "static"
    }
  }
  
  # Check required packages
  if (!requireNamespace("terra", quietly = TRUE)) {
    stop("Package 'terra' is required. Install with: install.packages('terra')",
         call. = FALSE)
  }
  
  if (viewer == "leaflet" && !requireNamespace("leaflet", quietly = TRUE)) {
    stop("Package 'leaflet' is required for interactive maps. ",
         "Install with: install.packages('leaflet') or use viewer='static'",
         call. = FALSE)
  }
  
  # Normalize inputs
  variable <- toupper(variable)
  crop <- toupper(crop)
  if (!is.null(climate_model)) climate_model <- toupper(climate_model)
  if (!is.null(ssp)) ssp <- toupper(ssp)
  water_management_level <- toupper(water_management_level)
  water_supply <- toupper(water_supply)
  
  if (verbose) {
    cat("\n=== GAEZ Data Preview ===\n\n")
  }
  
  # ====================
  # PARAMETER VALIDATION
  # ====================
  
  if (verbose) {
    cat("[1/5] Validating parameters...\n")
  }
  
  # Use comprehensive parameter validation - this validates all combinations
  # before attempting data access and provides informative error messages
  validated <- validate_gaez_parameters(
    variable = variable,
    time_period = time_period,
    start_year = start_year,
    end_year = end_year,
    climate_model = climate_model,
    ssp = ssp,
    crop = crop,
    water_management_level = water_management_level,
    water_supply = water_supply,
    interactive = interactive,
    verbose = verbose,
    resolution = resolution,
    class = class
  )
  
  # Extract validated parameters
  var_info <- validated$var_info
  crop_code <- validated$crop_code
  time_period <- validated$time_period
  climate_model <- validated$climate_model
  ssp <- validated$ssp
  water_management_level <- validated$water_management_level
  water_supply <- validated$water_supply
  
  # ====================
  # DETERMINE EXTENT
  # ====================
  
  if (verbose) {
    cat("\n[2/5] Determining geographic extent...\n")
  }
  
  extent_label <- "Global"
  bbox_coords <- NULL
  boundary_vector <- NULL
  
  if (!is.null(country)) {
    # Get country boundary
    if (verbose) {
      cat("  Retrieving country boundary...\n")
    }
    
    boundary_vector <- get_country_boundary(country, verbose = FALSE)
    # terra::ext returns c(xmin, xmax, ymin, ymax)
    # but bbox_coords uses c(xmin, ymin, xmax, ymax)
    ext_vector <- as.vector(terra::ext(boundary_vector))
    bbox_coords <- c(ext_vector[1], ext_vector[3], ext_vector[2], ext_vector[4])
    
    # Extract country name for labeling
    if (inherits(country, "SpatVector")) {
      if ("NAME_0" %in% names(boundary_vector)) {
        extent_label <- boundary_vector$NAME_0[1]
      } else {
        extent_label <- "Custom region"
      }
    } else {
      extent_label <- country
    }
    
    if (verbose) {
      cat("  Extent:", extent_label, "\n")
      cat("  Bbox:", paste(round(bbox_coords, 2), collapse = ", "), "\n")
    }
    
  } else if (!is.null(location)) {
    bbox_coords <- location_bbox(location, radius_km)
    extent_label <- sprintf("%.3f, %.3f", location[1], location[2])

    if (verbose) {
      cat("  Extent:", radius_km, "km around", extent_label, "\n")
      cat("  Bbox:", paste(round(bbox_coords, 2), collapse = ", "), "\n")
    }

  } else if (!is.null(bbox)) {
    # User-provided bbox
    if (length(bbox) != 4) {
      stop("bbox must be a numeric vector of length 4: c(xmin, ymin, xmax, ymax)",
           call. = FALSE)
    }
    
    # Validate bbox values
    if (bbox[1] < -180 || bbox[1] > 180 || bbox[3] < -180 || bbox[3] > 180) {
      stop("Longitude values must be between -180 and 180", call. = FALSE)
    }
    if (bbox[2] < -90 || bbox[2] > 90 || bbox[4] < -90 || bbox[4] > 90) {
      stop("Latitude values must be between -90 and 90", call. = FALSE)
    }
    if (bbox[1] >= bbox[3]) {
      stop("xmin must be less than xmax", call. = FALSE)
    }
    if (bbox[2] >= bbox[4]) {
      stop("ymin must be less than ymax", call. = FALSE)
    }
    
    bbox_coords <- bbox
    extent_label <- "Custom region"
    
    if (verbose) {
      cat("  Extent: Custom bbox\n")
      cat("  Bbox:", paste(round(bbox_coords, 2), collapse = ", "), "\n")
    }
    
  } else {
    # Global extent
    bbox_coords <- c(-180, -90, 180, 90)
    if (verbose) {
      cat("  Extent: Global\n")
    }
  }
  
  # ====================
  # LOAD PREVIEW DATA
  # ====================

  if (verbose) {
    cat("\n[3/5] Loading data...\n")
  }

  theme_number <- var_info$theme_number
  raster_data <- NULL
  data_source <- NA_character_
  cog_bbox <- if (extent_label == "Global") NULL else bbox_coords

  if (source != "cog" && check_wmts_availability(theme_number = theme_number)) {
    if (verbose) {
      cat("  Attempting WMTS preview...\n")
    }

    wmts_url <- build_wmts_url(theme_number = theme_number,
                               variable_code = var_info$data_code)

    if (!is.na(wmts_url)) {
      dims <- list(
        crop = map_crop_code_to_wmts(crop_code, theme_number),
        time_period = map_time_period_to_wmts(time_period),
        climate_model = map_climate_model_to_wmts(climate_model),
        ssp = map_ssp_to_wmts(ssp),
        water_supply = map_water_supply_to_wmts(water_supply),
        input_level = map_input_level_to_wmts(water_management_level, theme_number),
        year = map_year_to_wmts(validated$year),
        class = map_class_to_wmts(validated$class, var_info$data_code)
      )

      if (verbose) {
        cat("  Mapped parameters for WMTS:\n")
        for (d in dims[!vapply(dims, is.null, logical(1))]) {
          cat("    ", names(d), ": ", d, "\n", sep = "")
        }
      }

      temp_file <- tryCatch(
        do.call(fetch_wmts_tile, c(
          list(wmts_url = wmts_url, bbox = bbox_coords, width = width, height = height),
          dims,
          list(geometry = boundary_vector, verbose = verbose)
        )),
        error = function(e) {
          warning("WMTS image could not be assembled: ", conditionMessage(e), call. = FALSE)
          NULL
        }
      )

      if (!is.null(temp_file) && file.exists(temp_file)) {
        image <- terra::rast(temp_file)
        terra::RGB(image) <- 1:4
        alpha <- terra::values(image[[4]], mat = FALSE)

        if (any(alpha > 0, na.rm = TRUE)) {
          raster_data <- image
          data_source <- "wmts"
          if (verbose) cat("  Successfully loaded image via WMTS\n")
        } else if (verbose) {
          cat("  WMTS returned no visible data for this extent\n")
        }
      } else if (verbose) {
        cat("  WMTS preview failed\n")
      }
    }

    if (is.null(raster_data) && source == "wmts") {
      stop("WMTS preview failed. Use source = \"auto\" or \"cog\".", call. = FALSE)
    }
  }

  if (is.null(raster_data)) {
    if (verbose) cat("  Reading the Cloud-Optimized GeoTIFF...\n")
    raster_data <- tryCatch(
      read_gaez_preview(validated$url, bbox = cog_bbox, width = width, boundary = boundary_vector),
      error = function(e) {
        if (verbose) {
          cat("  Remote read failed (", conditionMessage(e), "); downloading the file\n", sep = "")
        }
        NULL
      }
    )
    if (!is.null(raster_data)) {
      data_source <- "cog"
      if (verbose) cat("  Read overview from", validated$url, "\n")
    }
  }

  if (is.null(raster_data)) {
    result <- load_gaez_data(
      variable = var_info$data_code,
      time_period = if (!is.na(validated$year)) validated$year else time_period,
      climate_model = if (is.na(climate_model)) NULL else climate_model,
      ssp = if (is.na(ssp)) "SSP370" else ssp,
      crop = if (is.na(crop_code)) crop else crop_code,
      water_management_level = if (is.na(water_management_level)) "HRLM" else water_management_level,
      water_supply = if (is.na(water_supply)) "WSR" else water_supply,
      class = class,
      country = country,
      return_metadata = TRUE,
      verbose = verbose,
      interactive = interactive
    )

    raster_data <- result$raster
    data_source <- "download"

    if (is.null(country) && extent_label != "Global") {
      ext_obj <- terra::ext(c(bbox_coords[1], bbox_coords[3], bbox_coords[2], bbox_coords[4]))
      raster_data <- terra::crop(raster_data, ext_obj)
    }
  }

  value_data <- raster_data
  image_data <- NULL
  if (identical(data_source, "wmts")) {
    image_data <- raster_data
    value_data <- NULL
    if (return_data) {
      value_data <- tryCatch(
        read_gaez_preview(validated$url, bbox = cog_bbox, width = width, boundary = boundary_vector),
        error = function(e) {
          warning("Could not read data values: ", conditionMessage(e), call. = FALSE)
          NULL
        }
      )
    }
  }

  location_value <- NULL
  if (!is.null(location)) {
    location_value <- tryCatch(
      read_gaez_point(validated$url, location[1], location[2]),
      error = function(e) {
        if (is.null(value_data)) NA else terra::extract(value_data, matrix(location, ncol = 2))[[1]]
      }
    )
    if (verbose) cat("  Value at location:", format(location_value), "\n")
  }

  tile_url <- NULL
  if (isTRUE(tiles)) {
    if (viewer != "leaflet") {
      warning("tiles = TRUE requires viewer = \"leaflet\"", call. = FALSE)
    } else {
      tile_url <- tryCatch(gaez_wmts_url(validated$url), error = function(e) {
        warning("Could not build the FAO WMTS URL: ", conditionMessage(e), call. = FALSE)
        NULL
      })
    }
  }

  # ====================
  # CREATE VISUALIZATION
  # ====================
  
  if (verbose) {
    cat("\n[4/5] Creating visualization...\n")
  }
  
  crop_desc <- gaez_crops$name[which(gaez_crops$gaez_crop_code == crop_code &
                                       gaez_crops$gaez_theme == theme_number)[1]]
  if (length(crop_desc) == 0 || is.na(crop_desc)) crop_desc <- NA_character_

  time_desc <- gaez_scenarios$description[which(gaez_scenarios$time_period == time_period)[1]]
  if (length(time_desc) == 0 || is.na(time_desc)) time_desc <- validated$year

  title_parts <- c(if (is.na(crop_desc)) var_info$variable_name else crop_desc,
                   validated$class, time_desc)
  map_title <- paste(title_parts[!is.na(title_parts)], collapse = " - ")
  if (extent_label != "Global") {
    map_title <- paste0(map_title, " (", extent_label, ")")
  }
  
  # Create visualization based on viewer type
  if (viewer == "leaflet") {
    map_obj <- create_leaflet_preview(
      raster_data = raster_data,
      map_title = map_title,
      color_palette = color_palette,
      na_color = na_color,
      opacity = opacity,
      verbose = verbose,
      location = location,
      location_value = location_value,
      tile_url = tile_url
    )
  } else {
    map_obj <- create_static_preview(
      raster_data = raster_data,
      map_title = map_title,
      color_palette = color_palette,
      verbose = verbose,
      location = location
    )
  }
  
  # ====================
  # DISPLAY AND SAVE
  # ====================
  
  if (verbose) {
    cat("\n[5/5] Finalizing preview...\n")
  }
  
  if (display) {
    if (viewer == "leaflet") {
      print(map_obj)
    }
  }
  
  if (!is.null(save_path)) {
    if (viewer == "leaflet") {
      if (requireNamespace("htmlwidgets", quietly = TRUE)) {
        htmlwidgets::saveWidget(map_obj, file = save_path)
        if (verbose) {
          cat("  Map saved to:", save_path, "\n")
        }
      } else {
        warning("Package 'htmlwidgets' required to save leaflet maps", call. = FALSE)
      }
    } else {
      # Save static plot as PNG
      png(save_path, width = width, height = height)
      create_static_preview(raster_data, map_title, color_palette, verbose = FALSE,
                            location = location)
      dev.off()
      if (verbose) {
        cat("  Plot saved to:", save_path, "\n")
      }
    }
  }
  
  if (verbose) {
    cat("\n[OK] Preview complete!\n\n")
    cat("Data info:\n")
    cat("  Source:", data_source, "\n")
    cat("  Dimensions:", paste(dim(raster_data)[1:2], collapse = " x "), "pixels\n")
    cat("  Extent:", paste(round(as.vector(terra::ext(raster_data)), 2), collapse = ", "), "\n")
    cat("  CRS:", as.character(terra::crs(raster_data)), "\n")
  }
  
  # Return based on return_data flag
  if (return_data) {
    return(list(
      map = map_obj,
      data = value_data,
      image = image_data,
      source = data_source,
      extent = extent_label,
      variable = var_info$variable_name,
      crop = crop_desc,
      time_period = time_desc,
      location_value = location_value
    ))
  } else {
    if (viewer == "leaflet") {
      return(invisible(map_obj))
    } else {
      return(invisible(NULL))
    }
  }
}


#' Create leaflet interactive map preview
#'
#' @param raster_data terra SpatRaster object
#' @param map_title Character - Title for map legend
#' @param color_palette Character - Color palette name
#' @param na_color Character - Color for NA values
#' @param opacity Numeric - Layer opacity
#' @param verbose Logical - Print progress messages
#' @param location Optional c(lon, lat) to mark
#' @param location_value Value shown in the marker popup
#' @param tile_url Optional WMTS URL template added as an overlay
#'
#' @return leaflet map object
#'
#' @keywords internal
#' @noRd
#' @importFrom leaflet leaflet addTiles addProviderTiles addRasterImage addLegend addLayersControl leafletOptions providers colorNumeric
create_leaflet_preview <- function(raster_data, map_title, color_palette,
                                    na_color, opacity, verbose,
                                    location = NULL, location_value = NULL,
                                    tile_url = NULL) {

  map <- leaflet::leaflet() |>
    leaflet::addTiles(group = "OpenStreetMap") |>
    leaflet::addProviderTiles(
      leaflet::providers$Esri.WorldImagery,
      group = "Satellite"
    )

  if (terra::has.RGB(raster_data)) {
    # Server-rendered image: colours come from the FAO style
    map <- map |>
      leaflet::addRasterImage(raster_data, opacity = opacity, group = "GAEZ Data") |>
      leaflet::addControl(
        html = paste0("<b>", map_title, "</b><br><small>Rendered by the FAO GAEZ v5 WMTS</small>"),
        position = "bottomright"
      )
  } else {
    raster_values <- terra::values(raster_data, mat = FALSE)
    valid_values <- raster_values[!is.na(raster_values)]

    # Negative values in GAEZ layers are no-data codes; zeros are valid
    valid_values <- valid_values[valid_values >= 0]

    if (length(valid_values) == 0) {
      stop("No valid data values in extent. The region may have no data coverage.\n",
           "  This could mean:\n",
           "  - The crop is not grown in this region\n",
           "  - Data is not available for this crop/scenario combination\n",
           "  - Try a different crop, time period, or region",
           call. = FALSE)
    }

    if (all(valid_values == 0)) {
      warning("All data values are zero in this extent. ",
              "The crop may have no suitable cultivation area in this region.",
              call. = FALSE)
      valid_values <- c(0, 1)
    }

    pal <- leaflet::colorNumeric(
      palette = color_palette,
      domain = range(valid_values, na.rm = TRUE),
      na.color = na_color
    )

    map <- map |>
      leaflet::addRasterImage(raster_data, colors = pal, opacity = opacity, group = "GAEZ Data") |>
      leaflet::addLegend(pal = pal, values = valid_values, title = map_title,
                         position = "bottomright")
  }

  overlays <- "GAEZ Data"
  if (!is.null(tile_url)) {
    map <- leaflet::addTiles(
      map,
      urlTemplate = tile_url,
      group = "FAO WMTS",
      attribution = "FAO & IIASA, GAEZ v5 (CC BY 4.0)",
      options = leaflet::tileOptions(opacity = opacity)
    )
    overlays <- c(overlays, "FAO WMTS")
  }
  if (!is.null(location)) {
    popup <- sprintf("lon %.4f, lat %.4f<br>value: %s", location[1], location[2],
                     format(location_value))
    map <- leaflet::addMarkers(map, lng = location[1], lat = location[2], popup = popup)
  }

  map <- leaflet::addLayersControl(
    map,
    baseGroups = c("OpenStreetMap", "Satellite"),
    overlayGroups = overlays,
    options = leaflet::layersControlOptions(collapsed = FALSE)
  )

  if (verbose) {
    cat("  Interactive leaflet map created\n")
  }

  map
}


#' Create static plot preview
#'
#' @param raster_data terra SpatRaster object
#' @param map_title Character - Plot title
#' @param color_palette Character - Color palette name
#' @param verbose Logical - Print progress messages
#' @param location Optional c(lon, lat) to mark
#'
#' @return NULL (invisibly)
#'
#' @keywords internal
#' @noRd
create_static_preview <- function(raster_data, map_title, color_palette, verbose,
                                  location = NULL) {
  
  if (terra::has.RGB(raster_data)) {
    terra::plotRGB(raster_data, r = 1, g = 2, b = 3, a = 4, axes = TRUE, main = map_title)
  } else {
    terra::plot(
      raster_data,
      main = map_title,
      col = hcl.colors(100, color_palette),
      axes = TRUE
    )
  }
  if (!is.null(location)) {
    terra::points(terra::vect(matrix(location, ncol = 2), crs = "EPSG:4326"),
                  pch = 3, cex = 1.5, lwd = 2)
  }
  
  if (verbose) {
    cat("  Static plot created\n")
  }
  
  return(invisible(NULL))
}


#' GDAL settings for reading remote Cloud-Optimized GeoTIFFs
#' @noRd
gdal_http_config <- c(
  GDAL_DISABLE_READDIR_ON_OPEN = "EMPTY_DIR",
  CPL_VSIL_CURL_ALLOWED_EXTENSIONS = ".tif",
  GDAL_HTTP_MULTIRANGE = "YES",
  GDAL_HTTP_MERGE_CONSECUTIVE_RANGES = "YES",
  GDAL_HTTP_MAX_RETRY = "3",
  GDAL_HTTP_RETRY_DELAY = "1"
)


#' Evaluate `expr` with gdal_http_config set, restoring previous values on exit
#' @noRd
with_gdal_http <- function(expr) {
  previous <- terra::getGDALconfig(names(gdal_http_config))
  terra::setGDALconfig(names(gdal_http_config), unname(gdal_http_config))
  on.exit(terra::setGDALconfig(names(previous), unname(previous)), add = TRUE)
  force(expr)
}


#' Coarsest COG overview level that keeps at least `width` pixels across `span`
#'
#' Overview k has span / 2^(k + 1) pixels. Returns -1 (full resolution) when
#' the span is already narrower than about twice the target width.
#' @noRd
overview_level <- function(span, width) {
  max(-1L, as.integer(floor(log2(span / width)) - 1L))
}


#' Read a reduced-resolution window of a remote GAEZ GeoTIFF into memory
#'
#' @param url HTTPS URL of a Cloud-Optimized GeoTIFF
#' @param bbox Optional c(xmin, ymin, xmax, ymax) in the raster's CRS
#' @param width Target number of pixels across the extent
#' @param boundary Optional SpatVector used to mask the result
#' @noRd
read_gaez_preview <- function(url, bbox = NULL, width = 800, boundary = NULL) {
  src <- paste0("/vsicurl/", url)

  with_gdal_http({
    full <- terra::rast(src)
    span <- if (is.null(bbox)) {
      terra::ncol(full)
    } else {
      ceiling((bbox[3] - bbox[1]) / terra::xres(full))
    }

    x <- full
    level <- overview_level(span, width)
    while (level >= 0) {
      x <- tryCatch(
        terra::rast(src, opts = paste0("OVERVIEW_LEVEL=", level)),
        error = function(e) NULL
      )
      if (!is.null(x)) break
      level <- level - 1L
    }
    if (is.null(x)) x <- full

    if (!is.null(bbox)) {
      x <- terra::crop(x, terra::ext(bbox[1], bbox[3], bbox[2], bbox[4]))
    }
    if (!is.null(boundary)) {
      if (!terra::same.crs(boundary, x)) boundary <- terra::project(boundary, terra::crs(x))
      x <- terra::mask(x, boundary)
    }
    terra::rast(x, vals = terra::values(x))
  })
}


#' Full-resolution value of a remote GAEZ GeoTIFF at one point
#' @noRd
read_gaez_point <- function(url, lon, lat) {
  with_gdal_http({
    x <- terra::rast(paste0("/vsicurl/", url))
    terra::extract(x, matrix(c(lon, lat), ncol = 2))[[1]]
  })
}


#' Window of +/- radius_km around a c(lon, lat) point, clipped to the globe
#' @noRd
location_bbox <- function(location, radius_km) {
  if (!is.numeric(location) || length(location) != 2 || anyNA(location) ||
      abs(location[1]) > 180 || abs(location[2]) > 90) {
    stop("location must be c(lon, lat) in WGS84 degrees", call. = FALSE)
  }
  if (!is.numeric(radius_km) || length(radius_km) != 1 || is.na(radius_km) || radius_km <= 0) {
    stop("radius_km must be a positive number", call. = FALSE)
  }
  dlat <- radius_km / 111.32
  dlon <- radius_km / (111.32 * max(cos(location[2] * pi / 180), 0.01))
  c(max(location[1] - dlon, -180), max(location[2] - dlat, -90),
    min(location[1] + dlon, 180), min(location[2] + dlat, 90))
}

