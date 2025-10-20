#' Preview GAEZ Data as Interactive Map
#'
#' Creates an interactive leaflet map preview of GAEZ v5 data without requiring
#' full dataset downloads. Attempts to use the GAEZ ImageServer REST API when
#' available, otherwise falls back to downloading and cropping the full dataset.
#' Supports both global and country-level previews.
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
#'   in WGS84 coordinates. Overridden by country if both are specified.
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
#'
#' @return Depends on viewer type:
#'   \itemize{
#'     \item "leaflet": A leaflet map object
#'     \item "static": Returns NULL invisibly (displays plot)
#'   }
#'   If return_data = TRUE, returns a list with map and raster data.
#'
#' @details
#' ## Preview Strategy
#'
#' The function uses a two-tier strategy:
#' 1. **REST API** (fast, no download): For all themes (1-6) when available
#' 2. **Download fallback** (slower, requires disk space): When REST API fails
#'
#' ## Geographic Extents
#'
#' Three ways to specify extent:
#' 1. **Global** (default): Shows entire world coverage
#' 2. **Country**: Automatically retrieves boundary and crops to extent
#' 3. **Custom bbox**: Manually specify rectangular bounds
#'
#' Priority: country > bbox > global
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
                              opacity = 0.7) {
  
  # Match viewer argument
  viewer <- match.arg(viewer)
  
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
  
  # Look up variable information
  var_info <- lookup_gaez_variable(variable)

  # Look up crop code (only for themes 3-6 which have crop-specific data)
  crop_code <- NULL
  if (var_info$theme_number %in% c(3, 4, 5, 6)) {
    crop_code <- lookup_gaez_crop(crop, var_info$theme_number, interactive = interactive)
  } else {
    # Themes 1 & 2 don't use crop codes
    crop_code <- NA_character_
  }
  
  # Handle time period lookup if years provided
  time_period <- lookup_time_period_from_years(
    start_year = start_year,
    end_year = end_year,
    time_period = time_period,
    interactive = interactive,
    verbose = verbose
  )
  
  # Validate and adjust climate model and SSP
  validation_result <- validate_climate_ssp(time_period, climate_model, ssp)
  climate_model <- validation_result$climate_model
  ssp <- validation_result$ssp
  
  if (verbose) {
    cat("  Variable:", var_info$variable_name, "\n")
    cat("  Crop:", crop_code, "\n")
    cat("  Time period:", time_period, "\n")
    cat("  Climate model:", climate_model, "\n")
    cat("  SSP:", ssp, "\n")
  }
  
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
    # but we need c(xmin, ymin, xmax, ymax) for ImageServer
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
  # ATTEMPT REST API PREVIEW
  # ====================
  
  if (verbose) {
    cat("\n[3/5] Loading data...\n")
  }
  
  theme_number <- var_info$theme_number
  variable_code <- var_info$variable_code
  use_rest_api <- check_imageserver_availability(theme_number = theme_number,
                                                   variable_code = variable_code)
  raster_data <- NULL

  if (use_rest_api) {
    if (verbose) {
      cat("  Attempting REST API preview...\n")
    }

    # Pass both theme_number and variable_code for accurate mapping
    export_url <- build_imageserver_url(theme_number = theme_number,
                                        variable_code = variable_code)

    if (!is.na(export_url)) {
      # Map GAEZ parameters to ImageServer format
      crop_name <- map_crop_code_to_name(crop_code, theme_number)
      year_range <- map_time_period_to_year(time_period)
      model_name <- map_climate_model_to_imageserver(climate_model, time_period)
      rcp_value <- map_ssp_to_rcp(ssp)
      water_supply_name <- map_water_supply_to_imageserver(water_supply)
      input_level_name <- map_input_level_to_imageserver(water_management_level)

      if (verbose) {
        cat("  Mapped parameters for ImageServer:\n")
        if (!is.na(crop_name)) cat("    Crop:", crop_name, "\n")
        if (!is.na(year_range)) cat("    Year:", year_range, "\n")
        if (!is.na(model_name)) cat("    Model:", model_name, "\n")
        if (!is.na(rcp_value)) cat("    RCP:", rcp_value, "\n")
        if (!is.na(water_supply_name)) cat("    Water supply:", water_supply_name, "\n")
        if (!is.na(input_level_name)) cat("    Input level:", input_level_name, "\n")
      }

      temp_file <- fetch_imageserver_tile(
        export_url = export_url,
        bbox = bbox_coords,
        width = width,
        height = height,
        crop_name = crop_name,
        year_range = year_range,
        model_name = model_name,
        rcp = rcp_value,
        water_supply_name = water_supply_name,
        input_level_name = input_level_name,
        geometry = boundary_vector,  # Pass polygon for server-side clipping
        verbose = verbose
      )

      if (!is.null(temp_file) && file.exists(temp_file)) {
        # Load raster from REST API
        raster_data <- terra::rast(temp_file)

        # Ensure CRS is set (ImageServer should provide this, but be safe)
        if (is.na(terra::crs(raster_data)) || terra::crs(raster_data) == "") {
          terra::crs(raster_data) <- "EPSG:4326"
        }

        # Post-process: The ImageServer Clip function sets pixels outside the
        # polygon to 0 instead of NoData. We need to mask these boundary zeros
        # to avoid showing a box of zeros around the country.
        if (!is.null(boundary_vector)) {
          # Reproject boundary if needed
          if (!terra::same.crs(boundary_vector, raster_data)) {
            boundary_vector_proj <- terra::project(boundary_vector, terra::crs(raster_data))
          } else {
            boundary_vector_proj <- boundary_vector
          }

          # Mask: set values outside polygon to NA
          # This removes the zero-value box around the country
          raster_data <- terra::mask(raster_data, boundary_vector_proj)

          if (verbose) {
            cat("  Applied polygon mask to remove boundary artifacts\n")
          }
        }

        # Validate that we have some valid data
        raster_values <- terra::values(raster_data, mat = FALSE)
        valid_values <- raster_values[!is.na(raster_values) & raster_values > 0]

        if (length(valid_values) == 0) {
          if (verbose) {
            cat("  REST API returned no valid data for this extent, using download method...\n")
          }
          use_rest_api <- FALSE
          raster_data <- NULL
        } else {
          if (verbose) {
            cat("  Successfully loaded data via REST API\n")
          }
        }
      } else {
        if (verbose) {
          cat("  REST API preview failed, using download method...\n")
        }
        use_rest_api <- FALSE
      }
    } else {
      use_rest_api <- FALSE
    }
  }
  
  # ====================
  # FALLBACK: DOWNLOAD METHOD
  # ====================
  
  if (is.null(raster_data)) {
    if (verbose) {
      cat("  Using download method...\n")
    }
    
    result <- load_gaez_data(
      variable = variable,
      time_period = time_period,
      climate_model = climate_model,
      ssp = ssp,
      crop = crop_code,  # Use already-resolved crop code to avoid duplicate lookup
      water_management_level = water_management_level,
      water_supply = water_supply,
      country = if (!is.null(country)) country else NULL,
      return_metadata = TRUE,
      verbose = verbose,
      interactive = interactive
    )
    
    raster_data <- result$raster
    
    # Crop to bbox if provided and country is NULL
    if (!is.null(bbox) && is.null(country)) {
      # terra::ext() expects (xmin, xmax, ymin, ymax)
      # bbox_coords is (xmin, ymin, xmax, ymax), so reorder
      ext_obj <- terra::ext(c(bbox_coords[1], bbox_coords[3], bbox_coords[2], bbox_coords[4]))
      raster_data <- terra::crop(raster_data, ext_obj)
    }
  }
  
  # ====================
  # CREATE VISUALIZATION
  # ====================
  
  if (verbose) {
    cat("\n[4/5] Creating visualization...\n")
  }
  
  # Get descriptive labels
  crop_desc <- gaez_crops |>
    dplyr::filter(gaez_crop_code == crop_code, gaez_theme == theme_number) |>
    dplyr::pull(name) |>
    dplyr::first()
  if (is.null(crop_desc) || length(crop_desc) == 0) crop_desc <- crop_code
  
  time_desc <- gaez_scenarios |>
    dplyr::filter(time_period == !!time_period) |>
    dplyr::pull(description) |>
    dplyr::first()
  if (is.null(time_desc) || length(time_desc) == 0) time_desc <- time_period
  
  map_title <- paste0(crop_desc, " - ", time_desc)
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
      verbose = verbose
    )
  } else {
    map_obj <- create_static_preview(
      raster_data = raster_data,
      map_title = map_title,
      color_palette = color_palette,
      verbose = verbose
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
      terra::plot(raster_data, main = map_title, col = hcl.colors(100, color_palette))
      dev.off()
      if (verbose) {
        cat("  Plot saved to:", save_path, "\n")
      }
    }
  }
  
  if (verbose) {
    cat("\n✓ Preview complete!\n\n")
    cat("Data info:\n")
    cat("  Dimensions:", paste(dim(raster_data)[1:2], collapse = " x "), "pixels\n")
    cat("  Extent:", paste(round(as.vector(terra::ext(raster_data)), 2), collapse = ", "), "\n")
    cat("  CRS:", as.character(terra::crs(raster_data)), "\n")
  }
  
  # Return based on return_data flag
  if (return_data) {
    return(list(
      map = map_obj,
      data = raster_data,
      extent = extent_label,
      variable = var_info$variable_name,
      crop = crop_desc,
      time_period = time_desc
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
#'
#' @return leaflet map object
#'
#' @keywords internal
#' @noRd
#' @importFrom leaflet leaflet addTiles addProviderTiles addRasterImage addLegend addLayersControl leafletOptions providers colorNumeric
create_leaflet_preview <- function(raster_data, map_title, color_palette,
                                    na_color, opacity, verbose) {
  
  # Get valid values for color scale
  raster_values <- terra::values(raster_data, mat = FALSE)
  valid_values <- raster_values[!is.na(raster_values)]

  # Filter out nodata values (negative numbers in GAEZ often indicate nodata)
  # But keep zeros as they represent valid data (zero yield)
  valid_values <- valid_values[valid_values >= 0]

  if (length(valid_values) == 0) {
    stop("No valid data values in extent. The region may have no data coverage.\n",
         "  This could mean:\n",
         "  - The crop is not grown in this region\n",
         "  - Data is not available for this crop/scenario combination\n",
         "  - Try a different crop, time period, or region",
         call. = FALSE)
  }

  # Check if all values are zero
  if (all(valid_values == 0)) {
    warning("All data values are zero in this extent. ",
            "The crop may have no suitable cultivation area in this region.",
            call. = FALSE)
    # Use a small range for visualization
    valid_values <- c(0, 1)
  }
  
  # Create color palette
  pal <- leaflet::colorNumeric(
    palette = color_palette,
    domain = range(valid_values, na.rm = TRUE),
    na.color = na_color
  )
  
  # Convert terra raster to raster package (leaflet compatibility)
  if (requireNamespace("raster", quietly = TRUE)) {
    raster_legacy <- raster::raster(raster_data)
  } else {
    stop("Package 'raster' is required for leaflet integration. ",
         "Install with: install.packages('raster')",
         call. = FALSE)
  }
  
  # Create leaflet map
  map <- leaflet::leaflet() |>
    leaflet::addTiles(group = "OpenStreetMap") |>
    leaflet::addProviderTiles(
      leaflet::providers$Esri.WorldImagery,
      group = "Satellite"
    ) |>
    leaflet::addRasterImage(
      raster_legacy,
      colors = pal,
      opacity = opacity,
      group = "GAEZ Data"
    ) |>
    leaflet::addLegend(
      pal = pal,
      values = valid_values,
      title = map_title,
      position = "bottomright"
    ) |>
    leaflet::addLayersControl(
      baseGroups = c("OpenStreetMap", "Satellite"),
      overlayGroups = "GAEZ Data",
      options = leaflet::layersControlOptions(collapsed = FALSE)
    )
  
  if (verbose) {
    cat("  Interactive leaflet map created\n")
  }
  
  return(map)
}


#' Create static plot preview
#'
#' @param raster_data terra SpatRaster object
#' @param map_title Character - Plot title
#' @param color_palette Character - Color palette name
#' @param verbose Logical - Print progress messages
#'
#' @return NULL (invisibly)
#'
#' @keywords internal
#' @noRd
create_static_preview <- function(raster_data, map_title, color_palette, verbose) {
  
  terra::plot(
    raster_data,
    main = map_title,
    col = hcl.colors(100, color_palette),
    axes = TRUE
  )
  
  if (verbose) {
    cat("  Static plot created\n")
  }
  
  return(invisible(NULL))
}
