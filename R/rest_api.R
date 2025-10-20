#' Build ImageServer REST API URL for GAEZ data
#'
#' Constructs the export image URL for GAEZ ImageServer REST API. Maps GAEZ
#' theme numbers OR variable codes to appropriate ImageServer service endpoints.
#' Variable code-based mapping is preferred as it's more reliable.
#'
#' @param theme_number Numeric - GAEZ theme number (1-6), optional
#' @param variable_code Character - Variable code (e.g., "RES05-YX"), optional
#'
#' @return Character string with ImageServer exportImage endpoint URL, or NA
#'   if mapping fails
#'
#' @details
#' ## GAEZ ImageServer REST API Mapping
#'
#' The ImageServer uses different service names than theme numbers:
#'
#' **By Variable Code Prefix** (recommended):
#' \itemize{
#'   \item \code{RES01-*} → res01 (Theme 2: Agro-climatic Resources)
#'   \item \code{RES02-*} → res02 (Theme 3: Agro-climatic Potential Yield)
#'   \item \code{RES05-*} → res05 (Theme 4: Suitability & Attainable Yield)
#'   \item \code{RES06-*} → res06 (Theme 5: Actual Yields & Production)
#'   \item \code{RES07-*} → res07 (Theme 6: Yield & Production Gaps)
#'   \item No prefix → res01 (Theme 1: Land & Water Resources)
#' }
#'
#' **By Theme Number**:
#' \itemize{
#'   \item Theme 1 → res01 (Land & Water Resources)
#'   \item Theme 2 → res01 (Agro-climatic Resources - shares with Theme 1)
#'   \item Theme 3 → res02 (Agro-climatic Potential Yield)
#'   \item Theme 4 → res05 (Suitability & Attainable Yield)
#'   \item Theme 5 → res06 (Actual Yields & Production)
#'   \item Theme 6 → res07 (Yield & Production Gaps)
#' }
#'
#' **Note**: All themes are available via REST API. Variable code-based
#' mapping is more reliable as the RES prefix directly indicates the service.
#'
#' @keywords internal
#' @noRd
build_imageserver_url <- function(theme_number = NULL, variable_code = NULL) {
  # Base URL for GAEZ ImageServer
  base_url <- "https://gaez-services.fao.org/server/rest/services"

  service_name <- NA_character_

  # Prefer variable code-based mapping (more reliable)
  if (!is.null(variable_code) && !is.na(variable_code)) {
    # Check if variable starts with RES0X pattern
    if (grepl("^RES0[0-9]", variable_code)) {
      # Extract the number from RES0X (e.g., "RES05" -> "05")
      prefix_num <- substr(variable_code, 4, 5)
      service_name <- paste0("res", prefix_num)
    } else {
      # Theme 1 variables without RES prefix (e.g., "AEZ57", "LR-LCC")
      service_name <- "res01"
    }
  }

  # Fallback to theme number-based mapping
  if (is.na(service_name) && !is.null(theme_number) && !is.na(theme_number)) {
    # Map theme number to service name
    theme_map <- c(
      "1" = "res01",  # Land & Water Resources
      "2" = "res01",  # Agro-climatic Resources (shares res01 with Theme 1)
      "3" = "res02",  # Agro-climatic Potential Yield
      "4" = "res05",  # Suitability & Attainable Yield
      "5" = "res06",  # Actual Yields & Production
      "6" = "res07"   # Yield & Production Gaps
    )

    service_name <- theme_map[as.character(theme_number)]
  }

  # Return NA if no valid mapping found
  if (is.na(service_name)) {
    return(NA_character_)
  }

  paste0(base_url, "/", service_name, "/ImageServer/exportImage")
}


#' Check if ImageServer REST API is available for dataset
#'
#' Determines if GAEZ data can be accessed via ImageServer REST API. As of
#' 2025-10-17, ALL themes (1-6) have REST API access via different ImageServer
#' endpoints (res01, res02, res05, res06, res07).
#'
#' @param theme_number Numeric - GAEZ theme number (1-6), optional
#' @param variable_code Character - Variable code (e.g., "RES05-YX"), optional
#'
#' @return Logical indicating if REST API access is available
#'
#' @details
#' All GAEZ v5 themes are now accessible via ImageServer REST API:
#' \itemize{
#'   \item Theme 1: res01 (Land & Water Resources)
#'   \item Theme 2: res01 (Agro-climatic Resources)
#'   \item Theme 3: res02 (Agro-climatic Potential Yield)
#'   \item Theme 4: res05 (Suitability & Attainable Yield)
#'   \item Theme 5: res06 (Actual Yields & Production)
#'   \item Theme 6: res07 (Yield & Production Gaps)
#' }
#'
#' @keywords internal
#' @noRd
check_imageserver_availability <- function(theme_number = NULL, variable_code = NULL) {
  # All themes 1-6 have ImageServer access via different endpoints
  # If theme_number is provided, check if it's valid
  if (!is.null(theme_number)) {
    return(theme_number %in% c(1, 2, 3, 4, 5, 6))
  }

  # If variable_code is provided, check if we can map it
  if (!is.null(variable_code)) {
    # All RES0X variables are supported
    if (grepl("^RES0[1-7]", variable_code)) {
      return(TRUE)
    }
    # Theme 1 variables without RES prefix are also supported
    return(TRUE)
  }

  # Default to FALSE if neither parameter provided
  return(FALSE)
}


#' Fetch image from GAEZ ImageServer REST API
#'
#' Makes an exportImage request to the GAEZ ImageServer REST API and downloads
#' the resulting GeoTIFF. Handles the two-step process: (1) request image export,
#' (2) download from returned URL. Uses mosaicRule to select specific layers
#' based on crop, time period, climate model, and scenario.
#'
#' @param export_url Character - ImageServer exportImage endpoint URL
#' @param bbox Numeric vector - Bounding box as c(xmin, ymin, xmax, ymax) in WGS84
#' @param width Numeric - Image width in pixels
#' @param height Numeric - Image height in pixels
#' @param crop_name Character - Full crop name for ImageServer (e.g., "Pearl millet")
#' @param year_range Character - Time period as year range (e.g., "1981-2010")
#' @param model_name Character - Climate model name (e.g., "CRUTS32", "ENSEMBLE")
#' @param rcp Character - RCP/scenario (e.g., "Historical", "RCP8.5")
#' @param water_supply_name Character - Water supply (e.g., "Rainfed")
#' @param input_level_name Character - Input level (e.g., "High", "Low")
#' @param geometry terra::SpatVector - Optional polygon geometry for server-side
#'   clipping. If provided, the ImageServer will clip the image to this exact
#'   polygon instead of the rectangular bbox. This is more efficient for country
#'   boundaries as it reduces bandwidth and eliminates data outside borders.
#' @param verbose Logical - Whether to print progress messages
#'
#' @return Character path to downloaded temporary GeoTIFF file, or NULL on failure
#'
#' @details
#' The function uses GeoTIFF format to preserve spatial reference information.
#' The image is downloaded to a temporary file that must be loaded separately.
#'
#' Uses mosaicRule with WHERE clause to filter layers by crop, time period,
#' climate model, RCP, water supply, and input level. This ensures the correct
#' data layer is returned instead of a default layer.
#'
#' @keywords internal
#' @noRd
#' @importFrom httr GET status_code content
fetch_imageserver_tile <- function(export_url, bbox, width, height,
                                   crop_name = NULL,
                                   year_range = NULL,
                                   model_name = NULL,
                                   rcp = NULL,
                                   water_supply_name = NULL,
                                   input_level_name = NULL,
                                   geometry = NULL,
                                   verbose = TRUE) {
  # Check if httr is available
  if (!requireNamespace("httr", quietly = TRUE)) {
    stop("Package 'httr' is required. Install with: install.packages('httr')",
         call. = FALSE)
  }

  # Build mosaicRule WHERE clause to filter layers
  where_conditions <- c()

  if (!is.null(crop_name) && !is.na(crop_name)) {
    where_conditions <- c(where_conditions, paste0("crop='", crop_name, "'"))
  }

  if (!is.null(year_range) && !is.na(year_range)) {
    where_conditions <- c(where_conditions, paste0("year='", year_range, "'"))
  }

  if (!is.null(model_name) && !is.na(model_name)) {
    where_conditions <- c(where_conditions, paste0("model='", model_name, "'"))
  }

  if (!is.null(rcp) && !is.na(rcp)) {
    where_conditions <- c(where_conditions, paste0("rcp='", rcp, "'"))
  }

  if (!is.null(water_supply_name) && !is.na(water_supply_name)) {
    where_conditions <- c(where_conditions, paste0("water_supply='", water_supply_name, "'"))
  }

  if (!is.null(input_level_name) && !is.na(input_level_name)) {
    where_conditions <- c(where_conditions, paste0("input_level='", input_level_name, "'"))
  }

  # Build request parameters
  params <- list(
    bbox = paste(bbox, collapse = ","),
    bboxSR = 4326,  # WGS84
    size = paste0(width, ",", height),
    imageSR = 4326,
    format = "tiff",
    pixelType = "S16",
    interpolation = "RSP_NearestNeighbor",
    f = "json"
  )

  # Add renderingRule with Clip function for server-side polygon clipping
  # This masks pixels outside the polygon to NoData
  if (!is.null(geometry)) {
    geometry_json <- spatvector_to_esri_json(geometry)

    if (!is.null(geometry_json)) {
      # Parse the geometry JSON to get the structure
      if (requireNamespace("jsonlite", quietly = TRUE)) {
        geom_obj <- jsonlite::fromJSON(geometry_json)

        # Build Clip raster function for renderingRule
        # ClippingType: 1 = clip to inside of polygon (keep inside, NoData outside)
        # "Raster": "$$" is a special token meaning "use the entire image service"
        rendering_rule <- list(
          rasterFunction = "Clip",
          rasterFunctionArguments = list(
            ClippingGeometry = geom_obj,
            ClippingType = 1,
            Raster = "$$"
          )
        )

        params$renderingRule <- jsonlite::toJSON(rendering_rule, auto_unbox = TRUE)

        if (verbose) {
          cat("  Using Clip raster function for server-side polygon masking\n")
        }
      } else {
        warning("Package 'jsonlite' required for renderingRule. Using bbox only.",
                call. = FALSE)
      }
    }
  }

  # Add mosaicRule if we have filter conditions
  if (length(where_conditions) > 0) {
    where_clause <- paste(where_conditions, collapse = " AND ")

    if (verbose) {
      cat("  Using layer filter:", where_clause, "\n")
    }

    # Build mosaicRule JSON
    # Using esriMosaicAttribute method with WHERE clause to filter layers
    mosaic_rule <- list(
      mosaicMethod = "esriMosaicAttribute",
      where = where_clause
    )

    # Convert to JSON string
    if (requireNamespace("jsonlite", quietly = TRUE)) {
      params$mosaicRule <- jsonlite::toJSON(mosaic_rule, auto_unbox = TRUE)
    } else {
      # Fallback: build JSON manually if jsonlite not available
      params$mosaicRule <- sprintf(
        '{"mosaicMethod":"esriMosaicAttribute","where":"%s"}',
        where_clause
      )
    }
  }
  
  if (verbose) {
    cat("Querying ImageServer API...\n")
  }

  # Make API request
  # Use POST if renderingRule or mosaicRule is present (can be large)
  # Use GET otherwise (faster for simple requests)
  response <- tryCatch({
    if (!is.null(params$renderingRule) || !is.null(params$mosaicRule)) {
      # POST request for complex queries with rendering/mosaic rules
      httr::POST(export_url, body = params, encode = "form")
    } else {
      # GET request for simple bbox-only queries
      httr::GET(export_url, query = params)
    }
  }, error = function(e) {
    warning("ImageServer API request failed: ", e$message, call. = FALSE)
    return(NULL)
  })
  
  if (is.null(response)) {
    return(NULL)
  }
  
  # Check response status
  if (httr::status_code(response) != 200) {
    warning("ImageServer API returned status ", httr::status_code(response),
            call. = FALSE)
    return(NULL)
  }
  
  # Parse JSON response
  result <- tryCatch({
    httr::content(response, as = "parsed")
  }, error = function(e) {
    warning("Failed to parse ImageServer response: ", e$message, call. = FALSE)
    return(NULL)
  })
  
  if (is.null(result) || is.null(result$href)) {
    warning("No image URL found in ImageServer response", call. = FALSE)
    return(NULL)
  }
  
  image_url <- result$href
  
  if (verbose) {
    cat("Downloading image from server...\n")
  }
  
  # Download the image
  img_response <- tryCatch({
    httr::GET(image_url)
  }, error = function(e) {
    warning("Failed to download image: ", e$message, call. = FALSE)
    return(NULL)
  })
  
  if (is.null(img_response) || httr::status_code(img_response) != 200) {
    warning("Image download failed", call. = FALSE)
    return(NULL)
  }
  
  # Save to temporary file
  temp_file <- tempfile(fileext = ".tif")
  writeBin(httr::content(img_response, "raw"), temp_file)
  
  if (verbose) {
    cat("Image downloaded successfully\n")
  }
  
  return(temp_file)
}


#' Parse and validate ImageServer response
#'
#' Validates the JSON response from ImageServer exportImage request and extracts
#' the image download URL.
#'
#' @param response httr response object
#'
#' @return Character URL for image download, or NULL if invalid
#'
#' @keywords internal
#' @noRd
#' @importFrom httr content
parse_imageserver_response <- function(response) {
  if (!requireNamespace("httr", quietly = TRUE)) {
    return(NULL)
  }
  
  result <- tryCatch({
    httr::content(response, as = "parsed")
  }, error = function(e) {
    return(NULL)
  })
  
  if (is.null(result) || is.null(result$href)) {
    return(NULL)
  }
  
  result$href
}


#' Map GAEZ crop code to ImageServer crop name
#'
#' Converts GAEZ crop codes (e.g., "PML") to full crop names used by ImageServer
#' (e.g., "Pearl millet").
#'
#' @param crop_code Character - GAEZ crop code (e.g., "PML", "SRG", "WHEA")
#' @param theme_number Numeric - GAEZ theme number for crop lookup
#'
#' @return Character crop name for ImageServer, or NA if not found
#'
#' @keywords internal
#' @noRd
map_crop_code_to_name <- function(crop_code, theme_number) {
  if (is.null(crop_code) || is.na(crop_code)) {
    return(NA_character_)
  }

  # Look up crop name from gaez_crops data
  crop_info <- gaez_crops |>
    dplyr::filter(gaez_crop_code == crop_code, gaez_theme == theme_number) |>
    dplyr::pull(name) |>
    dplyr::first()

  if (is.null(crop_info) || length(crop_info) == 0) {
    return(NA_character_)
  }

  return(crop_info)
}


#' Map GAEZ time period to ImageServer year range
#'
#' Converts GAEZ time period codes (e.g., "HP0120") to year ranges used by
#' ImageServer (e.g., "1981-2010"). Note that ImageServer periods may not
#' exactly match GAEZ periods.
#'
#' @param time_period Character - GAEZ time period code (e.g., "HP0120", "FP4160")
#'
#' @return Character year range for ImageServer, or NA if cannot map
#'
#' @details
#' ImageServer available periods (from API query):
#' - Historical: "1961-1990", "1971-2000", "1981-2010"
#' - Future: "2011-2040", "2041-2070", "2071-2100"
#'
#' GAEZ periods:
#' - HP8100: 1981-2000 → maps to "1981-2010" (closest match)
#' - HP0120: 2001-2020 → maps to "1981-2010" (latest historical available)
#' - FP2140: 2021-2040 → maps to "2011-2040"
#' - FP4160: 2041-2060 → maps to "2041-2070"
#' - FP6180: 2061-2080 → maps to "2071-2100"
#'
#' @keywords internal
#' @noRd
map_time_period_to_year <- function(time_period) {
  if (is.null(time_period) || is.na(time_period)) {
    return(NA_character_)
  }

  # Get year range from gaez_scenarios
  period_info <- gaez_scenarios |>
    dplyr::filter(time_period == !!time_period) |>
    dplyr::select(start_year, end_year) |>
    dplyr::first()

  if (is.null(period_info) || nrow(period_info) == 0) {
    return(NA_character_)
  }

  start_year <- period_info$start_year
  end_year <- period_info$end_year

  # Map to ImageServer periods based on midpoint or range overlap
  # Historical periods
  if (start_year <= 2020) {
    # HP8100 (1981-2000) and HP0120 (2001-2020) both map to latest historical
    return("1981-2010")
  }

  # Future periods
  if (start_year >= 2021 && start_year < 2041) {
    return("2011-2040")
  } else if (start_year >= 2041 && start_year < 2071) {
    return("2041-2070")
  } else if (start_year >= 2071) {
    return("2071-2100")
  }

  return(NA_character_)
}


#' Map GAEZ climate model to ImageServer model name
#'
#' Converts GAEZ climate model codes to ImageServer model names.
#'
#' @param climate_model Character - GAEZ climate model (e.g., "AGERA5", "ENSEMBLE")
#' @param time_period Character - Time period to determine if historical/future
#'
#' @return Character climate model name for ImageServer
#'
#' @details
#' ImageServer uses:
#' - Historical: "CRUTS32" (not "AGERA5")
#' - Future: "ENSEMBLE", "GFDL-ESM2M", "HadGEM2-ES", "IPSL-CM5A-LR",
#'   "MIROC-ESM-CHEM", "NorESM1-M"
#'
#' Note: GAEZ v5 uses different model names than ImageServer in some cases.
#'
#' @keywords internal
#' @noRd
map_climate_model_to_imageserver <- function(climate_model, time_period = NULL) {
  if (is.null(climate_model) || is.na(climate_model)) {
    return(NA_character_)
  }

  # Check if historical period
  is_historical <- !is.null(time_period) && grepl("^HP", time_period)

  if (is_historical) {
    # Historical periods use CRUTS32 in ImageServer, not AGERA5
    return("CRUTS32")
  }

  # Future periods - most names match, but check for variations
  # ENSEMBLE stays ENSEMBLE
  # Individual models should match ImageServer names
  return(climate_model)
}


#' Map GAEZ SSP to ImageServer RCP
#'
#' Converts GAEZ SSP/scenario codes to ImageServer RCP values.
#'
#' @param ssp Character - GAEZ SSP code (e.g., "HIST", "SSP126", "SSP370")
#'
#' @return Character RCP value for ImageServer
#'
#' @details
#' Mapping:
#' - HIST → "Historical"
#' - SSP126 → "RCP2.6"
#' - SSP245 → "RCP4.5"
#' - SSP370 → "RCP6.0" (approximate) or "RCP8.5"
#' - SSP585 → "RCP8.5"
#'
#' Note: SSP-RCP mapping is approximate as they represent different frameworks.
#' ImageServer uses RCP 2.6, 4.5, 6.0, and 8.5.
#'
#' @keywords internal
#' @noRd
map_ssp_to_rcp <- function(ssp) {
  if (is.null(ssp) || is.na(ssp)) {
    return(NA_character_)
  }

  ssp_upper <- toupper(ssp)

  # Map SSP to RCP
  rcp_map <- c(
    "HIST" = "Historical",
    "SSP126" = "RCP2.6",
    "SSP245" = "RCP4.5",
    "SSP370" = "RCP8.5",  # Using RCP8.5 as closest match to SSP370
    "SSP585" = "RCP8.5"
  )

  rcp <- rcp_map[ssp_upper]

  if (is.null(rcp) || is.na(rcp)) {
    return(NA_character_)
  }

  return(as.character(rcp))
}


#' Map GAEZ water supply code to ImageServer water supply name
#'
#' Converts GAEZ water supply codes to ImageServer water supply names.
#'
#' @param water_supply Character - GAEZ water supply code (e.g., "WSR", "WSS")
#'
#' @return Character water supply name for ImageServer
#'
#' @details
#' Mapping:
#' - WSR → "Rainfed"
#' - WSS → "Sprinkler Irrigation"
#' - WSG → "Gravity Irrigation" (if available)
#'
#' @keywords internal
#' @noRd
map_water_supply_to_imageserver <- function(water_supply) {
  if (is.null(water_supply) || is.na(water_supply)) {
    return(NA_character_)
  }

  water_supply_upper <- toupper(water_supply)

  # Map water supply codes to names
  water_map <- c(
    "WSR" = "Rainfed",
    "WSS" = "Sprinkler Irrigation",
    "WSG" = "Gravity Irrigation"
  )

  water_name <- water_map[water_supply_upper]

  if (is.null(water_name) || is.na(water_name)) {
    return(NA_character_)
  }

  return(as.character(water_name))
}


#' Map GAEZ input level to ImageServer input level name
#'
#' Converts GAEZ input level codes to ImageServer input level names.
#'
#' @param input_level Character - GAEZ input level code (e.g., "HRLM", "LR", "IR")
#'
#' @return Character input level name for ImageServer
#'
#' @details
#' Mapping:
#' - HRLM or HR → "High"
#' - IR → "Intermediate"
#' - LR → "Low"
#'
#' @keywords internal
#' @noRd
map_input_level_to_imageserver <- function(input_level) {
  if (is.null(input_level) || is.na(input_level)) {
    return(NA_character_)
  }

  input_level_upper <- toupper(input_level)

  # Map input level codes to names
  input_map <- c(
    "HRLM" = "High",
    "HR" = "High",
    "IR" = "Intermediate",
    "LR" = "Low"
  )

  input_name <- input_map[input_level_upper]

  if (is.null(input_name) || is.na(input_name)) {
    return(NA_character_)
  }

  return(as.character(input_name))
}


#' Convert terra SpatVector polygon to Esri JSON geometry
#'
#' Converts a terra::SpatVector polygon object to Esri JSON format for use
#' with ArcGIS ImageServer REST API geometry parameter. Supports both single
#' polygons and multipolygons.
#'
#' @param spatvector A terra::SpatVector object containing polygon geometry
#'
#' @return Character string containing Esri JSON geometry, or NULL on error
#'
#' @details
#' Esri JSON geometry format for polygons:
#' ```
#' {
#'   "rings": [
#'     [[x1,y1], [x2,y2], ..., [x1,y1]]  // Outer ring (clockwise)
#'   ],
#'   "spatialReference": {"wkid": 4326}
#' }
#' ```
#'
#' The function:
#' - Extracts coordinates from terra::SpatVector
#' - Handles multipolygon countries (e.g., Philippines, Indonesia)
#' - Ensures rings are closed (first point = last point)
#' - Formats as Esri JSON with proper spatial reference
#'
#' For complex polygons with many vertices, the server will handle simplification
#' as needed.
#'
#' @keywords internal
#' @noRd
spatvector_to_esri_json <- function(spatvector) {
  if (is.null(spatvector) || !inherits(spatvector, "SpatVector")) {
    return(NULL)
  }

  # Ensure CRS is WGS84 (EPSG:4326)
  if (!terra::same.crs(spatvector, "EPSG:4326")) {
    spatvector <- terra::project(spatvector, "EPSG:4326")
  }

  tryCatch({
    # Get geometry data first to check vertex count
    # geom() returns: geom, part, x, y, hole
    geom_data <- terra::geom(spatvector)

    # Simplify polygon if it has too many vertices
    # This prevents URL length issues and improves performance
    num_vertices <- nrow(geom_data)
    if (num_vertices > 1000) {
      # Simplify to reduce vertex count
      # tolerance of 0.01 degrees (~1km at equator) is usually sufficient
      spatvector <- terra::simplifyGeom(spatvector, tolerance = 0.01)
      # Re-extract geometry after simplification
      geom_data <- terra::geom(spatvector)
    }

    # Group by geometry and part to build rings
    rings <- list()

    # Get unique geometry IDs
    geom_ids <- unique(geom_data[, "geom"])

    for (geom_id in geom_ids) {
      geom_rows <- geom_data[geom_data[, "geom"] == geom_id, , drop = FALSE]

      # Get unique parts for this geometry
      part_ids <- unique(geom_rows[, "part"])

      for (part_id in part_ids) {
        part_rows <- geom_rows[geom_rows[, "part"] == part_id, , drop = FALSE]

        # Extract x, y coordinates
        coords <- part_rows[, c("x", "y"), drop = FALSE]

        # Ensure ring is closed (first point = last point)
        if (nrow(coords) > 0) {
          if (coords[1, "x"] != coords[nrow(coords), "x"] ||
              coords[1, "y"] != coords[nrow(coords), "y"]) {
            # Close the ring
            coords <- rbind(coords, coords[1, , drop = FALSE])
          }

          # Convert to list of [x, y] pairs
          ring <- lapply(seq_len(nrow(coords)), function(i) {
            c(coords[i, "x"], coords[i, "y"])
          })

          rings <- c(rings, list(ring))
        }
      }
    }

    if (length(rings) == 0) {
      return(NULL)
    }

    # Build Esri JSON structure
    esri_json <- list(
      rings = rings,
      spatialReference = list(wkid = 4326)
    )

    # Convert to JSON string
    if (requireNamespace("jsonlite", quietly = TRUE)) {
      json_str <- jsonlite::toJSON(esri_json, auto_unbox = TRUE, digits = 8)
      return(as.character(json_str))
    } else {
      # Fallback: manual JSON construction
      # This is complex for rings, so just return NULL and use bbox
      warning("Package 'jsonlite' required for geometry parameter. Using bbox only.",
              call. = FALSE)
      return(NULL)
    }

  }, error = function(e) {
    warning("Failed to convert SpatVector to Esri JSON: ", e$message,
            ". Using bbox only.", call. = FALSE)
    return(NULL)
  })
}
