#' List available GAEZ crops
#'
#' Returns a table of available crops in the GAEZ v5 database. Different themes
#' use different crop coding systems, so specify the appropriate theme for your
#' analysis. Optionally filter by crop group (e.g., cereals, oil crops).
#'
#' @param crop_group Character - Filter by crop group name (optional).
#'   Case-insensitive partial matching. Examples: "cereal", "oil", "pulse",
#'   "fodder", "bioenergy".
#' @param theme Numeric - GAEZ theme number (3, 4, 5, or 6). Default is 4.
#'   \itemize{
#'     \item Theme 3: Agro-climatic potential yield (4-letter codes)
#'     \item Theme 4: Suitability & attainable yield (3-letter codes)
#'     \item Theme 5: Actual yields & production (3-letter codes)
#'     \item Theme 6: Yield & production gaps (3-letter codes)
#'   }
#'
#' @return A tibble with columns:
#'   \itemize{
#'     \item \code{gaez_crop_code}: The crop code used in GAEZ URLs
#'     \item \code{name}: Full crop name
#'     \item \code{gaez_crop_group}: Crop group classification
#'     \item \code{gaez_theme}: Theme number
#'   }
#'
#' @examples
#' \dontrun{
#' # List all crops in theme 4 (most common)
#' list_gaez_crops()
#'
#' # List only cereals
#' list_gaez_crops(crop_group = "cereal")
#'
#' # List oil crops
#' list_gaez_crops(crop_group = "oil")
#'
#' # List crops for theme 3 (4-letter codes)
#' list_gaez_crops(theme = 3)
#' }
#'
#' @export
#' @importFrom dplyr filter
#' @importFrom stringr str_detect str_to_lower
list_gaez_crops <- function(crop_group = NULL, theme = 4) {
  # Validate theme
  if (!theme %in% c(3, 4, 5, 6)) {
    stop("Theme must be 3, 4, 5, or 6", call. = FALSE)
  }

  # Filter by theme
  crops <- gaez_crops |>
    filter(gaez_theme == theme)

  # Filter by crop group if specified
  if (!is.null(crop_group)) {
    crops <- crops |>
      filter(str_detect(str_to_lower(gaez_crop_group), str_to_lower(crop_group)))
  }

  return(crops)
}


#' Show GAEZ v5 function examples
#'
#' Displays example usage for the main functions in the gaezv5 package.
#' Helpful for learning the package workflow and discovering functionality.
#'
#' @return Invisible NULL. Prints examples to console.
#'
#' @examples
#' show_gaez_examples()
#'
#' @export
show_gaez_examples <- function() {
  cat("GAEZ v5 Function Examples:\n\n")

  cat("1. Variable lookup:\n")
  cat("   lookup_gaez_variable('RES05-YX')\n")
  cat("   lookup_gaez_variable('yield')\n\n")

  cat("2. Crop lookup:\n")
  cat("   lookup_gaez_crop('maize', theme = 4)\n")
  cat("   lookup_gaez_crop('pearl millet', theme = 4)\n\n")

  cat("3. URL building:\n")
  cat("   build_gaez_url()\n")
  cat(
    "   build_gaez_url(variable = 'RES05-YX', crop = 'maize', time_period = 'HP0120')\n"
  )
  cat(
    "   build_gaez_url(crop = 'pearl millet', climate_model = 'AGERA5', time_period = 'HP0120')\n\n"
  )

  cat("4. List crops:\n")
  cat("   list_gaez_crops()\n")
  cat("   list_gaez_crops(crop_group = 'cereals')\n\n")

  cat("5. Download datasets:\n")
  cat("   download_gaez_dataset()\n")
  cat(
    "   download_gaez_dataset(variable = 'RES05-YX', crop = 'maize', time_period = 'HP0120')\n"
  )
  cat(
    "   batch_download_gaez_datasets(variables = c('RES05-YX', 'RES05-ETC'), crops = c('sorghum', 'pearl millet'))\n\n"
  )

  cat("6. File management:\n")
  cat("   check_url_exists(url)\n")
  cat("   list_downloaded_files()\n\n")

  cat("7. Climate/SSP validation:\n")
  cat("   validate_climate_ssp('HP0120', 'AGERA5', 'HIST')\n")
  cat("   validate_climate_ssp('FP4160', 'ENSEMBLE', 'SSP370')\n\n")

  invisible(NULL)
}


#' Get country boundary for spatial cropping
#'
#' Retrieves or uses a country boundary as a terra SpatVector for cropping
#' GAEZ rasters to country-level extents. Supports country names, ISO3 codes,
#' or custom SpatVector objects. Automatically downloads GADM boundaries and
#' caches them locally to avoid repeated downloads.
#'
#' @param country Character or SpatVector - Country identifier or boundary.
#'   Can be:
#'   \itemize{
#'     \item Country name (e.g., "Niger", "United States")
#'     \item ISO3 code (e.g., "NER", "USA")
#'     \item terra SpatVector object with polygon(s)
#'   }
#' @param level Numeric - GADM administrative level (default: 0 = country level).
#'   Higher levels provide finer subdivisions (1 = provinces, 2 = districts, etc.)
#' @param cache_dir Character - Directory to cache GADM downloads. If NULL,
#'   uses package cache directory. Default: NULL.
#' @param verbose Logical - Whether to print progress messages (default: TRUE)
#'
#' @return A terra SpatVector object containing the country boundary polygon(s)
#'
#' @details
#' ## Country Matching
#' The function uses `geodata::country_codes()` to match country inputs:
#' \itemize{
#'   \item Exact ISO3 code match (case-insensitive)
#'   \item Partial country name match (case-insensitive)
#'   \item If multiple matches found, uses the first exact name match
#' }
#'
#' ## Boundary Downloads
#' Country boundaries are downloaded from the GADM database via `geodata::gadm()`:
#' \itemize{
#'   \item Downloads are cached locally to avoid repeated requests
#'   \item Default cache location: [package_cache]/GADM/
#'   \item Boundaries are in WGS84 coordinate system (EPSG:4326)
#' }
#'
#' ## Administrative Levels
#' GADM provides multiple administrative levels:
#' \itemize{
#'   \item Level 0: Country boundaries
#'   \item Level 1: First-level subdivisions (states, provinces)
#'   \item Level 2: Second-level subdivisions (districts, counties)
#'   \item Levels 3-5: Finer subdivisions (availability varies by country)
#' }
#'
#' ## Error Handling
#' The function provides informative errors for:
#' \itemize{
#'   \item Country not found - suggests similar names
#'   \item Ambiguous matches - lists all matches
#'   \item Download failures - reports network/server issues
#' }
#'
#' @examples
#' \dontrun{
#' # Get country boundary by name
#' niger <- get_country_boundary("Niger")
#' terra::plot(niger)
#'
#' # Get country boundary by ISO3 code
#' usa <- get_country_boundary("USA")
#'
#' # Get provincial boundaries (level 1)
#' niger_provinces <- get_country_boundary("Niger", level = 1)
#'
#' # Use custom SpatVector
#' library(geodata)
#' custom_boundary <- gadm(country = "NER", level = 0)
#' boundary <- get_country_boundary(custom_boundary)
#' }
#'
#' @seealso \code{\link{load_gaez_data}}, \code{\link{combine_gaez_batch}}
#'
#' @export
get_country_boundary <- function(country,
                                  level = 0,
                                  cache_dir = NULL,
                                  verbose = TRUE) {
  # Check if geodata is available
  if (!requireNamespace("geodata", quietly = TRUE)) {
    stop(
      "Package 'geodata' is required for country boundaries. ",
      "Please install it with: install.packages('geodata')",
      call. = FALSE
    )
  }

  # Check if terra is available
  if (!requireNamespace("terra", quietly = TRUE)) {
    stop(
      "Package 'terra' is required for this function. ",
      "Please install it with: install.packages('terra')",
      call. = FALSE
    )
  }

  # If country is already a SpatVector, return it
  if (inherits(country, "SpatVector")) {
    if (verbose) {
      cat("Using provided SpatVector boundary\n")
      cat("  Geometries:", terra::nrow(country), "\n")
      cat("  CRS:", as.character(terra::crs(country)), "\n")
    }
    return(country)
  }

  # Validate country input
  if (!is.character(country) || length(country) != 1 || country == "") {
    stop("country must be a non-empty character string or SpatVector",
      call. = FALSE
    )
  }

  if (verbose) {
    cat("Looking up country:", country, "\n")
  }

  # Get country codes database
  tryCatch(
    {
      country_db <- geodata::country_codes()
    },
    error = function(e) {
      stop(
        "Failed to retrieve country codes database: ",
        e$message,
        call. = FALSE
      )
    }
  )

  # Try exact ISO3 match first (case-insensitive)
  iso3_match <- country_db[toupper(country_db$ISO3) == toupper(country), ]

  if (nrow(iso3_match) == 1) {
    country_code <- iso3_match$ISO3[1]
    country_name <- iso3_match$NAME[1]
    if (verbose) {
      cat("  Matched ISO3:", country_code, "(", country_name, ")\n")
    }
  } else {
    # Try partial name match (case-insensitive)
    name_matches <- country_db[grepl(country, country_db$NAME, ignore.case = TRUE), ]

    if (nrow(name_matches) == 0) {
      # No matches found - suggest similar names
      all_names <- country_db$NAME
      # Simple fuzzy match: find names starting with same letter
      similar <- all_names[substr(tolower(all_names), 1, 1) == substr(tolower(country), 1, 1)]
      stop(
        "Country '", country, "' not found.\n",
        if (length(similar) > 0) {
          paste(
            "Did you mean one of these?\n  ",
            paste(head(similar, 10), collapse = ", ")
          )
        } else {
          "Please use a valid country name or ISO3 code."
        },
        call. = FALSE
      )
    } else if (nrow(name_matches) > 1) {
      # Multiple matches - try to find exact match
      exact_match <- name_matches[tolower(name_matches$NAME) == tolower(country), ]

      if (nrow(exact_match) == 1) {
        country_code <- exact_match$ISO3[1]
        country_name <- exact_match$NAME[1]
        if (verbose) {
          cat("  Matched name:", country_name, "(", country_code, ")\n")
        }
      } else {
        # Ambiguous match
        stop(
          "Ambiguous country name '", country, "'. Multiple matches found:\n",
          paste("  ", name_matches$NAME, " (", name_matches$ISO3, ")", collapse = "\n"),
          "\nPlease use a more specific name or ISO3 code.",
          call. = FALSE
        )
      }
    } else {
      # Single match
      country_code <- name_matches$ISO3[1]
      country_name <- name_matches$NAME[1]
      if (verbose) {
        cat("  Matched name:", country_name, "(", country_code, ")\n")
      }
    }
  }

  # Set cache directory
  if (is.null(cache_dir)) {
    cache_dir <- file.path(get_download_cache(create = TRUE), "GADM")
    if (!dir.exists(cache_dir)) {
      dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
    }
  }

  # Download/retrieve boundary
  if (verbose) {
    cat("  Retrieving GADM boundary (level", level, ")...\n")
  }

  tryCatch(
    {
      boundary <- geodata::gadm(
        country = country_code,
        level = level,
        path = cache_dir
      )

      if (verbose) {
        cat("  Successfully retrieved boundary\n")
        cat("    Geometries:", terra::nrow(boundary), "\n")
        cat("    Extent:", paste(as.vector(terra::ext(boundary)), collapse = ", "), "\n")
        cat("    CRS:", as.character(terra::crs(boundary)), "\n")
      }

      return(boundary)
    },
    error = function(e) {
      stop(
        "Failed to download GADM boundary for ", country_name, " (", country_code, "):\n",
        "  ", e$message, "\n",
        "  Please check your internet connection and try again.",
        call. = FALSE
      )
    }
  )
}
