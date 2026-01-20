#' List available GAEZ crops
#'
#' Returns a table of available crops in the GAEZ v5 database. Different themes
#' use different crop coding systems, so specify the appropriate theme for your
#' analysis. Optionally filter by crop group (e.g., cereals, oil crops).
#'
#' If no crops match the specified \code{crop_group}, the function will display
#' all available crop groups for the selected theme and return \code{NULL}
#' invisibly, prompting you to resubmit with a valid crop group.
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
#' @return A tibble with columns (or \code{NULL} if no match found):
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
#'
#' # If no match found, available groups are displayed
#' list_gaez_crops(crop_group = "invalid_group")
#' # Returns NULL and shows: "Available crop groups for theme 4: ..."
#' }
#'
#' @export
#' @importFrom dplyr filter pull
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
    crops_filtered <- crops |>
      filter(str_detect(str_to_lower(gaez_crop_group), str_to_lower(crop_group)))

    # Check if no match was found
    if (nrow(crops_filtered) == 0) {
      # Get available crop groups for this theme
      available_groups <- crops |>
        filter(!is.na(gaez_crop_group)) |>
        pull(gaez_crop_group) |>
        unique() |>
        sort()

      # Create informative message
      if (length(available_groups) > 0) {
        message(
          "No crops found matching crop_group '", crop_group, "' in theme ", theme, ".\n\n",
          "Available crop groups for theme ", theme, ":\n  ",
          paste(available_groups, collapse = "\n  "),
          "\n\nPlease resubmit your query with one of the available crop groups."
        )
      } else {
        message(
          "No crops found matching crop_group '", crop_group, "' in theme ", theme, ".\n\n",
          "Note: Theme ", theme, " does not use crop groups. ",
          "Consider listing all crops with: list_gaez_crops(theme = ", theme, ")"
        )
      }

      return(invisible(NULL))
    }

    crops <- crops_filtered
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

  cat("5. List scenarios:\n")
  cat("   list_gaez_scenarios()  # Time periods by default\n")
  cat("   list_gaez_scenarios('climate_model')\n")
  cat("   list_gaez_scenarios('ssp')\n")
  cat("   list_gaez_scenarios('water_management')\n\n")

  cat("6. Download datasets:\n")
  cat("   download_gaez_dataset()\n")
  cat(
    "   download_gaez_dataset(variable = 'RES05-YX', crop = 'maize', time_period = 'HP0120')\n"
  )
  cat(
    "   batch_download_gaez_datasets(variables = c('RES05-YX', 'RES05-ETC'), crops = c('sorghum', 'pearl millet'))\n\n"
  )

  cat("7. File management:\n")
  cat("   check_url_exists(url)\n")
  cat("   list_downloaded_files()\n\n")

  cat("8. Climate/SSP validation:\n")
  cat("   validate_climate_ssp('HP0120', 'AGERA5', 'HIST')\n")
  cat("   validate_climate_ssp('FP4160', 'ENSEMBLE', 'SSP370')\n\n")

  invisible(NULL)
}


#' Print method for batch download results
#'
#' Provides clear summary of batch download operations
#'
#' @param x A gaez_batch_result object
#' @param ... Additional arguments (ignored)
#' @export
print.gaez_batch_result <- function(x, ...) {
  if (length(x) == 0) {
    # Empty result - show error information
    cat("=== GAEZ Batch Download (FAILED) ===\n")
    cat("Error:", attr(x, "error_message"), "\n")
    cat("Combinations attempted:", attr(x, "combinations_attempted", exact = TRUE), "\n")
    cat("Valid combinations:", attr(x, "valid_combinations", exact = TRUE), "\n")
  } else {
    # Normal result with downloads
    successes <- sum(sapply(x, function(r) isTRUE(r$success)))
    cat("=== GAEZ Batch Download Results ===\n")
    cat("Total downloads:", length(x), "\n")
    cat("Successful:", successes, "\n")
    cat("Failed:", length(x) - successes, "\n")
  }
  invisible(x)
}


#' List available GAEZ scenarios
#'
#' Returns a table of scenario information for one dimension (time periods,
#' climate models, SSP scenarios, or water management codes). This function
#' provides a user-friendly way to explore available scenarios and understand
#' which combinations are valid for different time periods.
#'
#' @param type Character - Type of scenario information to list. Options:
#'   \itemize{
#'     \item \code{"time_period"} (default): Time periods with years and descriptions
#'     \item \code{"climate_model"}: Available climate models
#'     \item \code{"ssp"}: Shared Socioeconomic Pathway scenarios
#'     \item \code{"water_management"}: Water management codes
#'     \item \code{"water_supply"}: Water supply codes
#'   }
#'
#' @return A tibble containing the requested scenario information. For time
#'   periods, includes an additional note about which climate models are
#'   available for each period. Returns \code{NULL} invisibly if an invalid
#'   type is provided (after showing available options).
#'
#' @details
#' ## Scenario Combinations
#' Not all combinations of scenarios are valid in GAEZ v5:
#' \itemize{
#'   \item **Historical periods** (HP8100, HP0120): Only work with AGERA5
#'     climate model and HIST scenario
#'   \item **Future periods** (FP2140, FP4160, FP6180, FP8100): Work with
#'     6 climate models (ENSEMBLE, GFDL-ESM4, IPSL-CM6A-LR, MPI-ESM1-2-HR,
#'     MRI-ESM2-0, UKESM1-0-LL) and 3 SSP scenarios (SSP126, SSP370, SSP585)
#' }
#'
#' ## Other Scenario Types
#' Use \code{type} parameter to query different scenario dimensions:
#' \itemize{
#'   \item \code{"climate_model"}: List available climate models
#'   \item \code{"ssp"}: List SSP scenarios
#'   \item \code{"water_management"}: List water management codes (for themes 3-4)
#'   \item \code{"water_supply"}: List water supply codes (for themes 5-6)
#' }
#'
#' @examples
#' \dontrun{
#' # List time periods (default)
#' list_gaez_scenarios()
#'
#' # List climate models
#' list_gaez_scenarios("climate_model")
#'
#' # List SSP scenarios
#' list_gaez_scenarios("ssp")
#'
#' # List water management codes
#' list_gaez_scenarios("water_management")
#'
#' # List water supply codes
#' list_gaez_scenarios("water_supply")
#'
#' # Invalid type shows available options
#' list_gaez_scenarios("invalid")
#' }
#'
#' @export
#' @importFrom dplyr filter select
list_gaez_scenarios <- function(type = "time_period") {
  # Validate type parameter
  valid_types <- c("time_period", "climate_model", "ssp", "water_management", "water_supply")

  if (!type %in% valid_types) {
    message(
      "Invalid scenario type '", type, "'.\n\n",
      "Valid options:\n",
      "  - time_period: Time periods with years and descriptions\n",
      "  - climate_model: Available climate models\n",
      "  - ssp: Shared Socioeconomic Pathway scenarios\n",
      "  - water_management: Water management codes\n",
      "  - water_supply: Water supply codes\n\n",
      "Please resubmit with one of the valid types."
    )
    return(invisible(NULL))
  }

  # Get scenarios based on type
  result <- switch(
    type,
    "time_period" = {
      scenarios <- gaez_scenarios |>
        filter(!is.na(time_period)) |>
        select(time_period, description, start_year, end_year)

      # Add informative message about climate model combinations
      message(
        "GAEZ v5 Time Periods\n",
        "====================\n\n",
        "Historical periods (HP*):\n",
        "  - Only work with AGERA5 climate model and HIST scenario\n\n",
        "Future periods (FP*):\n",
        "  - Work with 6 climate models: ENSEMBLE (recommended), GFDL-ESM4,\n",
        "    IPSL-CM6A-LR, MPI-ESM1-2-HR, MRI-ESM2-0, UKESM1-0-LL\n",
        "  - Work with 3 SSP scenarios: SSP126, SSP370, SSP585\n\n",
        "Use list_gaez_scenarios('climate_model') to see available climate models.\n",
        "Use list_gaez_scenarios('ssp') to see available SSP scenarios.\n"
      )

      scenarios
    },
    "climate_model" = {
      scenarios <- gaez_scenarios |>
        filter(!is.na(climate_model)) |>
        select(climate_model, description)

      message(
        "GAEZ v5 Climate Models\n",
        "======================\n\n",
        "Historical periods (HP8100, HP0120):\n",
        "  - AGERA5 only\n\n",
        "Future periods (FP2140, FP4160, FP6180, FP8100):\n",
        "  - ENSEMBLE (multi-model mean, recommended)\n",
        "  - GFDL-ESM4, IPSL-CM6A-LR, MPI-ESM1-2-HR, MRI-ESM2-0, UKESM1-0-LL\n\n",
        "Use list_gaez_scenarios('time_period') to see available time periods.\n"
      )

      scenarios
    },
    "ssp" = {
      scenarios <- gaez_scenarios |>
        filter(!is.na(ssp)) |>
        select(ssp, description)

      message(
        "GAEZ v5 SSP Scenarios\n",
        "=====================\n\n",
        "Historical periods: HIST\n",
        "Future periods: SSP126 (low emissions), SSP370 (medium), SSP585 (high)\n\n",
        "Use list_gaez_scenarios('time_period') to see available time periods.\n"
      )

      scenarios
    },
    "water_management" = {
      scenarios <- gaez_scenarios |>
        filter(!is.na(water_management_code)) |>
        select(water_management_code, description)

      message(
        "GAEZ v5 Water Management Codes\n",
        "===============================\n\n",
        "Used in themes 3 (Agro-climatic Potential Yield) and\n",
        "4 (Suitability & Attainable Yield).\n\n",
        "Input level: High (H) or Low (L)\n",
        "Water supply: Rain-fed (R) or Irrigated (I)\n",
        "Management: Low (L) or Mixed (M)\n\n",
        "Use list_gaez_scenarios('water_supply') for themes 5-6 codes.\n"
      )

      scenarios
    },
    "water_supply" = {
      scenarios <- gaez_scenarios |>
        filter(!is.na(water_supply)) |>
        select(water_supply, description)

      message(
        "GAEZ v5 Water Supply Codes\n",
        "==========================\n\n",
        "Used in themes 5 (Actual Yields & Production) and\n",
        "6 (Yield & Production Gaps).\n\n",
        "Use list_gaez_scenarios('water_management') for themes 3-4 codes.\n"
      )

      scenarios
    }
  )

  return(result)
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
