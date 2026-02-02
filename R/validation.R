#' Validate and adjust climate model and SSP scenario based on time period
#'
#' GAEZ v5 data uses different climate models and scenarios for historical vs
#' future time periods. This function validates the climate model and SSP
#' combination and automatically selects appropriate defaults when needed.
#'
#' @param time_period Character - Time period code (e.g., "HP0120", "FP4160").
#'   Must start with "HP" (historical) or "FP" (future).
#' @param climate_model Character - Climate model name. Can be NULL for
#'   auto-selection. Valid options:
#'   \itemize{
#'     \item Historical: "AGERA5" (only option)
#'     \item Future: "ENSEMBLE", "GFDL-ESM4", "IPSL-CM6A-LR",
#'           "MPI-ESM1-2-HR", "MRI-ESM2-0", "UKESM1-0-LL"
#'   }
#' @param ssp Character - SSP scenario code. Valid options depend on time period:
#'   \itemize{
#'     \item Historical: "HIST" (automatically set)
#'     \item Future: "SSP126", "SSP370", "SSP585"
#'   }
#'
#' @return A list with two elements:
#'   \itemize{
#'     \item \code{climate_model}: Validated/adjusted climate model
#'     \item \code{ssp}: Validated/adjusted SSP scenario
#'   }
#'
#' @details
#' Historical periods (HP*) only work with the AGERA5 climate model and HIST
#' scenario. Future periods (FP*) require a climate model (default: ENSEMBLE)
#' and an SSP scenario (SSP126, SSP370, or SSP585).
#'
#' @examples
#' \dontrun{
#' # Historical period - auto-selects AGERA5 and HIST
#' validate_climate_ssp("HP0120", NULL, "SSP370")
#'
#' # Future period - auto-selects ENSEMBLE
#' validate_climate_ssp("FP4160", NULL, "SSP370")
#'
#' # Future period - use specific model
#' validate_climate_ssp("FP4160", "MRI-ESM2-0", "SSP370")
#' }
#'
#' @export
#' @importFrom stringr str_starts
validate_climate_ssp <- function(time_period,
                                  climate_model = NULL,
                                  ssp = "SSP370") {
  # Input validation
  if (missing(time_period) || is.null(time_period) || time_period == "") {
    stop("time_period is required", call. = FALSE)
  }

  # Determine if time period is future or historical
  is_future <- str_starts(time_period, "FP")
  is_historical <- str_starts(time_period, "HP")

  if (!is_future && !is_historical) {
    stop(
      "Time period must start with 'FP' (future) or 'HP' (historical)",
      call. = FALSE
    )
  }

  # Handle climate model validation
  if (is_future) {
    # Future periods
    if (is.null(climate_model)) {
      climate_model <- "ENSEMBLE"
      message(
        "No climate model specified for future period. Using default: ENSEMBLE"
      )
    } else if (climate_model == "AGERA5") {
      stop(
        "AgERA5 climate model only works with historical periods (HP)",
        call. = FALSE
      )
    }

    # Validate climate model for future periods
    valid_future_models <- c(
      "ENSEMBLE",
      "GFDL-ESM4",
      "IPSL-CM6A-LR",
      "MPI-ESM1-2-HR",
      "MRI-ESM2-0",
      "UKESM1-0-LL"
    )

    if (!climate_model %in% valid_future_models) {
      stop(
        paste(
          "Invalid climate model for future period. Valid options:",
          paste(valid_future_models, collapse = ", ")
        ),
        call. = FALSE
      )
    }

    # Validate SSP for future periods
    valid_ssps <- c("SSP126", "SSP370", "SSP585")
    if (!ssp %in% valid_ssps) {
      stop(
        paste(
          "Future periods require SSP scenarios. Valid options:",
          paste(valid_ssps, collapse = ", ")
        ),
        call. = FALSE
      )
    }
  } else {
    # Historical periods
    if (is.null(climate_model)) {
      climate_model <- "AGERA5"
      message(
        "No climate model specified for historical period. Using default: AGERA5"
      )
    } else if (climate_model != "AGERA5") {
      stop(
        "Historical periods (HP) only work with AGERA5 climate model",
        call. = FALSE
      )
    }

    # Historical periods use HIST scenario
    if (ssp != "HIST") {
      ssp <- "HIST"
      message("Historical periods use HIST scenario. Adjusting SSP parameter.")
    }
  }

  return(list(climate_model = climate_model, ssp = ssp))
}


#' Look up GAEZ time period from year range
#'
#' Validates year ranges and returns the corresponding GAEZ v5 time period code.
#' Handles three cases: start_year only, end_year only, or both years. When a
#' year range spans multiple time periods, prompts for user selection in
#' interactive mode or auto-selects the first match in non-interactive mode.
#'
#' @param start_year Numeric - Start year for time period lookup (optional).
#'   Must be between 1981-2100.
#' @param end_year Numeric - End year for time period lookup (optional).
#'   Must be between 1981-2100.
#' @param time_period Character - Existing time period code (optional). If
#'   provided along with years, the years take precedence for lookup.
#' @param interactive Logical - Whether to prompt for user input when year range
#'   spans multiple time periods. Default is TRUE. Set to FALSE to automatically
#'   select the first match without prompting.
#' @param verbose Logical - Whether to show informative messages about selected
#'   time period. Default is TRUE.
#'
#' @return Character string containing the time period code (e.g., "HP0120",
#'   "FP4160"). Returns the input time_period if no years are provided.
#'
#' @details
#' GAEZ v5 time periods represent 20-year averages:
#' \itemize{
#'   \item HP8100: Historical baseline (1981-2000)
#'   \item HP0120: Historical recent (2001-2020)
#'   \item FP2140: Future period (2021-2040)
#'   \item FP4160: Future period (2041-2060)
#'   \item FP6180: Future period (2061-2080)
#'   \item FP8100: Future period (2081-2100)
#' }
#'
#' The function validates that requested years fall within the GAEZ data
#' availability range (1981-2100) and provides informative error messages
#' listing all available periods when years are out of range.
#'
#' @examples
#' \dontrun{
#' # Single year - finds containing period
#' lookup_time_period_from_years(start_year = 2050)
#'
#' # Year range in single period
#' lookup_time_period_from_years(start_year = 2041, end_year = 2060)
#'
#' # Year range spanning multiple periods (prompts for selection)
#' lookup_time_period_from_years(start_year = 2030, end_year = 2070)
#'
#' # Auto-select without prompting
#' lookup_time_period_from_years(
#'   start_year = 2030,
#'   end_year = 2070,
#'   interactive = FALSE
#' )
#'
#' # Invalid year (errors with available periods)
#' lookup_time_period_from_years(start_year = 1950)  # Before 1981
#' }
#'
#' @seealso \code{\link{validate_climate_ssp}} for climate model/SSP validation
#'
#' @export
#' @importFrom dplyr filter select mutate row_number n
lookup_time_period_from_years <- function(start_year = NULL,
                                          end_year = NULL,
                                          time_period = NULL,
                                          interactive = TRUE,
                                          verbose = TRUE) {
  # If time_period already provided and no years, return it as-is
  if (!is.null(time_period) && is.null(start_year) && is.null(end_year)) {
    return(time_period)
  }

  # If no years provided, return the time_period parameter
  if (is.null(start_year) && is.null(end_year)) {
    return(time_period)
  }

  # Get time period data
  time_periods_data <- gaez_scenarios |>
    dplyr::filter(!is.na(time_period), !is.na(start_year), !is.na(end_year))

  # Validate years are within GAEZ range
  all_years <- c(start_year, end_year)
  all_years <- all_years[!is.na(all_years)]

  min_available <- min(time_periods_data$start_year)
  max_available <- max(time_periods_data$end_year)

  if (any(all_years < min_available | all_years > max_available)) {
    invalid_years <- all_years[all_years < min_available | all_years > max_available]

    # Build informative error message
    error_msg <- paste0(
      "Year(s) ", paste(invalid_years, collapse = ", "),
      " outside available GAEZ data range (", min_available, "-", max_available, ").\n\n",
      "Available time periods:\n"
    )

    periods_info <- time_periods_data |>
      dplyr::select(time_period, start_year, end_year) |>
      dplyr::distinct() |>
      dplyr::mutate(period_range = paste0(time_period, " (", start_year, "-", end_year, ")"))

    for (i in seq_len(nrow(periods_info))) {
      error_msg <- paste0(error_msg, "  - ", periods_info$period_range[i], "\n")
    }

    stop(error_msg, call. = FALSE)
  }

  # Case 1: Only start_year provided
  if (!is.null(start_year) && is.null(end_year)) {
    time_lookup <- time_periods_data |>
      dplyr::filter(start_year <= !!start_year, end_year >= !!start_year)

    if (nrow(time_lookup) == 0) {
      stop(paste("No time period found containing year", start_year), call. = FALSE)
    }

    result_period <- time_lookup$time_period[1]

    if (verbose) {
      message(paste0(
        "Using time period ", result_period, " (",
        time_lookup$start_year[1], "-", time_lookup$end_year[1], "). ",
        "Note: GAEZ data represents 20-year averages for this period."
      ))
    }

    return(result_period)
  }

  # Case 2: Only end_year provided
  if (is.null(start_year) && !is.null(end_year)) {
    time_lookup <- time_periods_data |>
      dplyr::filter(start_year <= !!end_year, end_year >= !!end_year)

    if (nrow(time_lookup) == 0) {
      stop(paste("No time period found containing year", end_year), call. = FALSE)
    }

    result_period <- time_lookup$time_period[1]

    if (verbose) {
      message(paste0(
        "Using time period ", result_period, " (",
        time_lookup$start_year[1], "-", time_lookup$end_year[1], "). ",
        "Note: GAEZ data represents 20-year averages for this period."
      ))
    }

    return(result_period)
  }

  # Case 3: Both start_year and end_year provided
  time_lookup <- time_periods_data |>
    dplyr::filter(start_year <= !!end_year & end_year >= !!start_year)

  if (nrow(time_lookup) == 0) {
    stop(
      paste("No time period found for years", start_year, "-", end_year),
      call. = FALSE
    )
  }

  # Single match
  if (nrow(time_lookup) == 1) {
    result_period <- time_lookup$time_period[1]

    if (verbose) {
      message(paste0(
        "Using time period ", result_period, " (",
        time_lookup$start_year[1], "-", time_lookup$end_year[1], "). ",
        "Note: GAEZ data represents 20-year averages for this period."
      ))
    }

    return(result_period)
  }

  # Multiple matches - need user selection
  if (verbose) {
    message(paste0(
      "The year range ", start_year, "-", end_year,
      " spans multiple GAEZ time periods:"
    ))
  }

  time_lookup_display <- time_lookup |>
    dplyr::mutate(option = dplyr::row_number()) |>
    dplyr::select(option, time_period, start_year, end_year)

  print(time_lookup_display)

  # Check if interactive mode
  use_interactive <- interactive & interactive()

  if (use_interactive) {
    choice <- as.numeric(readline("Please enter the number of your choice: "))

    if (is.na(choice) || choice < 1 || choice > nrow(time_lookup)) {
      stop("Invalid choice. Please run the function again.", call. = FALSE)
    }

    selected_period <- time_lookup[choice, ]
  } else {
    if (verbose) {
      message("Auto-selecting first time period match")
    }
    selected_period <- time_lookup[1, ]
  }

  result_period <- selected_period$time_period

  if (verbose) {
    message(paste0(
      "Selected time period ", result_period, " (",
      selected_period$start_year, "-", selected_period$end_year, "). ",
      "Note: GAEZ data represents 20-year averages for this period."
    ))
  }

  return(result_period)
}


#' Validate GAEZ parameter combinations
#'
#' Validates that all provided GAEZ parameters form a valid combination before
#' attempting data access. This function performs early validation to provide
#' informative error messages listing valid options, rather than failing with
#' HTTP 404 errors during download.
#'
#' @param variable Character - Variable code (e.g., "RES05-YX"). Must be a valid
#'   GAEZ variable code.
#' @param time_period Character - Time period code (e.g., "HP0120", "FP4160").
#' @param climate_model Character - Climate model name (e.g., "ENSEMBLE", "AGERA5").
#' @param ssp Character - SSP scenario code (e.g., "SSP370", "HIST").
#' @param crop Character - Crop name or code.
#' @param water_management_level Character - Water management code for themes 3-4
#'   (e.g., "HRLM", "HILM").
#' @param water_supply Character - Water supply code for themes 5-6
#'   (e.g., "WSR", "WSI", "WST").
#' @param interactive Logical - Whether to allow interactive selection when
#'   multiple matches are found. Default is TRUE.
#' @param verbose Logical - Whether to print validation progress. Default is TRUE.
#'
#' @return A list with validated/resolved parameters:
#'   \itemize{
#'     \item \code{var_info}: Tibble with variable information
#'     \item \code{crop_code}: Resolved crop code
#'     \item \code{time_period}: Validated time period
#'     \item \code{climate_model}: Validated climate model
#'     \item \code{ssp}: Validated SSP scenario
#'     \item \code{water_management_level}: Validated water management code (themes 3-4)
#'     \item \code{water_supply}: Validated water supply code (themes 5-6)
#'     \item \code{theme_number}: Theme number from variable info
#'   }
#'
#' @details
#' ## Validation Checks
#' The function performs the following validations:
#' \enumerate{
#'   \item **Variable**: Must be a valid GAEZ variable code
#'   \item **Crop**: Must be valid for the variable's theme
#'   \item **Time period**: Must be a valid GAEZ time period code
#'   \item **Climate model + SSP**: Must be valid combination for the time period
#'   \item **Water management** (themes 3-4): Must be one of HRLM, HILM, LRLM, LILM
#'   \item **Water supply** (themes 5-6): Must be one of WSR, WSI, WST
#'   \item **Theme-specific requirements**: Ensures correct parameters for each theme
#' }
#'
#' ## Error Messages
#' When validation fails, the function provides informative messages listing:
#' \itemize{
#'   \item What combination was invalid
#'   \item Which options are available
#'   \item How to use helper functions to explore valid options
#' }
#'
#' @examples
#' \dontrun
#' # Valid combination for theme 4 (suitability/yield)
#' params <- validate_gaez_parameters(
#'   variable = "RES05-YX",
#'   crop = "maize",
#'   time_period = "FP4160",
#'   climate_model = "ENSEMBLE",
#'   ssp = "SSP370",
#'   water_management_level = "HRLM"
#' )
#'
#' # Valid combination for theme 5 (actual yields)
#' params <- validate_gaez_parameters(
#'   variable = "RES06-YLD",
#'   crop = "maize",
#'   water_supply = "WSR"
#' )
#' }
#'
#' @seealso
#' \code{\link{lookup_gaez_variable}} for variable lookup,
#' \code{\link{lookup_gaez_crop}} for crop lookup,
#' \code{\link{validate_climate_ssp}} for climate/SSP validation,
#' \code{\link{list_gaez_scenarios}} for listing valid scenarios
#'
#' @export
validate_gaez_parameters <- function(variable,
                                      time_period = "FP4160",
                                      start_year = NULL,
                                      end_year = NULL,
                                      climate_model = NULL,
                                      ssp = "SSP370",
                                      crop = "WHEA",
                                      water_management_level = "HRLM",
                                      water_supply = "WSR",
                                      interactive = TRUE,
                                      verbose = TRUE) {
  
  # Normalize inputs to uppercase
  variable <- toupper(variable)
  crop <- toupper(crop)
  if (!is.null(climate_model)) climate_model <- toupper(climate_model)
  if (!is.null(ssp)) ssp <- toupper(ssp)
  water_management_level <- toupper(water_management_level)
  water_supply <- toupper(water_supply)
  
  # ===========================
  # 1. Validate variable
  # ===========================
  var_info <- lookup_gaez_variable(variable)
  theme_number <- var_info$theme_number
  
  # ===========================
  # 2. Validate crop (themes 3-6)
  # ===========================
  crop_code <- NULL
  if (theme_number %in% c(3, 4, 5, 6)) {
    crop_code <- lookup_gaez_crop(crop, theme_number, interactive = interactive)
  } else {
    crop_code <- NA_character_
  }
  
  # ===========================
  # 3. Validate time period
  # ===========================
  time_period <- lookup_time_period_from_years(
    start_year = start_year,
    end_year = end_year,
    time_period = time_period,
    interactive = interactive,
    verbose = verbose
  )
  
  # ===========================
  # 4. Validate climate model and SSP
  # ===========================
  # Themes 5 and 6 don't use time-varying climate data
  if (theme_number %in% c(5, 6)) {
    # Theme 5-6: Actual yields/gaps don't use climate models or time periods
    climate_model <- NA_character_
    ssp <- NA_character_
  } else if (theme_number %in% c(1)) {
    # Theme 1: Static land resources, no climate/time variation
    climate_model <- NA_character_
    ssp <- NA_character_
  } else {
    # Themes 2, 3, 4: Use climate models and SSPs
    validation_result <- validate_climate_ssp(time_period, climate_model, ssp)
    climate_model <- validation_result$climate_model
    ssp <- validation_result$ssp
  }
  
  # ===========================
  # 5. Validate water management (themes 3-4)
  # ===========================
  if (theme_number %in% c(3, 4)) {
    valid_water_management <- c("HRLM", "HILM", "LRLM", "LILM")
    
    if (!water_management_level %in% valid_water_management) {
      # Get descriptions for helpful message
      wm_scenarios <- gaez_scenarios |>
        dplyr::filter(!is.na(water_management_code)) |>
        dplyr::select(water_management_code, description)
      
      error_msg <- paste0(
        "Invalid water_management_level: '", water_management_level, "'\n\n",
        "Valid options for themes 3-4:\n"
      )
      
      for (i in seq_len(nrow(wm_scenarios))) {
        error_msg <- paste0(
          error_msg,
          "  - ", wm_scenarios$water_management_code[i], 
          ": ", wm_scenarios$description[i], "\n"
        )
      }
      
      error_msg <- paste0(
        error_msg,
        "\nUse list_gaez_scenarios('water_management') for more details."
      )
      
      stop(error_msg, call. = FALSE)
    }
  }
  
  # ===========================
  # 6. Validate water supply (themes 5-6)
  # ===========================
  if (theme_number %in% c(5, 6)) {
    valid_water_supply <- c("WSR", "WSI", "WST")
    
    if (!water_supply %in% valid_water_supply) {
      # Get descriptions for helpful message
      ws_scenarios <- gaez_scenarios |>
        dplyr::filter(!is.na(water_supply)) |>
        dplyr::select(water_supply, description)
      
      error_msg <- paste0(
        "Invalid water_supply: '", water_supply, "'\n\n",
        "Valid options for themes 5-6:\n"
      )
      
      for (i in seq_len(nrow(ws_scenarios))) {
        error_msg <- paste0(
          error_msg,
          "  - ", ws_scenarios$water_supply[i],
          ": ", ws_scenarios$description[i], "\n"
        )
      }
      
      error_msg <- paste0(
        error_msg,
        "\nUse list_gaez_scenarios('water_supply') for more details."
      )
      
      stop(error_msg, call. = FALSE)
    }
  }
  
  # ===========================
  # 7. Validate theme-specific parameter requirements
  # ===========================
  url_structure <- gaez_url_structure |>
    dplyr::filter(theme == theme_number) |>
    dplyr::pull(filename_parts) |>
    dplyr::first()
  
  if (!is.null(url_structure) && is.list(url_structure) && length(url_structure) > 0) {
    required_parts <- url_structure[[1]]
    
    # Check themes 3-4 require water_management, not water_supply
    if ("water_management" %in% required_parts && theme_number %in% c(3, 4)) {
      # Already validated above
      if (verbose) {
        cat("  Theme", theme_number, "uses water_management_level:", water_management_level, "\n")
      }
    }
    
    # Check themes 5-6 require water_supply, not water_management
    if ("water_supply" %in% required_parts && theme_number %in% c(5, 6)) {
      # Already validated above
      if (verbose) {
        cat("  Theme", theme_number, "uses water_supply:", water_supply, "\n")
      }
    }
    
    # Check if time_period/climate/ssp are required but theme doesn't support them
    if ("time_period" %in% required_parts && theme_number %in% c(5, 6)) {
      # This shouldn't happen based on url_structure, but check anyway
      warning(
        "Theme ", theme_number, " does not use time periods. ",
        "Time period parameter will be ignored.",
        call. = FALSE
      )
    }
  }
  
  # ===========================
  # Return validated parameters
  # ===========================
  if (verbose) {
    cat("  Variable:", var_info$variable_name, "\n")
    cat("  Crop:", crop_code, "\n")
    if (!is.na(time_period) && theme_number %in% c(2, 3, 4)) {
      cat("  Time period:", time_period, "\n")
    }
    if (!is.na(climate_model)) {
      cat("  Climate model:", climate_model, "\n")
    }
    if (!is.na(ssp)) {
      cat("  SSP:", ssp, "\n")
    }
    if (theme_number %in% c(3, 4)) {
      cat("  Water management:", water_management_level, "\n")
    }
    if (theme_number %in% c(5, 6)) {
      cat("  Water supply:", water_supply, "\n")
    }
  }
  
  return(list(
    var_info = var_info,
    crop_code = crop_code,
    time_period = time_period,
    climate_model = climate_model,
    ssp = ssp,
    water_management_level = water_management_level,
    water_supply = water_supply,
    theme_number = theme_number
  ))
}
