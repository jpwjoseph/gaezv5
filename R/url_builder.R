#' Build GAEZ v5 download URL
#'
#' Constructs a complete download URL for GAEZ v5 datasets from Google Cloud
#' Storage. Automatically validates parameters and suggests alternative
#' resolutions or time series variants when available.
#'
#' @param variable Character - Variable name or code. Default is "RES05-YX"
#'   (attainable yield). Use \code{lookup_gaez_variable()} to find variable codes.
#' @param time_period Character - Time period code. Default is "FP4160" (2041-2060).
#'   Valid options: "HP8100", "HP0120", "FP2140", "FP4160", "FP6180", "FP8100"
#' @param start_year Numeric - Start year for time period lookup (optional). Can be
#'   provided alone or with end_year. If only start_year is given, returns the time
#'   period containing that year. If both years are provided and span multiple periods,
#'   prompts for selection. Must be between 1981-2100.
#' @param end_year Numeric - End year for time period lookup (optional). Can be
#'   provided alone or with start_year. If only end_year is given, returns the time
#'   period containing that year. Must be between 1981-2100.
#' @param climate_model Character - Climate model. NULL for auto-selection based
#'   on time period. Historical periods use "AGERA5", future periods default to
#'   "ENSEMBLE". Other options: "GFDL-ESM4", "IPSL-CM6A-LR", "MPI-ESM1-2-HR",
#'   "MRI-ESM2-0", "UKESM1-0-LL"
#' @param ssp Character - SSP scenario. Default is "SSP370". Historical periods
#'   use "HIST". Future periods use "SSP126", "SSP370", or "SSP585".
#' @param crop Character - Crop name or code. Default is "WHEA" (wheat).
#'   Use \code{lookup_gaez_crop()} to find crop codes.
#' @param water_management_level Character - Water management code. Default is
#'   "HRLM" (High input, Rain-fed, Low management). Other options: "HILM"
#'   (irrigated), "LRLM" (low input, rain-fed), "LILM" (low input, irrigated).
#' @param water_supply Character - Water supply code for actual yields/gaps
#'   (themes 5-6). Default is "WSR" (rain-fed). Other options: "WSI" (irrigated),
#'   "WST" (total).
#' @param resolution Character - Resolution preference. Options: "1km", "10km", or
#'   NA (uses default for variable). Note: not all variables have multiple
#'   resolutions.
#'
#' @return Character string containing the complete HTTPS download URL
#'
#' @details
#' The function handles different URL structures for different GAEZ themes:
#' \itemize{
#'   \item Theme 1: Land & Water Resources (static maps)
#'   \item Theme 2: Agro-climatic Resources (time series)
#'   \item Theme 3: Agro-climatic Potential Yield (4-letter crop codes)
#'   \item Theme 4: Suitability & Attainable Yield (3-letter crop codes)
#'   \item Theme 5: Actual Yields & Production (3-letter crop codes)
#'   \item Theme 6: Yield & Production Gaps (3-letter crop codes)
#' }
#'
#' @examples
#' \dontrun{
#' # Basic usage - wheat yield for 2041-2060
#' build_gaez_url()
#'
#' # Specific crop and historical period
#' build_gaez_url(
#'   crop = "maize",
#'   time_period = "HP0120",
#'   climate_model = "AGERA5"
#' )
#'
#' # Using year ranges instead of period codes
#' build_gaez_url(
#'   crop = "sorghum",
#'   start_year = 2041,
#'   end_year = 2060,
#'   ssp = "SSP370"
#' )
#'
#' # Using single year - finds containing period
#' build_gaez_url(
#'   crop = "maize",
#'   start_year = 2050
#' )
#'
#' # Year range spanning multiple periods - prompts for selection
#' build_gaez_url(
#'   crop = "wheat",
#'   start_year = 2030,
#'   end_year = 2070
#' )
#'
#' # Irrigated scenario
#' build_gaez_url(
#'   crop = "rice",
#'   water_management_level = "HILM"
#' )
#' }
#'
#' @export
#' @importFrom dplyr filter pull first
#' @importFrom stringr str_remove str_detect str_ends str_starts
build_gaez_url <- function(variable = "RES05-YX",
                           time_period = "FP4160",
                           start_year = NULL,
                           end_year = NULL,
                           climate_model = NULL,
                           ssp = "SSP370",
                           crop = "WHEA",
                           water_management_level = "HRLM",
                           water_supply = "WSR",
                           resolution = NA) {
  # Normalize inputs to uppercase for consistent matching
  # GAEZ codes are case-sensitive but we accept lowercase for convenience
  variable <- toupper(variable)
  crop <- toupper(crop)
  if (!is.null(climate_model)) climate_model <- toupper(climate_model)
  if (!is.null(ssp)) ssp <- toupper(ssp)
  water_management_level <- toupper(water_management_level)
  water_supply <- toupper(water_supply)

  # Look up variable information
  var_info <- lookup_gaez_variable(variable)

  # Process crop input - use appropriate theme for crop lookup
  crop_code <- lookup_gaez_crop(crop, var_info$theme_number)

  # Handle time period lookup if start_year and/or end_year provided
  if (!is.null(start_year) || !is.null(end_year)) {
    # Get time period data (only rows with time_period defined)
    time_periods_data <- gaez_scenarios |>
      filter(!is.na(time_period), !is.na(start_year), !is.na(end_year))

    # Validate years are within GAEZ range
    all_years <- c(start_year, end_year)
    all_years <- all_years[!is.na(all_years)]

    min_available <- min(time_periods_data$start_year)
    max_available <- max(time_periods_data$end_year)

    if (any(all_years < min_available | all_years > max_available)) {
      invalid_years <- all_years[all_years < min_available | all_years > max_available]
      error_msg <- paste0(
        "Year(s) ", paste(invalid_years, collapse = ", "),
        " outside available GAEZ data range (", min_available, "-", max_available, ").\n\n",
        "Available time periods:\n"
      )

      # Format available periods
      periods_info <- time_periods_data |>
        select(time_period, description, start_year, end_year) |>
        mutate(period_range = paste0(time_period, " (", start_year, "-", end_year, ")"))

      for (i in seq_len(nrow(periods_info))) {
        error_msg <- paste0(error_msg, "  - ", periods_info$period_range[i], "\n")
      }

      stop(error_msg, call. = FALSE)
    }

    # Case 1: Only start_year provided - find period containing this year
    if (!is.null(start_year) && is.null(end_year)) {
      time_lookup <- time_periods_data |>
        filter(start_year <= !!start_year, end_year >= !!start_year)

      if (nrow(time_lookup) == 0) {
        stop(
          paste("No time period found containing year", start_year),
          call. = FALSE
        )
      }

      time_period <- time_lookup$time_period[1]
      period_start <- time_lookup$start_year[1]
      period_end <- time_lookup$end_year[1]

      message(
        paste0(
          "Using time period ", time_period, " (", period_start, "-", period_end, "). ",
          "Note: GAEZ data represents 20-year averages for this period."
        )
      )
    }

    # Case 2: Only end_year provided - find period containing this year
    else if (is.null(start_year) && !is.null(end_year)) {
      time_lookup <- time_periods_data |>
        filter(start_year <= !!end_year, end_year >= !!end_year)

      if (nrow(time_lookup) == 0) {
        stop(
          paste("No time period found containing year", end_year),
          call. = FALSE
        )
      }

      time_period <- time_lookup$time_period[1]
      period_start <- time_lookup$start_year[1]
      period_end <- time_lookup$end_year[1]

      message(
        paste0(
          "Using time period ", time_period, " (", period_start, "-", period_end, "). ",
          "Note: GAEZ data represents 20-year averages for this period."
        )
      )
    }

    # Case 3: Both start_year and end_year provided
    else {
      # Find all periods that overlap with the requested range
      time_lookup <- time_periods_data |>
        filter(
          # Period starts before or during requested range AND ends during or after requested range
          start_year <= !!end_year & end_year >= !!start_year
        )

      if (nrow(time_lookup) == 0) {
        stop(
          paste(
            "No time period found for years",
            start_year,
            "-",
            end_year
          ),
          call. = FALSE
        )
      }

      # Single match or multiple matches in same period
      if (nrow(time_lookup) == 1) {
        time_period <- time_lookup$time_period[1]
        period_start <- time_lookup$start_year[1]
        period_end <- time_lookup$end_year[1]

        message(
          paste0(
            "Using time period ", time_period, " (", period_start, "-", period_end, "). ",
            "Note: GAEZ data represents 20-year averages for this period."
          )
        )
      } else {
        # Multiple time periods match the range - user needs to choose
        message(paste0(
          "The year range ", start_year, "-", end_year,
          " spans multiple GAEZ time periods:"
        ))

        time_lookup_display <- time_lookup |>
          mutate(option = seq_len(n())) |>
          select(option, time_period, start_year, end_year)

        print(time_lookup_display)

        # For automated testing or non-interactive mode, select the first match
        if (interactive() && !isTRUE(getOption("gaez_testing_mode", FALSE))) {
          choice <- as.numeric(readline("Please enter the number of your choice: "))

          if (is.na(choice) || choice < 1 || choice > nrow(time_lookup)) {
            stop("Invalid choice. Please run the function again.", call. = FALSE)
          }

          selected_period <- time_lookup[choice, ]
        } else {
          message("Non-interactive mode: selecting first time period match")
          selected_period <- time_lookup[1, ]
        }

        time_period <- selected_period$time_period
        period_start <- selected_period$start_year
        period_end <- selected_period$end_year

        message(
          paste0(
            "Selected time period ", time_period, " (", period_start, "-", period_end, "). ",
            "Note: GAEZ data represents 20-year averages for this period."
          )
        )
      }
    }
  }

  # Validate and adjust climate model and SSP based on time period
  validation_result <- validate_climate_ssp(time_period, climate_model, ssp)
  climate_model <- validation_result$climate_model
  ssp <- validation_result$ssp

  # Build base URL
  base_url <- "https://storage.googleapis.com/fao-gismgr-gaez-v5-data/DATA/GAEZ-V5"
  data_folder <- var_info$data_folder
  variable_code <- var_info$data_code

  # Build filename components based on variable pattern
  filename_parts <- c("GAEZ-V5")

  # Add components based on variable type (theme-based logic)
  theme_num <- var_info$theme_number
  url_structure <- gaez_url_structure |>
    filter(theme == theme_num) |>
    pull(filename_parts) |>
    first()

  if (!is.null(url_structure)) {
    # Verify url_structure is properly formatted as a list
    if (!is.list(url_structure) || length(url_structure) == 0) {
      stop(
        "URL structure configuration error for theme ", theme_num, ". ",
        "Please report this issue.",
        call. = FALSE
      )
    }

    # Build filename parts based on theme-specific structure
    for (part in url_structure[[1]]) {
      component_value <- switch(
        part,
        "variable_code" = variable_code,
        "time_period" = time_period,
        "climate_model" = climate_model,
        "ssp" = ssp,
        "crop" = crop_code,
        "water_management" = water_management_level,
        "water_supply" = water_supply,
        "year" = time_period,  # Alias for time series
        NULL
      )

      # Add component if valid
      if (!is.null(component_value) &&
          !is.na(component_value) &&
          nchar(as.character(component_value)) > 0) {
        filename_parts <- c(filename_parts, as.character(component_value))
      }
    }

    # Sanity check: ensure we have more than just the base name
    if (length(filename_parts) < 2) {
      warning(
        "URL construction resulted in ", length(filename_parts), " component(s). ",
        "Expected multiple components for complete URL. ",
        "File may not be accessible.",
        call. = FALSE
      )
    }
  } else {
    stop(
      "No URL structure defined for theme ", theme_num, ". ",
      "This theme may not support direct downloads.",
      call. = FALSE
    )
  }

  # Create filename
  filename <- paste(filename_parts, collapse = ".") |>
    paste0(".tif")

  # Build complete URL
  url <- file.path(base_url, data_folder, variable_code, filename)

  # Check for alternative resolutions (1km vs 10km variants)
  base_var_code <- str_remove(var_info$variable_code, "30AS$") # Remove 1km indicator
  alt_resolution_vars <- gaez_variables |>
    filter(
      str_detect(variable_code, paste0("^", base_var_code)),
      variable_code != var_info$variable_code,
      str_detect(variable_name, "km")
    ) |>
    pull(variable_code)

  if (length(alt_resolution_vars) > 0) {
    message(
      "Alternative resolutions available: ",
      paste(alt_resolution_vars, collapse = ", ")
    )
  }

  # Check for time series availability
  if (!str_ends(var_info$data_code, "-TS")) {
    ts_var <- paste0(var_info$data_code, "-TS")
    if (ts_var %in% gaez_variables$data_code) {
      message("Time series version available: ", ts_var)
    }
  }

  # Create descriptive message
  var_desc <- var_info$variable_name

  # Safe lookup with fallback
  time_desc <- gaez_scenarios |>
    filter(time_period == !!time_period) |>
    pull(description) |>
    first()
  if (is.null(time_desc)) time_desc <- time_period

  climate_desc <- gaez_scenarios |>
    filter(climate_model == !!climate_model) |>
    pull(description) |>
    first()
  if (is.null(climate_desc)) climate_desc <- climate_model

  ssp_desc <- gaez_scenarios |>
    filter(ssp == !!ssp) |>
    pull(description) |>
    first()
  if (is.null(ssp_desc) || is.na(ssp_desc)) ssp_desc <- NA_character_

  crop_desc <- gaez_crops |>
    filter(
      gaez_crop_code == crop_code,
      gaez_theme == var_info$theme_number
    ) |>
    pull(name) |>
    first()
  if (is.null(crop_desc)) crop_desc <- crop_code

  message("Building URL for:")
  message(paste("  Variable:", var_desc))
  message(paste("  Time period:", time_desc))
  message(paste("  Climate:", climate_desc))
  if (!is.na(ssp_desc)) {
    message(paste("  Scenario:", ssp_desc))
  }
  message(paste("  Crop:", crop_desc))
  message(paste("  Management:", water_management_level))

  return(url)
}
