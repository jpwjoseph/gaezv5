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
