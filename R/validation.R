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
