#' Get GAEZ v5 Variables Table
#'
#' Returns a table containing information about all GAEZ v5 variables across
#' 6 themes. Use this to discover available data types and their codes.
#'
#' @return A tibble with 133 rows and 7 columns:
#' \describe{
#'   \item{theme_number}{Theme number (1-6)}
#'   \item{theme_name}{Descriptive theme name}
#'   \item{sub_theme_name}{Sub-theme classification}
#'   \item{variable_code}{Variable code (e.g., "RES05-YX")}
#'   \item{variable_name}{Full descriptive name}
#'   \item{data_folder}{Storage folder ("MAP" or "MAPSET")}
#'   \item{data_code}{Data code used in file naming}
#' }
#'
#' @examples
#' \dontrun{
#' # Get all variables
#' vars <- get_gaez_variables()
#' View(vars)
#'
#' # Find yield-related variables
#' library(dplyr)
#' yield_vars <- get_gaez_variables() %>%
#'   filter(grepl("yield", variable_name, ignore.case = TRUE))
#' }
#'
#' @export
get_gaez_variables <- function() {
  gaez_variables
}


#' Get GAEZ v5 Crop Codes Table
#'
#' Returns a table containing crop codes for all GAEZ v5 themes. Different
#' themes use different coding systems (3-letter vs 4-letter codes).
#'
#' @param theme Integer - Optional theme filter (3, 4, 5, or 6).
#'   If NULL (default), returns all crops across all themes.
#'
#' @return A tibble with crop information:
#' \describe{
#'   \item{gaez_crop_code}{Crop code (e.g., "MZE", "MAIZ")}
#'   \item{name}{Full crop name}
#'   \item{gaez_crop_group}{Crop group classification}
#'   \item{gaez_theme}{Theme number (3-6)}
#' }
#'
#' @examples
#' \dontrun{
#' # Get all crops
#' crops <- get_gaez_crops()
#' View(crops)
#'
#' # Get crops for theme 4 only
#' theme4_crops <- get_gaez_crops(theme = 4)
#'
#' # Filter to theme 4 cereals
#' library(dplyr)
#' cereals <- get_gaez_crops(theme = 4) %>%
#'   filter(gaez_crop_group == "Cereals")
#' }
#'
#' @export
#' @importFrom dplyr filter
get_gaez_crops <- function(theme = NULL) {
  if (is.null(theme)) {
    return(gaez_crops)
  }

  if (!theme %in% c(3, 4, 5, 6)) {
    stop("theme must be 3, 4, 5, or 6", call. = FALSE)
  }

  gaez_crops |>
    filter(gaez_theme == theme)
}


#' Get GAEZ v5 Scenarios Table (Internal)
#'
#' Returns reference data for time periods, climate models, SSP scenarios, and
#' water management codes used in GAEZ v5. This is an internal function that
#' returns the full scenarios table. Users should use \code{\link{list_gaez_scenarios}}
#' instead, which provides filtered, user-friendly views of scenario information.
#'
#' @return A tibble with multiple types of scenario information including:
#' \itemize{
#'   \item Time periods (HP8100, HP0120, FP2140, FP4160, FP6180, FP8100)
#'   \item Climate models (AGERA5, ENSEMBLE, GFDL-ESM4, etc.)
#'   \item SSP scenarios (HIST, SSP126, SSP370, SSP585)
#'   \item Water management codes (HRLM, HILM, LRLM, LILM)
#'   \item Water supply codes (WSI, WSR, WST)
#' }
#'
#' @seealso \code{\link{list_gaez_scenarios}} for user-friendly scenario listings
#'
#' @keywords internal
get_gaez_scenarios <- function() {
  gaez_scenarios
}
