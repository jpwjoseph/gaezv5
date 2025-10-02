#' GAEZ v5 Variables
#'
#' A comprehensive dataset containing information about all GAEZ v5 variables
#' across 6 themes. This table is essential for discovering available data types
#' and their corresponding codes for use with other package functions.
#'
#' @format A tibble with 133 rows and 7 variables:
#' \describe{
#'   \item{theme_number}{Integer (1-6). The GAEZ theme number. Themes are:
#'     1=Land & Water Resources, 2=Agro-climatic Resources,
#'     3=Agro-climatic Potential Yield, 4=Suitability & Attainable Yield,
#'     5=Actual Yields & Production, 6=Yield & Production Gaps}
#'   \item{theme_name}{Character. Descriptive name of the theme}
#'   \item{sub_theme_name}{Character. Sub-theme classification within each theme}
#'   \item{variable_code}{Character. The variable code used for lookup
#'     (e.g., "RES05-YX", "RES02-YLD")}
#'   \item{variable_name}{Character. Full descriptive name of the variable}
#'   \item{data_folder}{Character. Storage folder in GAEZ system
#'     (either "MAP" or "MAPSET")}
#'   \item{data_code}{Character. The code used in actual file naming,
#'     which may differ from variable_code}
#' }
#'
#' @source FAO GAEZ v5 Data Portal \url{https://gaez.fao.org/}
#'
#' @examples
#' # View all variables
#' head(gaez_variables)
#'
#' # Find yield-related variables
#' yield_vars <- subset(gaez_variables,
#'                      grepl("yield", variable_name, ignore.case = TRUE))
#' print(yield_vars[, c("variable_code", "variable_name")])
#'
#' # Variables by theme
#' table(gaez_variables$theme_name)
"gaez_variables"


#' GAEZ v5 Crop Codes
#'
#' A comprehensive dataset of crop codes used across different GAEZ v5 themes.
#' Different themes use different coding systems: Theme 3 uses 4-letter codes
#' (Module II), while Themes 4-6 use 3-letter codes (Modules V-VI).
#'
#' @format A tibble with multiple rows and 4 variables:
#' \describe{
#'   \item{gaez_crop_code}{Character. The crop code used in GAEZ URLs.
#'     Examples: "MZE" (maize, theme 4), "MAIZ" (maize, theme 3)}
#'   \item{name}{Character. Full common name of the crop}
#'   \item{gaez_crop_group}{Character. Crop group classification
#'     (e.g., "Cereals", "Oil crops", "Pulses"). May be NA for aggregate groups}
#'   \item{gaez_theme}{Integer (3-6). The GAEZ theme this crop code applies to}
#' }
#'
#' @details
#' The crop codes vary by theme:
#' \itemize{
#'   \item Theme 3 (Agro-climatic Potential Yield): 4-letter codes like "MAIZ", "WHEA"
#'   \item Theme 4 (Suitability & Attainable Yield): 3-letter codes like "MZE", "WHE"
#'   \item Theme 5 (Actual Yields): 3-letter codes including aggregates like "ALL", "CER"
#'   \item Theme 6 (Yield Gaps): 3-letter codes including aggregates like "GAP", "GCE"
#' }
#'
#' @source FAO GAEZ v5 Data Portal \url{https://gaez.fao.org/}
#'
#' @examples
#' # View crops for theme 4 (most common)
#' theme4_crops <- subset(gaez_crops, gaez_theme == 4)
#' head(theme4_crops)
#'
#' # Find all cereal crops
#' cereals <- subset(gaez_crops,
#'                   gaez_crop_group == "Cereals" & gaez_theme == 4)
#' print(cereals[, c("gaez_crop_code", "name")])
#'
#' # Compare coding systems
#' maize_codes <- subset(gaez_crops, grepl("Maize", name, ignore.case = TRUE))
#' print(maize_codes[, c("gaez_theme", "gaez_crop_code", "name")])
"gaez_crops"


#' GAEZ v5 Scenarios
#'
#' Reference data for time periods, climate models, SSP scenarios, and water
#' management codes used in GAEZ v5. This combined table includes all scenario
#' components needed for constructing download URLs.
#'
#' @format A tibble with multiple rows containing scenario information.
#'   Different rows contain different types of scenario data:
#' \describe{
#'   \item{Time periods}{Rows with time_period, description, start_year, end_year}
#'   \item{Climate models}{Rows with climate_model, description}
#'   \item{SSP scenarios}{Rows with ssp, description}
#'   \item{Water management}{Rows with water_management_code, description}
#'   \item{Water supply}{Rows with water_supply, description}
#' }
#'
#' @details
#' Time periods include:
#' \itemize{
#'   \item HP8100: Historical Period 1981-2000
#'   \item HP0120: Historical Period 2001-2020
#'   \item FP2140: Future Period 2021-2040
#'   \item FP4160: Future Period 2041-2060
#'   \item FP6180: Future Period 2061-2080
#'   \item FP8100: Future Period 2081-2100
#' }
#'
#' Climate models include AGERA5 (historical only), plus ENSEMBLE, GFDL-ESM4,
#' IPSL-CM6A-LR, MPI-ESM1-2-HR, MRI-ESM2-0, UKESM1-0-LL (future periods).
#'
#' SSP scenarios: HIST (historical), SSP126, SSP370, SSP585 (future periods).
#'
#' Water management: HRLM (high input, rain-fed), HILM (high input, irrigated),
#' LRLM (low input, rain-fed), LILM (low input, irrigated).
#'
#' @source FAO GAEZ v5 Data Portal \url{https://gaez.fao.org/}
#'
#' @examples
#' # View time periods
#' time_periods <- subset(gaez_scenarios, !is.na(time_period))
#' print(time_periods[, c("time_period", "description", "start_year", "end_year")])
#'
#' # View climate models
#' climate_models <- subset(gaez_scenarios, !is.na(climate_model))
#' print(climate_models[, c("climate_model", "description")])
"gaez_scenarios"


#' GAEZ v5 URL Structure
#'
#' Internal reference data describing how download URLs are constructed for
#' different GAEZ themes. This table defines which URL components are needed
#' for each theme.
#'
#' @format A tibble with 7 rows and 2 variables:
#' \describe{
#'   \item{theme}{Integer. The GAEZ theme number (1-6, plus 9 for time series)}
#'   \item{filename_parts}{List. Vector of component names needed in the URL
#'     for that theme}
#' }
#'
#' @details
#' This is primarily for internal use by \code{\link{build_gaez_url}}.
#' Different themes require different URL components:
#' \itemize{
#'   \item Theme 1: Just variable_code (static maps)
#'   \item Themes 2-4: Include time period, climate, SSP, crop, water management
#'   \item Themes 5-6: Include crop and water supply
#'   \item Theme 9: Time series variants
#' }
#'
#' @keywords internal
"gaez_url_structure"
