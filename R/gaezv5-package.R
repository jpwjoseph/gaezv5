#' @keywords internal
"_PACKAGE"

#' gaezv5: Download and Process GAEZ v5 Data
#'
#' The gaezv5 package provides tools for downloading and working with GAEZ
#' (Global Agro-Ecological Zones) version 5 data from the FAO. GAEZ v5 provides
#' comprehensive global data on agricultural potential, crop suitability, yield
#' estimates, and agro-climatic resources.
#'
#' @section Main functions:
#' \itemize{
#'   \item \code{\link{lookup_gaez_variable}}: Find variable codes
#'   \item \code{\link{lookup_gaez_crop}}: Find crop codes
#'   \item \code{\link{build_gaez_url}}: Construct download URLs
#'   \item \code{\link{download_gaez_dataset}}: Download single dataset
#'   \item \code{\link{batch_download_gaez_datasets}}: Download multiple datasets
#'   \item \code{\link{list_gaez_crops}}: List available crops
#'   \item \code{\link{list_downloaded_files}}: Inventory of local files
#'   \item \code{\link{check_url_exists}}: Validate URLs before download
#' }
#'
#' @section Data objects:
#' \itemize{
#'   \item \code{\link{gaez_variables}}: Table of all GAEZ variables
#'   \item \code{\link{gaez_crops}}: Table of all crop codes
#'   \item \code{\link{gaez_scenarios}}: Time periods, climate models, SSPs
#'   \item \code{\link{gaez_url_structure}}: URL construction patterns
#' }
#'
#' @section Getting started:
#' To get started with gaezv5:
#'
#' \preformatted{
#' # 1. Browse available crops
#' list_gaez_crops()
#'
#' # 2. Download a dataset
#' result <- download_gaez_dataset(
#'   crop = "maize",
#'   time_period = "HP0120",
#'   climate_model = "AGERA5"
#' )
#'
#' # 3. Load the raster
#' library(terra)
#' r <- rast(result$file_path)
#' plot(r)
#' }
#'
#' @section GAEZ v5 Themes:
#' GAEZ v5 data is organized into 6 themes:
#' \enumerate{
#'   \item Land and Water Resources
#'   \item Agro-climatic Resources
#'   \item Agro-climatic Potential Yield
#'   \item Suitability and Attainable Yield
#'   \item Actual Yields and Production
#'   \item Yield and Production Gaps
#' }
#'
#' @section Learn more:
#' \itemize{
#'   \item GAEZ v5 portal: \url{https://gaez.fao.org/}
#'   \item Package vignettes: \code{browseVignettes("gaezv5")}
#'   \item Examples: \code{show_gaez_examples()}
#' }
#'
#' @docType package
#' @name gaezv5-package
#' @aliases gaezv5
#'
#' @author Julian Joseph \email{joseph@@iiasa.ac.at}
#'
#' @importFrom dplyr filter mutate select arrange group_by summarise bind_rows pull first
#' @importFrom stringr str_detect str_to_lower str_to_upper str_starts str_ends str_remove str_extract
#' @importFrom tibble tibble
#' @importFrom httr HEAD status_code headers timeout
#' @importFrom tools file_ext
#' @importFrom tidyr tribble
#' @importFrom purrr map
NULL


## usethis namespace: start
## usethis namespace: end
NULL

#' GAEZ v5 Variables
#'
#' A dataset containing information about all GAEZ v5 variables across
#' 6 themes. Use this to discover available data types and their codes.
#'
#' @format A tibble with 133 rows and 7 columns:
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
#' # Browse variables
#' View(gaez_variables)
#'
#' # Find yield-related variables
#' subset(gaez_variables, grepl("yield", variable_name, ignore.case = TRUE))
#' }
"gaez_variables"


#' GAEZ v5 Crop Codes
#'
#' A dataset containing crop codes for all GAEZ v5 themes. Different themes
#' use different coding systems (3-letter vs 4-letter codes).
#'
#' @format A tibble with crop information:
#' \describe{
#'   \item{gaez_crop_code}{Crop code (e.g., "MZE", "MAIZ")}
#'   \item{name}{Full crop name}
#'   \item{gaez_crop_group}{Crop group classification}
#'   \item{gaez_theme}{Theme number (3-6)}
#' }
#'
#' @examples
#' \dontrun{
#' # Browse crops
#' View(gaez_crops)
#'
#' # Filter to theme 4 cereals
#' subset(gaez_crops, gaez_theme == 4 & gaez_crop_group == "Cereals")
#' }
"gaez_crops"


#' GAEZ v5 Scenarios
#'
#' Reference data for time periods, climate models, SSP scenarios, and
#' water management codes used in GAEZ v5.
#'
#' @format A tibble with multiple types of scenario information
#'
#' @examples
#' \dontrun{
#' View(gaez_scenarios)
#' }
"gaez_scenarios"


#' GAEZ v5 URL Structure
#'
#' Internal reference data describing how URLs are constructed for different
#' GAEZ themes.
#'
#' @format A tibble mapping themes to URL components
#'
#' @keywords internal
"gaez_url_structure"
