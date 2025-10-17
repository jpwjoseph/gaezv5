#' @section Main functions:
#' \itemize{
#'   \item \code{\link{lookup_gaez_variable}}: Find variable codes
#'   \item \code{\link{lookup_gaez_crop}}: Find crop codes
#'   \item \code{\link{build_gaez_url}}: Construct download URLs
#'   \item \code{\link{download_gaez_dataset}}: Download single dataset
#'   \item \code{\link{batch_download_gaez_datasets}}: Download multiple datasets
#'   \item \code{\link{list_gaez_crops}}: List available crops
#'   \item \code{\link{list_gaez_scenarios}}: List time periods, climate models, SSPs
#'   \item \code{\link{list_downloaded_files}}: Inventory of local files
#'   \item \code{\link{check_url_exists}}: Validate URLs before download
#' }
#'
#' @section Data access functions:
#' \itemize{
#'   \item \code{\link{get_gaez_variables}}: Get table of all GAEZ variables
#'   \item \code{\link{get_gaez_crops}}: Get table of all crop codes
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
#' @keywords internal
"_PACKAGE"
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
