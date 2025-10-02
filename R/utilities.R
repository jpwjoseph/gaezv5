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
