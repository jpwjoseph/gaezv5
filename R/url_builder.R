#' Build GAEZ v5 download URL
#'
#' Constructs the download URL of a GAEZ v5 GeoTIFF in the FAO Google Cloud
#' Storage bucket. Validates the parameter combination for the variable's theme
#' and reports alternative resolutions or time-series variants when available.
#'
#' @param variable Character - Variable name or code. Default is "RES05-YX"
#'   (attainable yield). Use \code{lookup_gaez_variable()} to find variable codes.
#' @param time_period Character - Time period code. Default is "FP4160" (2041-2060).
#'   Valid options: "HP8100", "HP0120", "FP2140", "FP4160", "FP6180", "FP8100".
#'   For annual time-series variables (codes ending in "-TS") a year such as
#'   "2010" is accepted.
#' @param start_year Numeric - Start year for time period lookup (optional). Can be
#'   provided alone or with end_year. If only start_year is given, returns the time
#'   period containing that year. If both years are provided and span multiple periods,
#'   prompts for selection. Must be between 1981-2100. For time-series variables,
#'   the year of the layer (1980-2022).
#' @param end_year Numeric - End year for time period lookup (optional). Can be
#'   provided alone or with start_year. If only end_year is given, returns the time
#'   period containing that year. Must be between 1981-2100.
#' @param climate_model Character - Climate model. NULL for auto-selection:
#'   historical periods use "AGERA5"; future periods of theme 4 use "ENSEMBLE".
#'   Themes 2 and 3 have no ENSEMBLE layers, so future periods require one of
#'   "GFDL-ESM4", "IPSL-CM6A-LR", "MPI-ESM1-2-HR", "MRI-ESM2-0", "UKESM1-0-LL".
#' @param ssp Character - SSP scenario. Default is "SSP370". Historical periods
#'   use "HIST". Future periods use "SSP126", "SSP370", or "SSP585".
#' @param crop Character - Crop name or code. Default is "WHEA" (wheat).
#'   Use \code{lookup_gaez_crop()} to find crop codes. Ignored for themes 1 and 2.
#' @param water_management_level Character - Input/management code for themes
#'   3-4. Default is "HRLM" (rain-fed, high input). Other options: "HILM"
#'   (irrigated, high input), "LRLM" (rain-fed, low input), "LILM" (irrigated,
#'   low input).
#' @param water_supply Character - Water supply code for actual yields/gaps
#'   (themes 5-6). Default is "WSR" (rain-fed). Other options: "WSI" (irrigated),
#'   "WST" (total).
#' @param resolution Character - "1km", "10km", or NA. NA selects the 10 km layer
#'   when a variable exists at both resolutions. The ~1 km layers of theme 4 cover
#'   the historical periods HP8100 and HP0120 only.
#' @param interactive Logical - Whether to prompt for user input when multiple
#'   matches are found (for crops or time periods). Default is TRUE. Set to FALSE
#'   to automatically select the best match without prompting.
#' @param class Character - Class code for theme-1 variables published as one
#'   raster per class: "LR-LCC" (land cover, "LC01"-"LC12") and "LR-SLC" (slope
#'   class, "SC01"-"SC10"). Ignored for other variables.
#'
#' @return Character string containing the complete HTTPS download URL
#'
#' @details
#' File names follow the FAO naming convention for each theme:
#' \itemize{
#'   \item Theme 1: \code{MAP/GAEZ-V5.<variable>.tif}, or
#'     \code{MAPSET/<variable>/GAEZ-V5.<variable>.<class>.tif}
#'   \item Theme 2: \code{GAEZ-V5.<variable>.<period>.<climate>.<ssp>.tif};
#'     time series: \code{GAEZ-V5.<variable>-TS.<year>.tif}
#'   \item Themes 3-4: \code{GAEZ-V5.<variable>.<period>.<climate>.<ssp>.<crop>.<input>.tif}
#'   \item Themes 5-6: \code{GAEZ-V5.<variable>.<crop>.<water_supply>.tif}
#' }
#'
#' The bucket root defaults to
#' \code{https://storage.googleapis.com/fao-gismgr-gaez-v5-data/DATA/GAEZ-V5} and
#' can be changed with \code{options(gaezv5.gcs_root = "<url>")}.
#'
#' @examples
#' \dontrun{
#' # Basic usage - wheat yield for 2041-2060
#' build_gaez_url()
#'
#' # Specific crop and historical period
#' build_gaez_url(crop = "maize", time_period = "HP0120")
#'
#' # Using year ranges instead of period codes
#' build_gaez_url(crop = "sorghum", start_year = 2041, end_year = 2060)
#'
#' # ~1 km suitability layer
#' build_gaez_url(variable = "RES05-SX", crop = "maize", time_period = "HP0120",
#'                resolution = "1km")
#'
#' # Theme 1: single map and per-class mapset
#' build_gaez_url(variable = "AEZ57")
#' build_gaez_url(variable = "LR-LCC", class = "LC02")
#'
#' # Theme 2: future periods need an individual climate model
#' build_gaez_url(variable = "RES01-PRC", time_period = "FP4160",
#'                climate_model = "MRI-ESM2-0")
#'
#' # Annual time series
#' build_gaez_url(variable = "RES01-PRC-TS", start_year = 2010)
#' }
#'
#' @export
build_gaez_url <- function(variable = "RES05-YX",
                           time_period = "FP4160",
                           start_year = NULL,
                           end_year = NULL,
                           climate_model = NULL,
                           ssp = "SSP370",
                           crop = "WHEA",
                           water_management_level = "HRLM",
                           water_supply = "WSR",
                           resolution = NA,
                           interactive = TRUE,
                           class = NULL) {
  req <- resolve_gaez_request(
    variable = variable,
    time_period = time_period,
    start_year = start_year,
    end_year = end_year,
    climate_model = climate_model,
    ssp = ssp,
    crop = crop,
    water_management_level = water_management_level,
    water_supply = water_supply,
    resolution = resolution,
    class = class,
    interactive = interactive
  )
  report_gaez_request(req)
  gaez_request_url(req)
}


#' Root URL of the GAEZ v5 data in Google Cloud Storage
#' @noRd
gaez_root <- function() {
  getOption(
    "gaezv5.gcs_root",
    "https://storage.googleapis.com/fao-gismgr-gaez-v5-data/DATA/GAEZ-V5"
  )
}


#' Resolve and validate a GAEZ request
#'
#' Returns the variable record and the dimension codes that make up the file
#' name. Dimensions that do not apply to the variable's theme are NA.
#' @noRd
resolve_gaez_request <- function(variable,
                                 time_period = "FP4160",
                                 start_year = NULL,
                                 end_year = NULL,
                                 climate_model = NULL,
                                 ssp = "SSP370",
                                 crop = "WHEA",
                                 water_management_level = "HRLM",
                                 water_supply = "WSR",
                                 resolution = NA,
                                 class = NULL,
                                 interactive = TRUE,
                                 verbose = TRUE) {
  var_info <- resolve_gaez_variable(variable, resolution)
  theme <- var_info$theme_number

  req <- list(
    var_info = var_info,
    theme_number = theme,
    crop_code = NA_character_,
    time_period = NA_character_,
    climate_model = NA_character_,
    ssp = NA_character_,
    water_management_level = NA_character_,
    water_supply = NA_character_,
    class = NA_character_,
    year = NA_character_
  )

  if (theme == 1 && var_info$data_folder == "MAPSET") {
    req$class <- resolve_gaez_class(class, var_info$data_code)
  }

  if (theme == 9) {
    req$year <- resolve_gaez_year(time_period, start_year, end_year)
  }

  if (theme %in% c(2, 3, 4)) {
    period <- lookup_time_period_from_years(
      start_year = start_year,
      end_year = end_year,
      time_period = time_period,
      interactive = interactive,
      verbose = verbose
    )
    if (!is.null(climate_model)) climate_model <- toupper(climate_model)
    if (!is.null(ssp)) ssp <- toupper(ssp)
    climate <- validate_climate_ssp(period, climate_model, ssp, theme = theme)

    if (var_info$resolution == "1km" && startsWith(period, "FP")) {
      stop(
        "The ~1 km layer ", var_info$data_code, " covers HP8100 and HP0120 only. ",
        "Use resolution = \"10km\" for future periods.",
        call. = FALSE
      )
    }
    req$time_period <- period
    req$climate_model <- climate$climate_model
    req$ssp <- climate$ssp
  }

  if (theme %in% c(3, 4, 5, 6)) {
    req$crop_code <- lookup_gaez_crop(toupper(crop), theme, interactive = interactive)
  }

  if (theme %in% c(3, 4)) {
    req$water_management_level <- check_gaez_code(
      toupper(water_management_level), "water_management_level",
      "water_management_code", "water_management"
    )
  }

  if (theme %in% c(5, 6)) {
    req$water_supply <- check_gaez_code(
      toupper(water_supply), "water_supply", "water_supply", "water_supply"
    )
  }

  req
}


#' Select the variable record, honouring the requested resolution
#'
#' A code can match two records (~10 km and ~1 km). An exact data-code match
#' wins; otherwise the 10 km record is used unless `resolution` says otherwise.
#' @noRd
resolve_gaez_variable <- function(variable, resolution = NA) {
  if (missing(variable) || is.null(variable) || is.na(variable) || variable == "") {
    stop("Please provide a variable code, data code, or variable name.", call. = FALSE)
  }
  resolution <- normalize_resolution(resolution)
  code <- toupper(variable)

  hits <- gaez_variables[toupper(gaez_variables$variable_code) == code |
                           toupper(gaez_variables$data_code) == code, ]
  if (nrow(hits) == 0) {
    hits <- lookup_gaez_variable(variable)
  }
  family <- gaez_variables[gaez_variables$variable_code %in% hits$variable_code, ]

  if (!is.na(resolution)) {
    match <- family[family$resolution == resolution, ]
    if (nrow(match) == 0) {
      stop(
        "No ", resolution, " layer for ", hits$variable_code[1], ". Available: ",
        paste(unique(family$resolution), collapse = ", "),
        call. = FALSE
      )
    }
    return(match[1, ])
  }

  exact <- hits[toupper(hits$data_code) == code, ]
  if (nrow(exact) == 1) {
    return(exact)
  }
  coarse <- hits[hits$resolution == "10km", ]
  if (nrow(coarse) > 0) coarse[1, ] else hits[1, ]
}


#' @noRd
normalize_resolution <- function(resolution) {
  if (is.null(resolution) || length(resolution) == 0 || is.na(resolution[1])) {
    return(NA_character_)
  }
  value <- tolower(gsub("[^[:alnum:]]", "", resolution[1]))
  if (value %in% c("1km", "30as")) return("1km")
  if (value %in% c("10km", "5am")) return("10km")
  stop("resolution must be \"1km\", \"10km\" or NA", call. = FALSE)
}


#' @noRd
resolve_gaez_class <- function(class, data_code) {
  options <- gaez_classes[gaez_classes$data_code == data_code, ]
  listing <- paste0("  ", options$class_code, ": ", options$description, collapse = "\n")

  if (is.null(class) || length(class) == 0 || is.na(class[1]) || class[1] == "") {
    stop(
      data_code, " is published as one raster per class. Set `class` to one of:\n",
      listing,
      call. = FALSE
    )
  }
  value <- toupper(class[1])
  if (!value %in% options$class_code) {
    stop("Invalid class '", class[1], "' for ", data_code, ". Valid classes:\n", listing,
         call. = FALSE)
  }
  value
}


#' @noRd
resolve_gaez_year <- function(time_period, start_year = NULL, end_year = NULL) {
  if (!is.null(start_year) && !is.null(end_year) && start_year != end_year) {
    stop(
      "Time-series layers are annual. Request one year, or pass several years to ",
      "batch_download_gaez_datasets(time_periods = ...).",
      call. = FALSE
    )
  }
  given <- if (!is.null(start_year)) start_year else if (!is.null(end_year)) end_year else time_period
  year <- suppressWarnings(as.integer(given))

  if (length(year) != 1 || is.na(year) || !year %in% gaez_ts_years) {
    stop(
      "Time-series layers cover ", min(gaez_ts_years), "-", max(gaez_ts_years),
      ". Set start_year (or time_period) to a single year in that range; got '",
      paste(given, collapse = ", "), "'.",
      call. = FALSE
    )
  }
  as.character(year)
}


#' Validate a code against the scenario table and list valid options on failure
#' @noRd
check_gaez_code <- function(value, argument, column, scenario_type) {
  table <- gaez_scenarios[!is.na(gaez_scenarios[[column]]), ]
  if (length(value) == 1 && value %in% table[[column]]) {
    return(value)
  }
  stop(
    "Invalid ", argument, ": '", paste(value, collapse = ", "), "'\n\nValid options:\n",
    paste0("  - ", table[[column]], ": ", table$description, collapse = "\n"),
    "\n\nUse list_gaez_scenarios('", scenario_type, "') for more details.",
    call. = FALSE
  )
}


#' Assemble the download URL of a resolved request
#' @noRd
gaez_request_url <- function(req) {
  var_info <- req$var_info
  parts <- gaez_url_structure$filename_parts[[
    which(gaez_url_structure$theme == req$theme_number)
  ]][[1]]

  values <- vapply(parts, function(part) {
    switch(part,
      variable_code = var_info$data_code,
      time_period = req$time_period,
      climate_model = req$climate_model,
      ssp = req$ssp,
      crop = req$crop_code,
      water_management = req$water_management_level,
      water_supply = req$water_supply,
      year = req$year,
      class = req$class,
      NA_character_
    )
  }, character(1))
  values <- values[!is.na(values) & nzchar(values)]

  filename <- paste0(paste(c("GAEZ-V5", values), collapse = "."), ".tif")

  if (var_info$data_folder == "MAP") {
    paste(gaez_root(), "MAP", filename, sep = "/")
  } else {
    paste(gaez_root(), "MAPSET", var_info$data_code, filename, sep = "/")
  }
}


#' Describe a resolved request and point to related layers
#' @noRd
report_gaez_request <- function(req) {
  var_info <- req$var_info

  siblings <- gaez_variables[gaez_variables$variable_code == var_info$variable_code &
                               gaez_variables$data_code != var_info$data_code, ]
  if (nrow(siblings) > 0) {
    message(
      "Other resolution available: ",
      paste0(siblings$data_code, " (resolution = \"", siblings$resolution, "\")",
             collapse = ", ")
    )
  }

  ts_code <- paste0(var_info$data_code, "-TS")
  if (req$theme_number != 9 && ts_code %in% gaez_variables$data_code) {
    message("Time series version available: ", ts_code)
  }

  describe <- function(column, value) {
    if (is.na(value)) return(NA_character_)
    hit <- gaez_scenarios$description[which(gaez_scenarios[[column]] == value)[1]]
    if (length(hit) == 0 || is.na(hit)) value else hit
  }

  message("Building URL for:")
  message("  Variable: ", var_info$variable_name)
  if (!is.na(req$class)) {
    class_desc <- gaez_classes$description[gaez_classes$data_code == var_info$data_code &
                                             gaez_classes$class_code == req$class]
    message("  Class: ", req$class, " (", class_desc, ")")
  }
  if (!is.na(req$year)) message("  Year: ", req$year)
  if (!is.na(req$time_period)) {
    message("  Time period: ", describe("time_period", req$time_period))
    message("  Climate: ", describe("climate_model", req$climate_model))
    message("  Scenario: ", describe("ssp", req$ssp))
  }
  if (!is.na(req$crop_code)) {
    crop_desc <- gaez_crops$name[gaez_crops$gaez_crop_code == req$crop_code &
                                   gaez_crops$gaez_theme == req$theme_number][1]
    message("  Crop: ", if (is.na(crop_desc)) req$crop_code else crop_desc)
  }
  if (!is.na(req$water_management_level)) {
    message("  Water management: ", req$water_management_level)
  }
  if (!is.na(req$water_supply)) {
    message("  Water supply: ", req$water_supply)
  }
  invisible(req)
}
