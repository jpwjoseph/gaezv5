#' Download GAEZ v5 Dataset
#'
#' Downloads a GAEZ v5 dataset from Google Cloud Storage with comprehensive
#' validation, duplicate detection, and progress reporting. Supports automatic
#' resumption if files already exist and intelligent path resolution across
#' multiple common locations.
#'
#' @param variable Character - Variable code (e.g., "RES05-YX", "RES02-YLD").
#'   Default is "RES05-YX" (attainable yield).
#' @param time_period Character - Time period code (default: "FP4160"). See
#'   \code{?build_gaez_url} for valid options.
#' @param start_year Numeric - Start year for time period lookup (optional)
#' @param end_year Numeric - End year for time period lookup (optional)
#' @param climate_model Character - Climate model (auto-selected if NULL)
#' @param ssp Character - SSP scenario (default: "SSP370")
#' @param crop Character - Crop name or code (default: "WHEA" for wheat)
#' @param water_management_level Character - Water management level (default: "HRLM")
#' @param water_supply Character - Water supply code (default: "WSR")
#' @param resolution Character - Resolution ("1km", "10km", or NA)
#' @param download_dir Character - Custom download directory. If NULL, uses
#'   "Data/GAEZ/" or creates it if it doesn't exist.
#' @param overwrite Logical - Whether to overwrite existing files (default: FALSE).
#'   If FALSE and file exists, returns immediately without downloading.
#' @param validate_inputs Logical - Whether to validate inputs before download
#'   (default: TRUE). Reserved for future use.
#' @param verbose Logical - Whether to show detailed progress messages (default: TRUE)
#'
#' @return A list with the following elements:
#'   \itemize{
#'     \item \code{success}: Logical indicating if download succeeded
#'     \item \code{file_path}: Full path to the downloaded file
#'     \item \code{url}: The download URL used
#'     \item \code{message}: Status message
#'     \item \code{validation_errors}: Character vector of any validation errors
#'     \item \code{file_size}: File size in bytes
#'     \item \code{download_time}: Time taken for download in seconds
#'     \item \code{already_exists}: Logical indicating if file already existed
#'   }
#'
#' @details
#' The function performs the following steps:
#' \enumerate{
#'   \item Builds the download URL using \code{build_gaez_url()}
#'   \item Determines the download directory and checks for existing files
#'     in multiple common locations
#'   \item Performs a HEAD request to validate the URL exists
#'   \item Downloads the file with progress reporting
#'   \item Validates the downloaded file (checks file size, extension)
#' }
#'
#' The function automatically searches for existing downloads in:
#' \itemize{
#'   \item The specified download_dir
#'   \item Data/GAEZ/
#'   \item GAEZ/ (in working directory)
#'   \item Data/GAEZ_Niger_Analysis/ (legacy location)
#' }
#'
#' @examples
#' \dontrun{
#' # Basic download - wheat yield, future period
#' result <- download_gaez_dataset()
#'
#' # Download specific crop for historical period
#' result <- download_gaez_dataset(
#'   crop = "maize",
#'   time_period = "HP0120",
#'   climate_model = "AGERA5"
#' )
#'
#' # Download with year specification
#' result <- download_gaez_dataset(
#'   crop = "sorghum",
#'   start_year = 2041,
#'   end_year = 2060,
#'   ssp = "SSP370"
#' )
#'
#' # Check result
#' if (result$success) {
#'   print(paste("Downloaded:", result$file_path))
#'   print(paste("Size:", round(result$file_size / 1024^2, 2), "MB"))
#' }
#' }
#'
#' @seealso \code{\link{build_gaez_url}}, \code{\link{batch_download_gaez_datasets}}
#'
#' @export
#' @importFrom httr HEAD status_code headers
#' @importFrom tools file_ext
download_gaez_dataset <- function(variable = "RES05-YX",
                                   time_period = "FP4160",
                                   start_year = NULL,
                                   end_year = NULL,
                                   climate_model = NULL,
                                   ssp = "SSP370",
                                   crop = "WHEA",
                                   water_management_level = "HRLM",
                                   water_supply = "WSR",
                                   resolution = NA,
                                   download_dir = NULL,
                                   overwrite = FALSE,
                                   validate_inputs = TRUE,
                                   verbose = TRUE) {
  # Initialize result object
  result <- list(
    success = FALSE,
    file_path = NULL,
    url = NULL,
    message = NULL,
    validation_errors = character(0),
    file_size = NULL,
    download_time = NULL,
    already_exists = FALSE
  )

  if (verbose) {
    cat("=== GAEZ v5 Dataset Download ===\n")
    cat("Variable:", variable, "\n")
    cat("Crop:", crop, "\n")
    cat("Time period:", time_period, "\n")
  }

  # ====================
  # 1. BUILD URL
  # ====================

  if (verbose) {
    cat("\n[1/5] Building download URL...\n")
  }

  # Attempt to build download URL
  url_result <- tryCatch({
    # Enable testing mode to suppress interactive prompts
    old_option <- getOption("gaez_testing_mode")
    options(gaez_testing_mode = TRUE)

    url <- build_gaez_url(
      variable = variable,
      time_period = time_period,
      start_year = start_year,
      end_year = end_year,
      climate_model = climate_model,
      ssp = ssp,
      crop = crop,
      water_management_level = water_management_level,
      water_supply = water_supply,
      resolution = resolution
    )

    # Restore user's testing mode setting
    options(gaez_testing_mode = old_option)

    # Return success indicator with URL
    list(success = TRUE, url = url, error = NULL)
  }, error = function(e) {
    # Restore setting even on error
    options(gaez_testing_mode = old_option)

    # Capture error message for reporting
    list(success = FALSE, url = NULL, error = conditionMessage(e))
  })

  # Handle URL building failure
  if (!url_result$success) {
    result$message <- paste("Failed to build URL:", url_result$error)
    result$validation_errors <- c(result$validation_errors, url_result$error)

    if (verbose) {
      cat("\u2717 URL building failed:", url_result$error, "\n")
    }
    return(result)
  }

  # Extract successfully built URL
  result$url <- url_result$url

  if (verbose) {
    cat("\u2713 URL built successfully\n")
    cat("   URL:", result$url, "\n")
  }

  # ====================
  # 2. DETERMINE DOWNLOAD PATHS
  # ====================

  if (verbose) {
    cat("\n[2/5] Setting up download paths...\n")
  }

  # Extract filename from URL
  filename <- basename(url)

  # Determine download directory
  if (is.null(download_dir)) {
    # Check for Data/GAEZ/ directory first
    data_gaez_dir <- file.path("Data", "GAEZ")
    working_gaez_dir <- "GAEZ"

    if (dir.exists(data_gaez_dir)) {
      download_dir <- data_gaez_dir
    } else if (dir.exists(working_gaez_dir)) {
      download_dir <- working_gaez_dir
    } else {
      # Create Data/GAEZ/ directory
      download_dir <- data_gaez_dir
      dir.create(download_dir, recursive = TRUE, showWarnings = FALSE)
    }
  }

  # Create download directory if it doesn't exist
  if (!dir.exists(download_dir)) {
    dir.create(download_dir, recursive = TRUE, showWarnings = FALSE)
  }

  # Full file path
  file_path <- file.path(download_dir, filename)
  result$file_path <- file_path

  if (verbose) {
    cat("   Download directory:", download_dir, "\n")
    cat("   Filename:", filename, "\n")
  }

  # ====================
  # 3. CHECK FOR EXISTING FILE
  # ====================

  if (verbose) {
    cat("\n[3/5] Checking for existing files...\n")
  }

  # Check multiple possible locations
  check_paths <- c(
    file_path, # Primary location
    file.path("Data", "GAEZ", filename), # Data/GAEZ/
    file.path("GAEZ", filename), # Working directory GAEZ/
    file.path("Data", "GAEZ_Niger_Analysis", filename) # Existing analysis folder
  )

  existing_file <- NULL
  for (path in check_paths) {
    if (file.exists(path)) {
      existing_file <- path
      break
    }
  }

  if (!is.null(existing_file) && !overwrite) {
    result$success <- TRUE
    result$already_exists <- TRUE
    result$file_path <- existing_file
    result$file_size <- file.size(existing_file)
    result$message <- "File already exists (use overwrite=TRUE to re-download)"

    if (verbose) {
      cat("\u2713 File already exists:\n")
      cat("   Path:", existing_file, "\n")
      cat("   Size:", round(result$file_size / 1024^2, 2), "MB\n")
    }
    return(result)
  } else if (!is.null(existing_file) && overwrite) {
    if (verbose) cat("   Existing file found, will overwrite\n")
  } else {
    if (verbose) cat("   No existing file found\n")
  }

  # ====================
  # 4. DOWNLOAD FILE
  # ====================

  if (verbose) {
    cat("\n[4/5] Downloading file...\n")
  }

  # Check if URL exists before downloading
  tryCatch(
    {
      response <- HEAD(url)
      if (status_code(response) != 200) {
        result$message <- paste(
          "File not available on server (HTTP",
          status_code(response),
          ")"
        )
        if (verbose) {
          cat(
            "\u2717 File not available on server (HTTP",
            status_code(response),
            ")\n"
          )
        }
        return(result)
      }

      # Get file size from headers
      content_length <- headers(response)$`content-length`
      if (!is.null(content_length)) {
        expected_size <- as.numeric(content_length)
        if (verbose) {
          cat(
            "   Expected file size:",
            round(expected_size / 1024^2, 2),
            "MB\n"
          )
        }
      }
    },
    error = function(e) {
      if (verbose) cat("   Warning: Could not check file availability\n")
    }
  )

  # Download the file
  start_time <- Sys.time()

  tryCatch(
    {
      download_result <- download.file(
        url = url,
        destfile = file_path,
        method = "auto",
        mode = "wb",
        quiet = !verbose
      )

      end_time <- Sys.time()
      result$download_time <- as.numeric(end_time - start_time, units = "secs")

      if (download_result == 0) {
        # Successful download
        result$success <- TRUE
        result$file_size <- file.size(file_path)
        result$message <- "Download completed successfully"

        if (verbose) {
          cat("\u2713 Download completed successfully\n")
          cat("   File size:", round(result$file_size / 1024^2, 2), "MB\n")
          cat("   Download time:", round(result$download_time, 1), "seconds\n")
        }
      } else {
        result$message <- "Download failed (unknown error)"
        if (verbose) cat("\u2717 Download failed\n")
      }
    },
    error = function(e) {
      result$message <- paste("Download error:", e$message)
      if (verbose) cat("\u2717 Download error:", e$message, "\n")
    }
  )

  # ====================
  # 5. VALIDATE DOWNLOADED FILE
  # ====================

  if (result$success && file.exists(file_path)) {
    if (verbose) {
      cat("\n[5/5] Validating downloaded file...\n")
    }

    # Basic file validation
    if (result$file_size > 0) {
      # Check if it's a valid raster file (basic check)
      file_ext <- tools::file_ext(filename)
      if (tolower(file_ext) == "tif") {
        if (verbose) cat("\u2713 File appears to be a valid GeoTIFF\n")
      }

      if (verbose) {
        cat("\n=== Download Summary ===\n")
        cat("Status: SUCCESS\n")
        cat("File:", result$file_path, "\n")
        cat("Size:", round(result$file_size / 1024^2, 2), "MB\n")
        cat("URL:", result$url, "\n")
      }
    } else {
      result$success <- FALSE
      result$message <- "Downloaded file is empty"
      if (verbose) cat("\u2717 Downloaded file is empty\n")
    }
  }

  return(result)
}


#' Batch Download GAEZ Datasets
#'
#' Downloads multiple GAEZ datasets with the same base parameters but different
#' combinations of crops, variables, time periods, SSP scenarios, or water
#' management levels. Useful for comparative studies, time series analysis, or
#' bulk data acquisition.
#'
#' @param variables Character vector - Variable codes to download. Default is
#'   "RES05-YX" (attainable yield). Can be a vector like \code{c("RES05-YX", "RES05-SI")}
#'   or a list like \code{list("RES05-YX", "RES05-SI")}.
#' @param crops Character vector - Crop names/codes to download. Default is
#'   "WHEA" (wheat). Can be a vector like \code{c("MZE", "WHE")} or a list like
#'   \code{list("MZE", "WHE")}.
#' @param time_periods Character vector - Time period codes to download. Default
#'   is NULL (uses single time_period from ...). Can specify multiple periods like
#'   c("HP0120", "FP4160", "FP6180") for time series analysis. Accepts both vectors
#'   and lists.
#' @param ssps Character vector - SSP scenarios to download. Default is NULL,
#'   which auto-selects based on time_periods: "HIST" for historical periods
#'   (HP8100, HP0120) or "SSP370" for future periods (FP*). When time_periods
#'   contains both historical and future periods, the function automatically
#'   assigns appropriate SSPs.
#' @param climate_models Character vector - Climate models to download. Default
#'   is NULL, which auto-selects: "AGERA5" for historical periods or "ENSEMBLE"
#'   for future periods.
#' @param water_management_levels Character vector - Water management levels to
#'   download. Default is "HRLM". Options: "HILM", "HRLM", "LILM", "LRLM".
#' @param parallel Logical - Whether to use parallel downloads (default: TRUE).
#'   When TRUE, uses \code{curl::multi_download()} for efficient concurrent downloads
#'   with built-in progress tracking. Set to FALSE for sequential downloads
#'   (useful for debugging or when parallel downloads cause issues).
#' @param ... Other parameters passed to \code{download_gaez_dataset()}, such as
#'   download_dir, overwrite, verbose. Note: time_period, ssp, and climate_model
#'   should NOT be passed via ... when using the vectorized parameters above.
#'
#' @return A named list of download results, one for each combination. Each
#'   element is the result object from \code{download_gaez_dataset()}.
#'
#' @details
#' ## Parameter Combinations
#' The function creates all combinations of the specified parameters and downloads
#' each sequentially. Progress is reported for each download.
#'
#' ## Time Period and SSP/Climate Model Validation
#' GAEZ v5 has strict requirements for time periods:
#' \itemize{
#'   \item \strong{Historical periods} (HP8100, HP0120): Must use climate_model
#'     = "AGERA5" and ssp = "HIST"
#'   \item \strong{Future periods} (FP2140, FP4160, FP6180, FP8100): Must use
#'     GCM climate models (ENSEMBLE, GFDL-ESM4, etc.) and future SSPs
#'     (SSP126, SSP370, SSP585)
#' }
#'
#' When \code{time_periods} contains multiple periods spanning historical and
#' future, the function automatically pairs each time period with the appropriate
#' climate model and SSP scenario, ignoring incompatible combinations.
#'
#' ## Parameter Flexibility
#' All vector parameters (\code{variables}, \code{crops}, \code{time_periods}, etc.)
#' accept both atomic vectors and lists. The function automatically converts lists to
#' vectors for processing. This means these are equivalent:
#' \itemize{
#'   \item \code{crops = c("MZE", "WHE", "SRG")}
#'   \item \code{crops = list("MZE", "WHE", "SRG")}
#' }
#'
#' ## Parallel Downloads
#' By default, the function uses parallel downloads via \code{curl::multi_download()}
#' which provides:
#' \itemize{
#'   \item \strong{Speed}: 3-6x faster for multiple files (depending on network and
#'     HTTP version)
#'   \item \strong{Progress}: Built-in progress bar showing download status
#'   \item \strong{HTTP/2}: Automatic multiplexing when server supports it
#'   \item \strong{Resume}: Ability to resume interrupted downloads
#' }
#'
#' Set \code{parallel = FALSE} to use sequential downloads, which can be useful
#' for debugging or if parallel downloads encounter issues.
#'
#' ## Error Handling
#' The function continues downloading even if some files fail, allowing you to
#' get as much data as possible. Check the returned list for individual success
#' status.
#'
#' @examples
#' \dontrun{
#' # Download attainable yield for multiple crops in historical period
#' results <- batch_download_gaez_datasets(
#'   variables = "RES05-YX",  # Attainable yield
#'   crops = c("maize", "wheat", "sorghum"),
#'   time_periods = "HP0120"  # Auto-selects AGERA5 and HIST
#' )
#'
#' # Download same crop under different future scenarios
#' results <- batch_download_gaez_datasets(
#'   variables = "RES05-YX",  # Attainable yield
#'   crops = "maize",
#'   time_periods = "FP4160",
#'   ssps = c("SSP126", "SSP370", "SSP585")  # Auto-selects ENSEMBLE
#' )
#'
#' # Time series: Download maize yield across multiple time periods
#' # Function automatically uses HIST+AGERA5 for HP0120 and SSP370+ENSEMBLE for future
#' results <- batch_download_gaez_datasets(
#'   variables = "RES05-YX",  # Attainable yield
#'   crops = "maize",
#'   time_periods = c("HP0120", "FP4160", "FP6180")  # 2001-2020, 2041-2060, 2061-2080
#' )
#'
#' # Compare rain-fed vs irrigated for rice across time
#' results <- batch_download_gaez_datasets(
#'   variables = "RES05-YX",  # Attainable yield
#'   crops = "rice",
#'   time_periods = c("HP0120", "FP4160"),
#'   water_management_levels = c("HRLM", "HILM")  # Rain-fed vs Irrigated
#' )
#'
#' # Download multiple variables for same crop
#' results <- batch_download_gaez_datasets(
#'   variables = c("RES05-YX", "RES05-SI"),  # Yield and suitability
#'   crops = "wheat",
#'   time_periods = "FP4160",
#'   ssps = "SSP370",
#'   climate_models = "ENSEMBLE"
#' )
#'
#' # Use sequential downloads for debugging
#' results <- batch_download_gaez_datasets(
#'   variables = "RES05-YX",
#'   crops = c("maize", "wheat"),
#'   time_periods = "HP0120",
#'   parallel = FALSE  # Disable parallel downloads
#' )
#'
#' # Check success rate
#' success_count <- sum(sapply(results, function(x) x$success))
#' print(paste(success_count, "out of", length(results), "downloads succeeded"))
#' }
#'
#' @seealso \code{\link{download_gaez_dataset}}
#'
#' @export
#' @importFrom curl multi_download
batch_download_gaez_datasets <- function(variables = "RES05-YX",
                                          crops = "MZE",
                                          time_periods = "HP0120",
                                          ssps = NULL,
                                          climate_models = NULL,
                                          water_management_levels = "HRLM",
                                          parallel = TRUE,
                                          ...) {
  # Extract and clean up dots (parameters passed through)
  dots <- list(...)

  # Extract time_period from ... if time_periods not specified
  if (is.null(time_periods)) {
    if (!is.null(dots$time_period)) {
      time_periods <- dots$time_period
    } else {
      time_periods <- "FP4160"  # Default to future period
    }
  }

  # Remove vectorized parameters from dots to avoid conflicts
  # These should NOT be passed to download_gaez_dataset()
  # Note: Parameters in the function signature (time_periods, ssps, etc.) should
  # NOT appear in dots, but we clean them defensively
  dots$time_periods <- NULL
  dots$time_period <- NULL
  dots$ssps <- NULL
  dots$ssp <- NULL
  dots$climate_models <- NULL
  dots$climate_model <- NULL
  dots$parallel <- NULL
  dots$variables <- NULL
  dots$crops <- NULL
  dots$water_management_levels <- NULL

  # ====================
  # NORMALIZE PARAMETER TYPES
  # ====================
  # Convert lists to atomic vectors to ensure compatibility with expand.grid()
  # This allows the function to accept both c("A", "B") and list("A", "B")
  if (is.list(variables) && length(variables) > 0) {
    variables <- unlist(variables, use.names = FALSE)
  }
  if (is.list(crops) && length(crops) > 0) {
    crops <- unlist(crops, use.names = FALSE)
  }
  if (is.list(time_periods) && length(time_periods) > 0) {
    time_periods <- unlist(time_periods, use.names = FALSE)
  }
  if (is.list(ssps) && length(ssps) > 0) {
    ssps <- unlist(ssps, use.names = FALSE)
  }
  if (is.list(climate_models) && length(climate_models) > 0) {
    climate_models <- unlist(climate_models, use.names = FALSE)
  }
  if (is.list(water_management_levels) && length(water_management_levels) > 0) {
    water_management_levels <- unlist(water_management_levels, use.names = FALSE)
  }

  # ====================
  # INPUT VALIDATION
  # ====================
  # Validate parameters early to provide clear error messages

  # Check for empty parameters
  if (length(variables) == 0) {
    stop("'variables' cannot be empty. Provide at least one variable code.", call. = FALSE)
  }
  if (length(crops) == 0) {
    stop("'crops' cannot be empty. Provide at least one crop name or code.", call. = FALSE)
  }
  if (length(time_periods) == 0) {
    stop("'time_periods' cannot be empty.", call. = FALSE)
  }

  # Check parameter types
  if (!is.character(variables)) {
    stop("'variables' must be a character vector", call. = FALSE)
  }
  if (!is.character(crops)) {
    stop("'crops' must be a character vector", call. = FALSE)
  }
  if (!is.logical(parallel)) {
    stop("'parallel' must be TRUE or FALSE", call. = FALSE)
  }

  # Validate time periods against known values
  valid_periods <- c("HP8100", "HP0120", "FP2140", "FP4160", "FP6180", "FP8100")
  invalid_periods <- setdiff(time_periods, valid_periods)
  if (length(invalid_periods) > 0) {
    warning(
      "Unrecognized time period(s): ", paste(invalid_periods, collapse=", "), "\n",
      "Valid time periods: ", paste(valid_periods, collapse=", "),
      call. = FALSE
    )
  }

  # Validate SSP scenarios if provided
  if (!is.null(ssps)) {
    valid_ssps <- c("HIST", "SSP126", "SSP370", "SSP585")
    invalid_ssps <- setdiff(ssps, valid_ssps)
    if (length(invalid_ssps) > 0) {
      warning(
        "Unrecognized SSP scenario(s): ", paste(invalid_ssps, collapse=", "), "\n",
        "Valid SSPs: ", paste(valid_ssps, collapse=", "),
        call. = FALSE
      )
    }
  }

  # Validate climate models if provided
  if (!is.null(climate_models)) {
    valid_climate <- c("AGERA5", "ENSEMBLE", "GFDL-ESM4", "IPSL-CM6A-LR",
                       "MPI-ESM1-2-HR", "MRI-ESM2-0", "UKESM1-0-LL")
    invalid_climate <- setdiff(climate_models, valid_climate)
    if (length(invalid_climate) > 0) {
      warning(
        "Unrecognized climate model(s): ", paste(invalid_climate, collapse=", "), "\n",
        "Valid models: ", paste(valid_climate, collapse=", "),
        call. = FALSE
      )
    }
  }

  # Validate water management levels
  valid_wml <- c("HILM", "HRLM", "LILM", "LRLM")
  invalid_wml <- setdiff(water_management_levels, valid_wml)
  if (length(invalid_wml) > 0) {
    warning(
      "Unrecognized water management level(s): ", paste(invalid_wml, collapse=", "), "\n",
      "Valid levels: ", paste(valid_wml, collapse=", "),
      call. = FALSE
    )
  }

  # Determine if time periods are historical or future
  is_historical <- grepl("^HP", time_periods)
  is_future <- grepl("^FP", time_periods)

  # Auto-select SSPs based on time periods if not specified
  if (is.null(ssps)) {
    if (all(is_historical)) {
      ssps <- "HIST"
    } else if (all(is_future)) {
      ssps <- "SSP370"
    } else {
      # Mixed historical and future - will pair appropriately below
      ssps <- c("HIST", "SSP370")
    }
  }

  # Auto-select climate models based on time periods if not specified
  if (is.null(climate_models)) {
    if (all(is_historical)) {
      climate_models <- "AGERA5"
    } else if (all(is_future)) {
      climate_models <- "ENSEMBLE"
    } else {
      # Mixed historical and future
      climate_models <- c("AGERA5", "ENSEMBLE")
    }
  }

  # Create initial parameter combinations
  combinations <- expand.grid(
    variable = variables,
    crop = crops,
    time_period = time_periods,
    ssp = ssps,
    climate_model = climate_models,
    water_management_level = water_management_levels,
    stringsAsFactors = FALSE
  )

  # ====================
  # VALIDATE TIME PERIOD / SSP / CLIMATE MODEL COMBINATIONS
  # ====================
  # GAEZ v5 has strict requirements:
  # - Historical periods (HP8100, HP0120): Must use AGERA5 climate and HIST SSP
  # - Future periods (FP2140, FP4160, FP6180, FP8100): Must use GCM models
  #   (ENSEMBLE, GFDL-ESM4, IPSL-CM6A-LR, MPI-ESM1-2-HR, MRI-ESM2-0, UKESM1-0-LL)
  #   and future SSPs (SSP126, SSP370, SSP585)
  #
  # This validation filters out invalid combinations before attempting downloads
  valid_rows <- rep(TRUE, nrow(combinations))

  for (i in seq_len(nrow(combinations))) {
    tp <- combinations$time_period[i]
    ssp <- combinations$ssp[i]
    cm <- combinations$climate_model[i]

    # Historical period validation: Must be AGERA5 + HIST
    if (grepl("^HP", tp)) {
      if (ssp != "HIST" || cm != "AGERA5") {
        valid_rows[i] <- FALSE
      }
    }

    # Future period validation: Cannot be AGERA5 or HIST
    if (grepl("^FP", tp)) {
      if (ssp == "HIST" || cm == "AGERA5") {
        valid_rows[i] <- FALSE
      }
    }
  }

  combinations <- combinations[valid_rows, ]

  if (nrow(combinations) == 0) {
    # Build helpful error message explaining validation rules
    error_msg <- paste0(
      "No valid parameter combinations found.\n\n",
      "GAEZ v5 validation rules:\n",
      "  \u2022 Historical periods (HP*) require: climate_model='AGERA5' and ssp='HIST'\n",
      "  \u2022 Future periods (FP*) require: GCM models and SSP126/SSP370/SSP585\n\n",
      "Parameters provided:\n",
      "  \u2022 time_periods: ", paste(time_periods, collapse=", "), "\n",
      "  \u2022 ssps: ", paste(ssps, collapse=", "), "\n",
      "  \u2022 climate_models: ", paste(climate_models, collapse=", ")
    )

    cat("ERROR:", error_msg, "\n")

    # Return structured empty result with metadata
    # Calculate combinations attempted without expand.grid to avoid list type errors
    # Ensure all parameters are properly unlisted for safe multiplication
    safe_length <- function(x) {
      if (is.null(x)) return(1)
      if (is.list(x)) x <- unlist(x, use.names = FALSE)
      return(length(x))
    }

    combinations_attempted <- safe_length(variables) * safe_length(crops) * safe_length(time_periods) *
                               safe_length(ssps) * safe_length(climate_models) * safe_length(water_management_levels)

    return(structure(
      list(),
      class = c("gaez_batch_result", "list"),
      error_message = error_msg,
      combinations_attempted = combinations_attempted,
      valid_combinations = 0
    ))
  }

  cat("=== Batch Download ===\n")
  cat("Total valid combinations:", nrow(combinations), "\n")

  # Show breakdown by time period
  if (length(unique(combinations$time_period)) > 1) {
    cat("\nTime periods:\n")
    for (tp in unique(combinations$time_period)) {
      n <- sum(combinations$time_period == tp)
      cat("  ", tp, ":", n, "downloads\n")
    }
  }
  cat("\n")

  results <- list()

  # ====================
  # PARALLEL DOWNLOAD MODE
  # ====================
  if (parallel) {
    cat("Using parallel downloads (curl::multi_download)\n")

    # Extract download_dir and overwrite from dots
    download_dir <- if (!is.null(dots$download_dir)) dots$download_dir else NULL
    overwrite <- if (!is.null(dots$overwrite)) dots$overwrite else FALSE
    verbose_mode <- if (!is.null(dots$verbose)) dots$verbose else TRUE

    # Build all URLs and destination paths upfront
    urls <- vector("character", nrow(combinations))
    destfiles <- vector("character", nrow(combinations))
    to_download <- rep(TRUE, nrow(combinations))

    # Set testing mode to avoid interactive prompts
    old_option <- getOption("gaez_testing_mode")
    options(gaez_testing_mode = TRUE)

    for (i in seq_len(nrow(combinations))) {
      # Build URL with error capture
      url_result <- tryCatch({
        url <- build_gaez_url(
          variable = combinations$variable[i],
          time_period = combinations$time_period[i],
          climate_model = combinations$climate_model[i],
          ssp = combinations$ssp[i],
          crop = combinations$crop[i],
          water_management_level = combinations$water_management_level[i]
        )
        list(success = TRUE, url = url, error = NULL)
      }, error = function(e) {
        list(success = FALSE, url = NULL, error = conditionMessage(e))
      })

      if (url_result$success) {
        urls[i] <- url_result$url

        # Determine destination file path
        filename <- basename(url_result$url)

        if (is.null(download_dir)) {
          data_gaez_dir <- file.path("Data", "GAEZ")
          if (dir.exists(data_gaez_dir)) {
            download_dir <- data_gaez_dir
          } else if (dir.exists("GAEZ")) {
            download_dir <- "GAEZ"
          } else {
            download_dir <- data_gaez_dir
            dir.create(download_dir, recursive = TRUE, showWarnings = FALSE)
          }
        }

        if (!dir.exists(download_dir)) {
          dir.create(download_dir, recursive = TRUE, showWarnings = FALSE)
        }

        destfiles[i] <- file.path(download_dir, filename)

        # Check if file already exists
        if (!overwrite && file.exists(destfiles[i])) {
          to_download[i] <- FALSE
        }
      } else {
        # URL building failed - mark as failed
        urls[i] <- NA
        destfiles[i] <- NA
        to_download[i] <- FALSE
        if (verbose_mode) {
          cat("Warning: Failed to build URL for combination", i, ":", url_result$error, "\n")
        }
      }
    }

    options(gaez_testing_mode = old_option)

    # Separate files into those needing download vs already existing
    files_to_download <- which(to_download)
    already_exist <- which(!to_download)

    # Check for duplicate destination filenames
    # This can occur if URL construction is incomplete or parameters are identical
    if (length(files_to_download) > 0) {
      dup_mask <- duplicated(destfiles[files_to_download])
      if (any(dup_mask)) {
        # Identify unique duplicate files
        dup_files <- unique(destfiles[files_to_download][dup_mask])

        warning(
          length(dup_files), " duplicate filename(s) detected. ",
          "This may indicate identical parameter combinations.\n",
          "Example: ", dup_files[1], "\n",
          "Removing duplicates to proceed with download.",
          call. = FALSE
        )

        # Keep only first occurrence of each filename
        files_to_download <- files_to_download[!duplicated(destfiles[files_to_download])]
      }
    }

    if (length(already_exist) > 0) {
      cat(length(already_exist), "file(s) already exist (skipping)\n")
    }

    # Perform parallel downloads using curl::multi_download
    if (length(files_to_download) > 0) {
      cat("\nDownloading", length(files_to_download), "file(s) in parallel...\n")

      download_start <- Sys.time()
      download_results <- multi_download(
        urls = urls[files_to_download],
        destfiles = destfiles[files_to_download],
        resume = TRUE,
        progress = TRUE
      )
      download_end <- Sys.time()

      cat("Total download time:", round(as.numeric(download_end - download_start, units = "secs"), 1), "seconds\n\n")
    } else {
      download_results <- data.frame()
    }

    # Convert results to expected format
    for (i in seq_len(nrow(combinations))) {
      result_name <- paste(
        combinations$variable[i],
        combinations$crop[i],
        combinations$time_period[i],
        combinations$ssp[i],
        combinations$climate_model[i],
        combinations$water_management_level[i],
        sep = "_"
      )

      if (i %in% files_to_download) {
        # Find corresponding download result
        idx <- which(files_to_download == i)
        dl_result <- download_results[idx, ]

        results[[i]] <- list(
          success = dl_result$success,
          file_path = destfiles[i],
          url = urls[i],
          message = if (dl_result$success) "Download completed successfully" else paste("Download failed:", dl_result$error),
          validation_errors = character(0),
          file_size = if (file.exists(destfiles[i])) file.size(destfiles[i]) else NA,
          download_time = NA,
          already_exists = FALSE
        )
      } else if (i %in% already_exist) {
        # File already existed
        results[[i]] <- list(
          success = TRUE,
          file_path = destfiles[i],
          url = urls[i],
          message = "File already exists (skipped download)",
          validation_errors = character(0),
          file_size = if (file.exists(destfiles[i])) file.size(destfiles[i]) else NA,
          download_time = NA,
          already_exists = TRUE
        )
      } else {
        # URL building failed
        results[[i]] <- list(
          success = FALSE,
          file_path = NA,
          url = NA,
          message = "Failed to build URL",
          validation_errors = character(0),
          file_size = NA,
          download_time = NA,
          already_exists = FALSE
        )
      }

      names(results)[i] <- result_name
    }

  } else {
    # ====================
    # SEQUENTIAL DOWNLOAD MODE
    # ====================
    cat("Using sequential downloads\n\n")

    for (i in seq_len(nrow(combinations))) {
      cat("Download", i, "of", nrow(combinations), "\n")

      # Build argument list with cleaned dots
      args <- c(
        list(
          variable = combinations$variable[i],
          crop = combinations$crop[i],
          time_period = combinations$time_period[i],
          ssp = combinations$ssp[i],
          climate_model = combinations$climate_model[i],
          water_management_level = combinations$water_management_level[i]
        ),
        dots  # Pass cleaned dots
      )

      result <- do.call(download_gaez_dataset, args)

      results[[i]] <- result

      # Create descriptive name
      names(results)[i] <- paste(
        combinations$variable[i],
        combinations$crop[i],
        combinations$time_period[i],
        combinations$ssp[i],
        combinations$climate_model[i],
        combinations$water_management_level[i],
        sep = "_"
      )

      cat("\n")
    }
  }

  # Summary
  successes <- sum(sapply(results, function(x) x$success))
  cat("=== Batch Summary ===\n")
  cat("Successful downloads:", successes, "/", length(results), "\n")

  # Show failures if any
  failures <- which(!sapply(results, function(x) x$success))
  if (length(failures) > 0) {
    cat("\nFailed downloads:\n")
    for (idx in failures) {
      cat("  ", names(results)[idx], "\n")
    }
  }

  return(results)
}


#' Load GAEZ data into R workspace
#'
#' A convenient wrapper around \code{download_gaez_dataset()} that downloads
#' (if needed) and immediately loads GAEZ data into R as a terra SpatRaster
#' object. Automatically checks if the file is already cached locally to avoid
#' redundant downloads. Optionally crops data to country-level extents for
#' regional analysis. This provides a streamlined one-function workflow for
#' accessing GAEZ data.
#'
#' @inheritParams download_gaez_dataset
#' @param country Character or SpatVector - Country to crop data to. Can be:
#'   \itemize{
#'     \item NULL (default) - Returns global data
#'     \item Country name (e.g., "Niger")
#'     \item ISO3 code (e.g., "NER")
#'     \item terra SpatVector boundary object
#'   }
#' @param mask_to_boundary Logical - If TRUE (default) and country is specified,
#'   masks raster to country boundary (sets values outside boundary to NA).
#'   If FALSE, only crops to country extent (rectangular bounding box).
#' @param keep_global Logical - If TRUE (default) and country is specified,
#'   retains the global downloaded file. If FALSE, deletes the global file
#'   after successful cropping to save disk space.
#' @param return_metadata Logical - If TRUE, returns a list containing both the
#'   SpatRaster object and the download metadata. If FALSE (default), returns
#'   only the SpatRaster object.
#'
#' @return If \code{return_metadata = FALSE} (default), returns a terra SpatRaster
#'   object. If \code{return_metadata = TRUE}, returns a list with two elements:
#'   \itemize{
#'     \item \code{raster}: The terra SpatRaster object
#'     \item \code{metadata}: Download result metadata (file path, URL, size, etc.)
#'   }
#'
#' @details
#' The function performs the following workflow:
#' \enumerate{
#'   \item Calls \code{download_gaez_dataset()} with provided parameters
#'   \item If file exists locally, download is skipped (fast)
#'   \item If file doesn't exist, downloads from FAO Google Cloud Storage
#'   \item Loads the GeoTIFF file using \code{terra::rast()}
#'   \item Returns the raster ready for analysis
#' }
#'
#' ## Advantages over separate download + load
#' \itemize{
#'   \item Single function call simplifies workflow
#'   \item Automatic caching - no need to manually check for existing files
#'   \item Error handling combines download and load validation
#'   \item Optional metadata return provides full download information
#' }
#'
#' ## Country Cropping
#' When \code{country} is specified, the function:
#' \enumerate{
#'   \item Downloads global data (or uses cached version)
#'   \item Retrieves country boundary via \code{get_country_boundary()}
#'   \item Crops raster to country extent
#'   \item Optionally masks to exact country boundary
#'   \item Saves cropped data with "_[ISO3]" suffix
#'   \item Optionally deletes global file if \code{keep_global = FALSE}
#' }
#'
#' Cropped files are saved in the same directory as global files with the
#' country ISO3 code appended (e.g., "GAEZ-V5.RES05-YXX.FP4160.ENSEMBLE.SSP370.MZE.HRLM_NER.tif")
#'
#' ## Memory considerations
#' Large rasters may consume significant memory. For very large datasets or
#' limited RAM, consider using country cropping to reduce data size, or work
#' with file paths and load subsets as needed.
#'
#' @examples
#' \dontrun{
#' # Basic usage - load global maize yield data
#' maize <- load_gaez_data(
#'   crop = "maize",
#'   time_period = "HP0120",
#'   climate_model = "AGERA5"
#' )
#' terra::plot(maize)
#'
#' # Load country-level data
#' niger_maize <- load_gaez_data(
#'   crop = "maize",
#'   time_period = "HP0120",
#'   country = "Niger"
#' )
#' terra::plot(niger_maize, main = "Niger Maize Yield")
#'
#' # Load with ISO3 code and delete global file
#' niger_wheat <- load_gaez_data(
#'   crop = "wheat",
#'   country = "NER",
#'   keep_global = FALSE
#' )
#'
#' # Load with metadata
#' result <- load_gaez_data(
#'   crop = "wheat",
#'   time_period = "FP4160",
#'   ssp = "SSP370",
#'   country = "Niger",
#'   return_metadata = TRUE
#' )
#' terra::plot(result$raster)
#' print(result$metadata$file_path)
#' print(result$metadata$file_size)
#'
#' # Compare scenarios for specific country
#' ssp126 <- load_gaez_data(crop = "sorghum", time_period = "FP4160",
#'                          ssp = "SSP126", country = "Niger")
#' ssp370 <- load_gaez_data(crop = "sorghum", time_period = "FP4160",
#'                          ssp = "SSP370", country = "Niger")
#'
#' # Calculate differences
#' diff_370_126 <- ssp370 - ssp126
#' terra::plot(diff_370_126, main = "Niger: Yield change SSP370 vs SSP126")
#'
#' # Use custom boundary (e.g., administrative level 1)
#' library(geodata)
#' provinces <- gadm(country = "NER", level = 1)
#' provincial_data <- load_gaez_data(
#'   crop = "maize",
#'   country = provinces
#' )
#' }
#'
#' @seealso
#' \code{\link{download_gaez_dataset}} for download-only functionality,
#' \code{\link{batch_download_gaez_datasets}} for downloading multiple files,
#' \code{\link{combine_gaez_batch}} for combining multiple rasters,
#' \code{\link{get_country_boundary}} for country boundary retrieval
#'
#' @export
load_gaez_data <- function(variable = "RES05-YX",
                            time_period = "FP4160",
                            start_year = NULL,
                            end_year = NULL,
                            climate_model = NULL,
                            ssp = "SSP370",
                            crop = "WHEA",
                            water_management_level = "HRLM",
                            water_supply = "WSR",
                            resolution = NA,
                            download_dir = NULL,
                            overwrite = FALSE,
                            validate_inputs = TRUE,
                            verbose = TRUE,
                            country = NULL,
                            mask_to_boundary = TRUE,
                            keep_global = TRUE,
                            return_metadata = FALSE) {
  # Check if terra is available
  if (!requireNamespace("terra", quietly = TRUE)) {
    stop(
      "Package 'terra' is required for this function. ",
      "Please install it with: install.packages('terra')",
      call. = FALSE
    )
  }

  # Download the dataset (or find existing file)
  download_result <- download_gaez_dataset(
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
    download_dir = download_dir,
    overwrite = overwrite,
    validate_inputs = validate_inputs,
    verbose = verbose
  )

  # Check if download was successful
  if (!download_result$success) {
    stop(
      "Failed to download/locate GAEZ data: ",
      download_result$message,
      call. = FALSE
    )
  }

  # Load the raster
  global_file_path <- download_result$file_path

  if (verbose) {
    if (download_result$already_exists) {
      cat("Loading cached file...\n")
    } else {
      cat("Loading newly downloaded file...\n")
    }
  }

  tryCatch(
    {
      raster <- terra::rast(global_file_path)

      if (verbose && is.null(country)) {
        cat("Loaded SpatRaster:\n")
        cat("  Dimensions:", terra::nrow(raster), "rows x", terra::ncol(raster), "cols\n")
        cat("  Layers:", terra::nlyr(raster), "\n")
        cat("  CRS:", as.character(terra::crs(raster)), "\n")
        cat("  Extent:", paste(as.vector(terra::ext(raster)), collapse = ", "), "\n")
      }

      # ====================
      # COUNTRY CROPPING
      # ====================
      if (!is.null(country)) {
        if (verbose) {
          cat("\n=== Country Cropping ===\n")
        }

        # Get country boundary
        boundary <- get_country_boundary(country, verbose = verbose)

        # Extract ISO3 code for file naming
        if (inherits(country, "SpatVector")) {
          # Try to extract ISO3 from SpatVector attributes
          if ("ISO3" %in% names(boundary)) {
            country_iso3 <- boundary$ISO3[1]
          } else if ("ISO" %in% names(boundary)) {
            country_iso3 <- boundary$ISO[1]
          } else {
            country_iso3 <- "CUSTOM"
          }
        } else {
          # Get ISO3 from geodata lookup
          country_db <- geodata::country_codes()
          iso3_match <- country_db[toupper(country_db$ISO3) == toupper(country), ]
          if (nrow(iso3_match) == 1) {
            country_iso3 <- iso3_match$ISO3[1]
          } else {
            name_matches <- country_db[grepl(country, country_db$NAME, ignore.case = TRUE), ]
            if (nrow(name_matches) >= 1) {
              exact_match <- name_matches[tolower(name_matches$NAME) == tolower(country), ]
              if (nrow(exact_match) == 1) {
                country_iso3 <- exact_match$ISO3[1]
              } else {
                country_iso3 <- name_matches$ISO3[1]
              }
            } else {
              country_iso3 <- "UNKNOWN"
            }
          }
        }

        # Check if cropped file already exists
        base_name <- tools::file_path_sans_ext(basename(global_file_path))
        ext_name <- tools::file_ext(global_file_path)
        cropped_filename <- paste0(base_name, "_", country_iso3, ".", ext_name)
        cropped_file_path <- file.path(dirname(global_file_path), cropped_filename)

        # Check if cropped version exists and is newer than global
        if (file.exists(cropped_file_path) && !overwrite) {
          if (verbose) {
            cat("  Found existing cropped file, loading...\n")
          }
          raster <- terra::rast(cropped_file_path)
          download_result$file_path <- cropped_file_path
        } else {
          # Reproject boundary if CRS doesn't match
          if (!terra::same.crs(boundary, raster)) {
            if (verbose) {
              cat("  Reprojecting boundary to match raster CRS...\n")
            }
            boundary <- terra::project(boundary, terra::crs(raster))
          }

          # Crop to country extent
          if (verbose) {
            cat("  Cropping to country extent...\n")
          }
          raster_cropped <- terra::crop(raster, boundary)

          # Optionally mask to boundary
          if (mask_to_boundary) {
            if (verbose) {
              cat("  Masking to country boundary...\n")
            }
            raster <- terra::mask(raster_cropped, boundary)
          } else {
            raster <- raster_cropped
          }

          # Check if result has any data
          if (all(is.na(terra::values(raster, mat = FALSE)))) {
            warning(
              "All values are NA after cropping to country boundary. ",
              "The country may not overlap with the raster extent."
            )
          }

          # Save cropped raster
          if (verbose) {
            cat("  Saving cropped raster:", cropped_filename, "\n")
          }
          terra::writeRaster(raster, cropped_file_path, overwrite = TRUE)

          # Update download result metadata
          download_result$file_path <- cropped_file_path
          download_result$file_size <- file.size(cropped_file_path)

          # Delete global file if requested
          if (!keep_global && file.exists(global_file_path)) {
            if (verbose) {
              cat("  Deleting global file (keep_global = FALSE)...\n")
            }
            file.remove(global_file_path)
          }
        }

        if (verbose) {
          cat("\nLoaded Country-Cropped SpatRaster:\n")
          cat("  Country:", country_iso3, "\n")
          cat("  Dimensions:", terra::nrow(raster), "rows x", terra::ncol(raster), "cols\n")
          cat("  Layers:", terra::nlyr(raster), "\n")
          cat("  CRS:", as.character(terra::crs(raster)), "\n")
          cat("  Extent:", paste(as.vector(terra::ext(raster)), collapse = ", "), "\n")
          cat("  File:", cropped_file_path, "\n")
        }
      }

      # Return based on return_metadata flag
      if (return_metadata) {
        return(list(
          raster = raster,
          metadata = download_result
        ))
      } else {
        return(raster)
      }
    },
    error = function(e) {
      stop(
        "Failed to load raster from file: ",
        global_file_path,
        "\nError: ", e$message,
        call. = FALSE
      )
    }
  )
}
