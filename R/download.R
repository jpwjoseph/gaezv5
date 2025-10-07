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

  tryCatch(
    {
      # Set non-interactive mode for automated processing
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

      # Restore original option
      options(gaez_testing_mode = old_option)

      result$url <- url
      if (verbose) {
        cat("\u2713 URL built successfully\n")
        cat("   URL:", url, "\n")
      }
    },
    error = function(e) {
      options(gaez_testing_mode = old_option)
      result$message <- paste("Failed to build URL:", e$message)
      if (verbose) {
        cat("\u2717 URL building failed:", e$message, "\n")
      }
      return(result)
    }
  )

  if (is.null(result$url)) {
    return(result)
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
      file_ext <- file_ext(filename)
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
#'   "RES05-YX" (attainable yield).
#' @param crops Character vector - Crop names/codes to download. Default is
#'   "WHEA" (wheat).
#' @param time_periods Character vector - Time period codes to download. Default
#'   is NULL (uses single time_period from ...). Can specify multiple periods like
#'   c("HP0120", "FP4160", "FP6180") for time series analysis.
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
#' # Check success rate
#' success_count <- sum(sapply(results, function(x) x$success))
#' print(paste(success_count, "out of", length(results), "downloads succeeded"))
#' }
#'
#' @seealso \code{\link{download_gaez_dataset}}
#'
#' @export
batch_download_gaez_datasets <- function(variables = "RES05-YX",
                                          crops = "WHEA",
                                          time_periods = NULL,
                                          ssps = NULL,
                                          climate_models = NULL,
                                          water_management_levels = "HRLM",
                                          ...) {
  # Extract time_period from ... if time_periods not specified
  dots <- list(...)
  if (is.null(time_periods)) {
    if (!is.null(dots$time_period)) {
      time_periods <- dots$time_period
      dots$time_period <- NULL  # Remove from dots to avoid duplication
    } else {
      time_periods <- "FP4160"  # Default to future period
    }
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
    cat("ERROR: No valid parameter combinations found.\n")
    cat("Historical periods (HP*) require climate_model='AGERA5' and ssp='HIST'\n")
    cat("Future periods (FP*) require GCM models and future SSPs (SSP126/SSP370/SSP585)\n")
    return(list())
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

  for (i in seq_len(nrow(combinations))) {
    cat("Download", i, "of", nrow(combinations), "\n")

    result <- download_gaez_dataset(
      variable = combinations$variable[i],
      crop = combinations$crop[i],
      time_period = combinations$time_period[i],
      ssp = combinations$ssp[i],
      climate_model = combinations$climate_model[i],
      water_management_level = combinations$water_management_level[i],
      ...
    )

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
