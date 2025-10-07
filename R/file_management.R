#' Check if a GAEZ URL exists
#'
#' Performs an HTTP HEAD request to verify that a GAEZ dataset URL is accessible
#' before attempting a full download. Useful for validating parameter combinations.
#'
#' @param url Character - The full URL to check
#' @param timeout Numeric - Timeout in seconds (default: 10)
#'
#' @return Logical - TRUE if URL exists (HTTP 200), FALSE otherwise
#'
#' @examples
#' \dontrun{
#' # Build a URL and check if it exists
#' url <- build_gaez_url(crop = "maize", time_period = "HP0120")
#' if (check_url_exists(url)) {
#'   download_gaez_dataset(crop = "maize", time_period = "HP0120")
#' }
#' }
#'
#' @export
#' @importFrom httr HEAD status_code timeout
check_url_exists <- function(url, timeout = 10) {
  if (missing(url) || is.null(url) || url == "") {
    stop("URL is required", call. = FALSE)
  }

  tryCatch(
    {
      response <- HEAD(url, timeout(timeout))
      return(status_code(response) == 200)
    },
    error = function(e) {
      message("Error checking URL: ", e$message)
      return(FALSE)
    }
  )
}


#' List downloaded GAEZ files
#'
#' Scans common GAEZ data directories and returns a table of all downloaded
#' GAEZ v5 files with metadata (size, modification time).
#'
#' @param directories Character vector - Directories to search. If NULL, searches
#'   default locations: "Data/GAEZ", "GAEZ", "Data/GAEZ_Niger_Analysis"
#' @param pattern Character - File pattern to match (default: "GAEZ-V5.*\\.tif$")
#' @param full_info Logical - If TRUE, returns detailed file information
#'   (default: TRUE)
#'
#' @return A tibble with columns:
#'   \itemize{
#'     \item \code{filename}: File name
#'     \item \code{path}: Full file path
#'     \item \code{size_mb}: File size in megabytes
#'     \item \code{modified}: Last modification time
#'     \item \code{variable} (if full_info=TRUE): Extracted variable code
#'     \item \code{crop} (if full_info=TRUE): Extracted crop code
#'   }
#'
#' @examples
#' \dontrun{
#' # List all downloaded files
#' files <- list_downloaded_files()
#' print(files)
#'
#' # Get total size
#' total_size_gb <- sum(files$size_mb) / 1024
#' print(paste("Total:", round(total_size_gb, 2), "GB"))
#' }
#'
#' @export
#' @importFrom dplyr tibble mutate arrange
#' @importFrom stringr str_extract
list_downloaded_files <- function(directories = NULL,
                                   pattern = "GAEZ-V5.*\\.tif$",
                                   full_info = TRUE) {
  # Default directories
  if (is.null(directories)) {
    directories <- c(
      "Data/GAEZ",
      "GAEZ",
      "Data/GAEZ_Niger_Analysis"
    )
  }

  # Filter to existing directories
  directories <- directories[dir.exists(directories)]

  if (length(directories) == 0) {
    message("No GAEZ data directories found")
    return(tibble(
      filename = character(0),
      path = character(0),
      size_mb = numeric(0),
      modified = as.POSIXct(character(0))
    ))
  }

  # Scan directories
  all_files <- c()
  for (dir in directories) {
    files <- list.files(dir, pattern = pattern, full.names = TRUE, recursive = FALSE)
    all_files <- c(all_files, files)
  }

  if (length(all_files) == 0) {
    message("No GAEZ files found in specified directories")
    return(tibble(
      filename = character(0),
      path = character(0),
      size_mb = numeric(0),
      modified = as.POSIXct(character(0))
    ))
  }

  # Get file info
  file_info <- tibble(
    filename = basename(all_files),
    path = all_files,
    size_mb = file.size(all_files) / 1024^2,
    modified = file.mtime(all_files)
  )

  # Extract variable and crop codes if requested
  if (full_info) {
    file_info <- file_info |>
      mutate(
        variable = str_extract(filename, "(?<=GAEZ-V5\\.)[A-Z0-9-]+"),
        crop = str_extract(filename, "[A-Z]{3,4}(?=\\.[A-Z]{4}\\.tif$)")
      )
  }

  file_info <- file_info |>
    arrange(modified)

  return(file_info)
}


#' Verify GAEZ file integrity
#'
#' Checks if a downloaded GAEZ file is valid by verifying it can be opened
#' as a raster and checking for reasonable data values.
#'
#' @param file_path Character - Path to the GAEZ file
#' @param check_values Logical - Whether to check if raster contains reasonable
#'   values (default: FALSE, as this requires loading the entire raster)
#'
#' @return Logical - TRUE if file is valid, FALSE otherwise
#'
#' @examples
#' \dontrun{
#' # Check a downloaded file
#' verify_file_integrity("Data/GAEZ/GAEZ-V5.RES05-YXX.FP4160.ENSEMBLE.SSP370.MZE.HRLM.tif")
#' }
#'
#' @export
verify_file_integrity <- function(file_path, check_values = FALSE) {
  if (!file.exists(file_path)) {
    message("File does not exist: ", file_path)
    return(FALSE)
  }

  # Check file size
  file_size <- file.size(file_path)
  if (file_size == 0) {
    message("File is empty")
    return(FALSE)
  }

  # Check file extension
  if (!grepl("\\.tif$", file_path, ignore.case = TRUE)) {
    message("File is not a TIFF")
    return(FALSE)
  }

  # Try to open with terra if available
  if (requireNamespace("terra", quietly = TRUE)) {
    tryCatch(
      {
        r <- terra::rast(file_path)

        # Basic checks
        if (terra::ncell(r) == 0) {
          message("Raster has no cells")
          return(FALSE)
        }

        # Optional value check
        if (check_values) {
          vals <- terra::values(r, mat = FALSE)
          if (all(is.na(vals))) {
            message("Raster contains only NA values")
            return(FALSE)
          }
        }

        return(TRUE)
      },
      error = function(e) {
        message("Error opening raster: ", e$message)
        return(FALSE)
      }
    )
  } else {
    # If terra not available, just check file basics
    return(file_size > 1000) # At least 1KB
  }
}


#' Get download cache directory
#'
#' Returns the path to the default GAEZ download directory, creating it if
#' necessary.
#'
#' @param create Logical - Whether to create the directory if it doesn't exist
#'   (default: TRUE)
#'
#' @return Character - Path to the cache directory
#'
#' @examples
#' \dontrun{
#' cache_dir <- get_download_cache()
#' print(paste("GAEZ files stored in:", cache_dir))
#' }
#'
#' @export
get_download_cache <- function(create = TRUE) {
  cache_dir <- file.path("Data", "GAEZ")

  if (create && !dir.exists(cache_dir)) {
    dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
  }

  return(cache_dir)
}


#' Clear download cache
#'
#' Removes downloaded GAEZ files from the cache directory. Use with caution!
#'
#' @param confirm Logical - Must be TRUE to actually delete files (safety feature)
#' @param pattern Character - File pattern to remove (default: all GAEZ-V5 files)
#' @param cache_dir Character - Directory to clear (default: uses get_download_cache())
#'
#' @return Invisible numeric - Number of files deleted
#'
#' @examples
#' \dontrun{
#' # List files first
#' files <- list_downloaded_files()
#' print(files)
#'
#' # Clear cache (must set confirm=TRUE)
#' clear_download_cache(confirm = TRUE)
#' }
#'
#' @export
clear_download_cache <- function(confirm = FALSE,
                                  pattern = "GAEZ-V5.*\\.tif$",
                                  cache_dir = NULL) {
  if (!confirm) {
    message("Set confirm=TRUE to actually delete files")
    return(invisible(0))
  }

  if (is.null(cache_dir)) {
    cache_dir <- get_download_cache(create = FALSE)
  }

  if (!dir.exists(cache_dir)) {
    message("Cache directory does not exist: ", cache_dir)
    return(invisible(0))
  }

  files <- list.files(cache_dir, pattern = pattern, full.names = TRUE)

  if (length(files) == 0) {
    message("No files to delete")
    return(invisible(0))
  }

  message("Deleting ", length(files), " files from ", cache_dir)

  success <- file.remove(files)
  deleted_count <- sum(success)

  message("Deleted ", deleted_count, " files")

  return(invisible(deleted_count))
}


#' Combine batch download results into multi-layer dataset
#'
#' Takes the output from \code{batch_download_gaez_datasets()} and combines all
#' successfully downloaded rasters into a single multi-layer SpatRaster object.
#' Optionally exports to NetCDF format for efficient multi-dimensional storage.
#' This is useful for comparative analysis, time series, or scenario comparisons.
#'
#' @param batch_results List - Output from \code{batch_download_gaez_datasets()},
#'   or a named list where each element contains a \code{file_path} component.
#' @param output_file Character - Optional path for NetCDF export. If NULL,
#'   only returns the SpatRaster object without saving. File extension should
#'   be ".nc" for NetCDF format.
#' @param format Character - Output format: "spatraster" (default) returns a
#'   terra SpatRaster object, "netcdf" saves to NetCDF and returns the SpatRaster.
#' @param layer_names Character vector - Custom layer names. If NULL, names are
#'   automatically generated from the batch result names. Must match the number
#'   of successful downloads.
#' @param overwrite Logical - Whether to overwrite existing output file
#'   (default: FALSE)
#' @param verbose Logical - Whether to print progress messages (default: TRUE)
#'
#' @return A terra SpatRaster object with multiple layers. Each layer corresponds
#'   to one successfully downloaded file from the batch.
#'
#' @details
#' The function performs the following steps:
#' \enumerate{
#'   \item Filters batch results to only successful downloads
#'   \item Loads each GeoTIFF file as a raster layer using terra
#'   \item Combines all layers into a single SpatRaster object
#'   \item Assigns meaningful layer names (e.g., "MZE_FP4160_SSP370_ENSEMBLE")
#'   \item Optionally exports to NetCDF format with compression
#' }
#'
#' ## NetCDF Export
#' When \code{output_file} is specified or \code{format = "netcdf"}, the function
#' exports the multi-layer raster to NetCDF format, which provides:
#' \itemize{
#'   \item Efficient compression for large datasets
#'   \item Self-describing metadata
#'   \item Wide compatibility with GIS and climate tools
#'   \item Support for multi-dimensional arrays
#' }
#'
#' ## Layer Naming
#' If \code{layer_names = NULL}, layer names are automatically generated from the
#' batch result names (typically format: "VARIABLE_CROP_TIMEPERIOD_SSP_CLIMATE_WATER").
#' These names are cleaned to be valid R variable names and NetCDF dimension names.
#'
#' @examples
#' \dontrun{
#' # Download multiple crops for comparison
#' results <- batch_download_gaez_datasets(
#'   crops = c("maize", "wheat", "sorghum"),
#'   time_period = "HP0120",
#'   climate_model = "AGERA5"
#' )
#'
#' # Combine into single multi-layer raster
#' combined <- combine_gaez_batch(results)
#' print(combined)  # Shows 3 layers
#'
#' # Access individual layers
#' terra::plot(combined[[1]])  # Maize
#' terra::plot(combined[[2]])  # Wheat
#'
#' # Export to NetCDF
#' combined <- combine_gaez_batch(
#'   results,
#'   output_file = "crop_comparison.nc",
#'   format = "netcdf"
#' )
#'
#' # Time series example
#' results <- batch_download_gaez_datasets(
#'   crops = "maize",
#'   time_periods = c("HP0120", "FP4160", "FP6180")
#' )
#' time_series <- combine_gaez_batch(results, output_file = "maize_timeseries.nc")
#' }
#'
#' @seealso \code{\link{batch_download_gaez_datasets}}, \code{\link{load_gaez_data}}
#'
#' @export
combine_gaez_batch <- function(batch_results,
                                output_file = NULL,
                                format = c("spatraster", "netcdf"),
                                layer_names = NULL,
                                overwrite = FALSE,
                                verbose = TRUE) {
  # Check if terra is available
  if (!requireNamespace("terra", quietly = TRUE)) {
    stop("Package 'terra' is required for this function. Please install it with: install.packages('terra')",
      call. = FALSE
    )
  }

  format <- match.arg(format)

  # Validate input
  if (!is.list(batch_results) || length(batch_results) == 0) {
    stop("batch_results must be a non-empty list from batch_download_gaez_datasets()",
      call. = FALSE
    )
  }

  # Filter to successful downloads
  successful <- sapply(batch_results, function(x) {
    !is.null(x$success) && x$success && !is.null(x$file_path) && file.exists(x$file_path)
  })

  if (sum(successful) == 0) {
    stop("No successful downloads found in batch_results", call. = FALSE)
  }

  success_results <- batch_results[successful]
  n_layers <- length(success_results)

  if (verbose) {
    cat("=== Combining GAEZ Batch Results ===\n")
    cat("Successful downloads:", n_layers, "/", length(batch_results), "\n")
  }

  # Extract file paths
  file_paths <- sapply(success_results, function(x) x$file_path)

  # Load all rasters
  if (verbose) {
    cat("Loading", n_layers, "raster layers...\n")
  }

  raster_list <- list()
  for (i in seq_along(file_paths)) {
    tryCatch(
      {
        raster_list[[i]] <- terra::rast(file_paths[i])
        if (verbose) {
          cat("  [", i, "/", n_layers, "] Loaded:", basename(file_paths[i]), "\n")
        }
      },
      error = function(e) {
        warning("Failed to load raster: ", file_paths[i], " - ", e$message)
        raster_list[[i]] <- NULL
      }
    )
  }

  # Remove any failed loads
  raster_list <- raster_list[!sapply(raster_list, is.null)]

  if (length(raster_list) == 0) {
    stop("Failed to load any raster files", call. = FALSE)
  }

  # Combine into single SpatRaster
  if (verbose) {
    cat("Combining layers into single SpatRaster...\n")
  }

  combined <- do.call(c, raster_list)

  # Set layer names
  if (!is.null(layer_names)) {
    if (length(layer_names) != terra::nlyr(combined)) {
      warning(
        "layer_names length (", length(layer_names), ") does not match number of layers (",
        terra::nlyr(combined), "). Using auto-generated names."
      )
      layer_names <- NULL
    }
  }

  if (is.null(layer_names)) {
    # Generate names from batch result names
    layer_names <- names(success_results)
    if (is.null(layer_names)) {
      layer_names <- paste0("layer_", seq_len(terra::nlyr(combined)))
    } else {
      # Clean names to be valid R/NetCDF names
      layer_names <- make.names(layer_names)
    }
  }

  names(combined) <- layer_names

  if (verbose) {
    cat("Created SpatRaster with", terra::nlyr(combined), "layers\n")
    cat("Dimensions:", terra::nrow(combined), "rows x", terra::ncol(combined), "cols\n")
    cat("CRS:", as.character(terra::crs(combined)), "\n")
  }

  # Export to NetCDF if requested
  if (!is.null(output_file) || format == "netcdf") {
    if (is.null(output_file)) {
      output_file <- "gaez_combined.nc"
    }

    # Ensure .nc extension
    if (!grepl("\\.nc$", output_file, ignore.case = TRUE)) {
      output_file <- paste0(tools::file_path_sans_ext(output_file), ".nc")
    }

    # Check if file exists
    if (file.exists(output_file) && !overwrite) {
      stop(
        "Output file already exists: ", output_file,
        "\nSet overwrite=TRUE to replace it.",
        call. = FALSE
      )
    }

    if (verbose) {
      cat("Exporting to NetCDF:", output_file, "\n")
    }

    tryCatch(
      {
        terra::writeCDF(combined, output_file, overwrite = overwrite, compression = 4)
        if (verbose) {
          cat("NetCDF export successful\n")
          cat("File size:", round(file.size(output_file) / 1024^2, 2), "MB\n")
        }
      },
      error = function(e) {
        stop("Failed to export NetCDF: ", e$message, call. = FALSE)
      }
    )
  }

  if (verbose) {
    cat("\n=== Summary ===\n")
    cat("Layers:", paste(layer_names, collapse = ", "), "\n")
    if (!is.null(output_file) && file.exists(output_file)) {
      cat("NetCDF file:", output_file, "\n")
    }
  }

  return(combined)
}
