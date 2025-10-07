# Demo: Country-Level Cropping in gaezv5
# ========================================
# This script demonstrates the new country cropping features added in v0.1.2

library(gaezv5)
library(terra)

cat("===== Country Cropping Demo =====\n\n")

# ============================================================================
# Part 1: Using get_country_boundary()
# ============================================================================

cat("Part 1: Retrieving Country Boundaries\n")
cat("======================================\n\n")

# Method 1: By country name
cat("1. Get boundary by country name:\n")
niger_boundary <- get_country_boundary("Niger")
cat("   Result: Got", nrow(niger_boundary), "polygon\n")
cat("   Extent:", paste(as.vector(ext(niger_boundary)), collapse = ", "), "\n\n")

# Method 2: By ISO3 code (faster, no ambiguity)
cat("2. Get boundary by ISO3 code:\n")
usa_boundary <- get_country_boundary("USA", verbose = FALSE)
cat("   Result: Got", nrow(usa_boundary), "polygon\n\n")

# Method 3: Get administrative subdivisions (provinces)
cat("3. Get provincial boundaries (level 1):\n")
niger_provinces <- get_country_boundary("Niger", level = 1, verbose = FALSE)
cat("   Result: Got", nrow(niger_provinces), "provinces\n\n")

# Method 4: Use custom SpatVector
cat("4. Use existing SpatVector:\n")
custom_boundary <- get_country_boundary(niger_boundary, verbose = FALSE)
cat("   Result: Validated SpatVector with", nrow(custom_boundary), "polygon\n\n")


# ============================================================================
# Part 2: load_gaez_data() with Country Cropping
# ============================================================================

cat("\nPart 2: Loading Country-Level Data\n")
cat("===================================\n\n")

cat("Example: Load maize yield data for Niger (2001-2020)\n\n")

# This example requires actual data download or cached files
# Uncomment the following to run with real data:

# # Load global data
# cat("A. Loading global data...\n")
# global_maize <- load_gaez_data(
#   crop = "MZE",  # Use crop code to avoid lookup issues
#   time_period = "HP0120",
#   climate_model = "AGERA5",
#   verbose = FALSE
# )
# cat("   Global dimensions:", dim(global_maize), "\n")
# cat("   Global file size:", round(object.size(global_maize) / 1024^2, 2), "MB\n\n")
#
# # Load country-level data
# cat("B. Loading Niger data (with country cropping)...\n")
# niger_maize <- load_gaez_data(
#   crop = "MZE",
#   time_period = "HP0120",
#   climate_model = "AGERA5",
#   country = "Niger",
#   mask_to_boundary = TRUE,
#   keep_global = TRUE  # Keep global file for comparison
# )
# cat("   Niger dimensions:", dim(niger_maize), "\n")
# cat("   Niger file size:", round(object.size(niger_maize) / 1024^2, 2), "MB\n")
# cat("   Size reduction:",
#     round((1 - object.size(niger_maize) / object.size(global_maize)) * 100, 1),
#     "%\n\n")
#
# # Plot comparison
# if (interactive()) {
#   par(mfrow = c(1, 2))
#   plot(global_maize, main = "Global Maize Yield")
#   plot(niger_maize, main = "Niger Maize Yield")
# }

cat("(Commented out - uncomment to run with actual data)\n\n")


# ============================================================================
# Part 3: combine_gaez_batch() with Country Cropping
# ============================================================================

cat("\nPart 3: Combining Batch Results with Country Cropping\n")
cat("======================================================\n\n")

cat("Example: Time series analysis for Niger\n\n")

# This example requires actual data download
# Uncomment to run with real data:

# # Download multiple time periods
# cat("A. Downloading rice yield for 3 time periods...\n")
# results <- batch_download_gaez_datasets(
#   crops = "rice",
#   time_periods = c("HP0120", "FP4160", "FP6180"),
#   verbose = FALSE
# )
# cat("   Downloaded", length(results), "datasets\n\n")
#
# # Combine as global data
# cat("B. Combining into global multi-layer raster...\n")
# global_timeseries <- combine_gaez_batch(
#   results,
#   verbose = FALSE
# )
# cat("   Global time series dimensions:", dim(global_timeseries), "\n")
# cat("   Layers:", names(global_timeseries), "\n\n")
#
# # Combine with country cropping
# cat("C. Combining with Niger cropping...\n")
# niger_timeseries <- combine_gaez_batch(
#   results,
#   country = "Niger",
#   output_file = "niger_rice_timeseries.nc",
#   keep_global = FALSE  # Delete global files to save space
# )
# cat("   Niger time series dimensions:", dim(niger_timeseries), "\n")
# cat("   Saved to: niger_rice_timeseries.nc\n\n")
#
# # Analyze temporal changes
# if (terra::nlyr(niger_timeseries) >= 3) {
#   change_21_41 <- niger_timeseries[[2]] - niger_timeseries[[1]]
#   change_41_61 <- niger_timeseries[[3]] - niger_timeseries[[2]]
#
#   cat("D. Temporal changes calculated:\n")
#   cat("   2001-2020 to 2041-2060: ",
#       round(mean(values(change_21_41), na.rm = TRUE), 2), "kg/ha\n")
#   cat("   2041-2060 to 2061-2080: ",
#       round(mean(values(change_41_61), na.rm = TRUE), 2), "kg/ha\n\n")
#
#   if (interactive()) {
#     plot(c(change_21_41, change_41_61),
#          main = c("Change: HP0120 to FP4160", "Change: FP4160 to FP6180"))
#   }
# }

cat("(Commented out - uncomment to run with actual data)\n\n")


# ============================================================================
# Part 4: Advanced Use Cases
# ============================================================================

cat("\nPart 4: Advanced Use Cases\n")
cat("==========================\n\n")

cat("Example use cases:\n\n")

cat("1. Multi-country comparison:\n")
cat("   countries <- c('Niger', 'Nigeria', 'Burkina Faso')\n")
cat("   for (country in countries) {\n")
cat("     data <- load_gaez_data(crop='maize', country=country)\n")
cat("     # Analyze...\n")
cat("   }\n\n")

cat("2. Provincial-level analysis:\n")
cat("   provinces <- get_country_boundary('Niger', level=1)\n")
cat("   data <- load_gaez_data(crop='rice', country=provinces)\n")
cat("   # Extract stats by province...\n\n")

cat("3. Custom region analysis:\n")
cat("   library(terra)\n")
cat("   # Create custom polygon for Sahel region\n")
cat("   sahel <- vect('sahel_boundary.shp')\n")
cat("   data <- load_gaez_data(crop='sorghum', country=sahel)\n\n")

cat("4. Storage-efficient workflow:\n")
cat("   # Download, crop, and delete global files\n")
cat("   data <- load_gaez_data(\n")
cat("     crop='wheat',\n")
cat("     country='Niger',\n")
cat("     keep_global=FALSE  # Saves disk space\n")
cat("   )\n\n")


# ============================================================================
# Summary
# ============================================================================

cat("\n===== Summary =====\n\n")

cat("Key features of country cropping:\n")
cat("  ✓ Automatic boundary download and caching\n")
cat("  ✓ Support for country names, ISO3 codes, and custom boundaries\n")
cat("  ✓ Significant file size reduction (10-100x smaller)\n")
cat("  ✓ Cropped files are cached for fast reuse\n")
cat("  ✓ Optional global file deletion to save space\n")
cat("  ✓ Works with both single files and batch operations\n")
cat("  ✓ Perfect for country-specific climate impact studies\n\n")

cat("For more information, see:\n")
cat("  ?get_country_boundary\n")
cat("  ?load_gaez_data\n")
cat("  ?combine_gaez_batch\n\n")

cat("Demo complete!\n")
