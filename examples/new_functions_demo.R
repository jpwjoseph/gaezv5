# Demo: New Functions in gaezv5 0.1.2
# =====================================
# This script demonstrates the new load_gaez_data() and combine_gaez_batch()
# functions added to the gaezv5 package.

library(gaezv5)

# 1. LOAD_GAEZ_DATA() - Simplified workflow
# ==========================================

cat("===== Example 1: Using load_gaez_data() =====\n\n")

# Old workflow (2 steps):
# result <- download_gaez_dataset(crop = "maize", time_period = "HP0120")
# library(terra)
# maize <- terra::rast(result$file_path)

# New workflow (1 step):
maize <- load_gaez_data(
  crop = "maize",
  time_period = "HP0120",
  climate_model = "AGERA5"
)

# The raster is immediately ready for analysis
cat("\nMaize raster loaded:\n")
print(maize)

# If you need metadata too:
result <- load_gaez_data(
  crop = "wheat",
  time_period = "FP4160",
  ssp = "SSP370",
  return_metadata = TRUE
)

cat("\nWheat raster with metadata:\n")
print(result$raster)
cat("\nFile path:", result$metadata$file_path, "\n")
cat("File size:", round(result$metadata$file_size / 1024^2, 2), "MB\n")


# 2. COMBINE_GAEZ_BATCH() - Multi-layer datasets
# ===============================================

cat("\n\n===== Example 2: Using combine_gaez_batch() =====\n\n")

# Download multiple crops for comparison
results <- batch_download_gaez_datasets(
  crops = c("maize", "wheat", "sorghum"),
  time_period = "HP0120",
  climate_model = "AGERA5"
)

# Combine into single multi-layer raster
combined <- combine_gaez_batch(results)

cat("\nCombined raster:\n")
print(combined)

# Access individual layers
cat("\nLayer names:\n")
print(names(combined))

# Plot comparison (if interactive)
if (interactive()) {
  terra::plot(combined, main = names(combined))
}


# 3. TIME SERIES EXAMPLE
# ======================

cat("\n\n===== Example 3: Time series analysis =====\n\n")

# Download maize yield across multiple time periods
time_series_results <- batch_download_gaez_datasets(
  crops = "maize",
  time_periods = c("HP0120", "FP4160", "FP6180"),
  verbose = FALSE
)

# Combine and export to NetCDF
time_series <- combine_gaez_batch(
  time_series_results,
  output_file = "maize_timeseries.nc",
  format = "netcdf"
)

cat("\nTime series raster:\n")
print(time_series)
cat("\nSaved to NetCDF: maize_timeseries.nc\n")


# 4. SCENARIO COMPARISON
# ======================

cat("\n\n===== Example 4: Scenario comparison =====\n\n")

# Compare different SSP scenarios
scenario_results <- batch_download_gaez_datasets(
  crops = "sorghum",
  time_period = "FP4160",
  ssps = c("SSP126", "SSP370", "SSP585"),
  climate_models = "ENSEMBLE",
  verbose = FALSE
)

scenarios <- combine_gaez_batch(
  scenario_results,
  layer_names = c("SSP1-2.6", "SSP3-7.0", "SSP5-8.5")
)

cat("\nScenario comparison:\n")
print(scenarios)

# Calculate differences
if (terra::nlyr(scenarios) >= 2) {
  diff_370_126 <- scenarios[[2]] - scenarios[[1]]
  cat("\nYield change (SSP370 vs SSP126) calculated\n")

  if (interactive()) {
    terra::plot(diff_370_126, main = "Yield change: SSP3-7.0 vs SSP1-2.6")
  }
}

cat("\n\n===== Demo completed =====\n")
cat("New functions simplify GAEZ data workflows!\n")
