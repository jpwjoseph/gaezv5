# preview_demo.R
# Demonstration of preview_gaez_map() functionality
# Tests various parameter combinations and display options

library(gaezv5)

# ====================
# EXAMPLE 1: Basic Global Preview
# ====================
cat("\n=== Example 1: Global Maize Yield (Historical) ===\n")

preview_gaez_map(
  crop = "maize",
  time_period = "HP0120",
  climate_model = "AGERA5"
)


# ====================
# EXAMPLE 2: Country-Level Preview
# ====================
cat("\n\n=== Example 2: Austria Wheat Yield (Future SSP370) ===\n")

preview_gaez_map(
  crop = "wheat",
  time_period = "FP4160",
  ssp = "SSP370",
  country = "Austria"
)


# ====================
# EXAMPLE 3: Custom Bounding Box (West Africa)
# ====================
cat("\n\n=== Example 3: West Africa Sorghum (Custom Bbox) ===\n")

preview_gaez_map(
  crop = "sorghum",
  time_period = "HP0120",
  bbox = c(-20, 5, 20, 25),  # West Africa region
  width = 1000,
  height = 800
)


# ====================
# EXAMPLE 4: Different Water Supply Systems
# ====================
cat("\n\n=== Example 4: Rice with Irrigation (Brazil) ===\n")

preview_gaez_map(
  crop = "rice",
  country = "Brazil",
  time_period = "HP0120",
  water_supply = "WSS",  # Sprinkler irrigation
  water_management_level = "HRLM",
  climate_model = "AGERA5"
)


# ====================
# EXAMPLE 5: Using Year Range Instead of Period Code
# ====================
cat("\n\n=== Example 5: Sorghum Using Year Range (Niger) ===\n")

preview_gaez_map(
  crop = "sorghum",
  start_year = 2041,
  end_year = 2060,
  ssp = "SSP370",
  country = "Niger",
  verbose = FALSE
)


# ====================
# EXAMPLE 6: Different Climate Models
# ====================
cat("\n\n=== Example 6: Comparing Climate Models ===\n")

# Using specific climate model (not ensemble)
preview_gaez_map(
  crop = "maize",
  country = "Kenya",
  time_period = "FP4160",
  ssp = "SSP370",
  climate_model = "GFDL-ESM4",  # Specific climate model
  verbose = FALSE
)


# ====================
# EXAMPLE 7: Custom Color Palettes
# ====================
cat("\n\n=== Example 7: Custom Color Palettes ===\n")

preview_gaez_map(
  crop = "pearl millet",
  country = "Niger",
  time_period = "HP0120",
  color_palette = "viridis",
  opacity = 0.8,
  verbose = FALSE
)


# ====================
# EXAMPLE 8: Static Plot for Quick Inspection
# ====================
cat("\n\n=== Example 8: Static Plot (Quick Inspection) ===\n")

preview_gaez_map(
  crop = "cassava",
  country = "Nigeria",
  time_period = "HP0120",
  viewer = "static",
  color_palette = "RdYlGn"
)


# ====================
# EXAMPLE 9: High Resolution (1km) Suitability Map
# ====================
cat("\n\n=== Example 9: High Resolution 1km Suitability (Rwanda Wheat) ===\n")

# Use 1km resolution crop suitability data (30 arc-second resolution)
# Specify the data_code with "30AS" suffix to get 1km instead of 10km
# Most crop yield variables are at 10km, but suitability has 1km option
preview_gaez_map(
  variable = "RES05-SIX30AS",  # Suitability index at 1km resolution
  crop = "wheat",
  country = "France",
  time_period = "HP0120",
  verbose = FALSE,
  color_palette = "RdYlGn",
  width = 1200,
  height = 1000
)


# ====================
# EXAMPLE 10: Save Preview to File
# ====================
cat("\n\n=== Example 10: Save Preview to HTML ===\n")

preview_gaez_map(
  crop = "maize",
  country = "Zambia",
  time_period = "FP4160",
  ssp = "SSP126",
  save_path = "zambia_maize_ssp126.html",
  display = FALSE,
  verbose = FALSE
)

cat("Preview saved to: zambia_maize_ssp126.html\n")


# ====================
# EXAMPLE 11: Return Data for Analysis
# ====================
cat("\n\n=== Example 11: Return Data with Map ===\n")

result <- preview_gaez_map(
  crop = "wheat",
  country = "India",
  time_period = "HP0120",
  return_data = TRUE,
  verbose = FALSE
)

cat("\nReturned data structure:\n")
cat("  - map: leaflet map object\n")
cat("  - data: SpatRaster with", prod(dim(result$data)[1:2]), "cells\n")
cat("  - extent:", result$extent, "\n")
cat("  - variable:", result$variable, "\n")
cat("  - crop:", result$crop, "\n")
cat("  - time_period:", result$time_period, "\n")

# Calculate summary statistics
raster_values <- terra::values(result$data, mat = FALSE)
valid_values <- raster_values[!is.na(raster_values) & raster_values > 0]

if (length(valid_values) > 0) {
  cat("\nData summary:\n")
  cat("  Mean yield:", round(mean(valid_values), 2), "\n")
  cat("  Median yield:", round(median(valid_values), 2), "\n")
  cat("  Min yield:", round(min(valid_values), 2), "\n")
  cat("  Max yield:", round(max(valid_values), 2), "\n")
}


# ====================
# EXAMPLE 12: Scenario Comparison
# ====================
cat("\n\n=== Example 12: Scenario Comparison (SSP126 vs SSP370) ===\n")

# Load both scenarios
result_126 <- preview_gaez_map(
  crop = "maize",
  country = "Tanzania",
  time_period = "FP4160",
  ssp = "SSP126",
  return_data = TRUE,
  display = FALSE,
  verbose = FALSE
)

result_370 <- preview_gaez_map(
  crop = "maize",
  country = "Tanzania",
  time_period = "FP4160",
  ssp = "SSP370",
  return_data = TRUE,
  display = FALSE,
  verbose = FALSE
)

# Calculate difference
diff_raster <- result_370$data - result_126$data

# Plot difference
terra::plot(
  diff_raster,
  main = "Tanzania Maize Yield Change (SSP370 - SSP126, 2041-2060)",
  col = hcl.colors(100, "RdYlGn", rev = FALSE),
  axes = TRUE
)

cat("\nYield difference (SSP370 - SSP126):\n")
diff_values <- terra::values(diff_raster, mat = FALSE)
valid_diff <- diff_values[!is.na(diff_values)]
if (length(valid_diff) > 0) {
  cat("  Mean change:", round(mean(valid_diff), 2), "\n")
  cat("  Median change:", round(median(valid_diff), 2), "\n")
  cat("  Max decrease:", round(min(valid_diff), 2), "\n")
  cat("  Max increase:", round(max(valid_diff), 2), "\n")
}


# ====================
# EXAMPLE 13: Interactive Mode
# ====================
cat("\n\n=== Example 13: Interactive Crop Selection ===\n")
cat("(Skipped in batch mode - requires user input)\n")
cat("To test interactive mode, run:\n")
cat('  preview_gaez_map(crop = "whe", country = "France", interactive = TRUE)\n')


# ====================
# EXAMPLE 14: Different Input Levels
# ====================
cat("\n\n=== Example 14: Low Input Agriculture ===\n")

preview_gaez_map(
  crop = "sorghum",
  country = "Ethiopia",
  time_period = "HP0120",
  water_management_level = "LR",  # Low input level
  water_supply = "WSR",  # Rainfed
  verbose = FALSE
)


# ====================
# EXAMPLE 15: Multiple Countries (Custom Boundary)
# ====================
cat("\n\n=== Example 15: Regional Preview (East Africa bbox) ===\n")

preview_gaez_map(
  crop = "coffee",
  time_period = "FP4160",
  ssp = "SSP370",
  bbox = c(28, -12, 42, 5),  # East Africa region
  color_palette = "YlGnBu",
  verbose = FALSE
)


# === All Examples Complete! ===
# Resolution Information:
#   - Most crop variables (yield): 10km resolution (5 arc-minutes)
#   - Suitability variables: Available at both 10km and 1km (30 arc-seconds)
#   - Use data codes with '30AS' suffix for 1km: RES05-SIX30AS, RES05-YCX30AS, etc.
#   - Land resources (AEZ): ~1km resolution
# Key parameters demonstrated:
#   - variable: crop-specific and theme-specific variables
#   - crop: various crops (maize, wheat, rice, sorghum, etc.)
#   - time_period: historical (HP) and future periods (FP)
#   - start_year/end_year: year-based period selection
#   - climate_model: AGERA5, ENSEMBLE, specific models
#   - ssp: SSP126, SSP245, SSP370, SSP585
#   - water_supply: WSR (rainfed), WSS (sprinkler irrigation)
#   - water_management_level: HRLM (high), LR (low), IR (intermediate)
#   - country: country names or ISO codes
#   - bbox: custom bounding boxes
#   - width/height: custom dimensions
#   - viewer: leaflet (interactive) or static
#   - color_palette: various palettes (YlOrRd, viridis, RdYlGn, etc.)
#   - opacity: layer transparency
#   - save_path: export to HTML
#   - return_data: get raster data for analysis
#   - interactive: enable/disable user prompts
#   - verbose: control output verbosity