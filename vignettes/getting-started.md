# Getting Started with gaezv5

Julian Joseph 2025-10-08

-   [Introduction](#introduction)
-   [What is GAEZ v5?](#what-is-gaez-v5)
-   [Installation](#installation)
-   [Quick Start](#quick-start)
    -   [Simplest Workflow: Load Data Directly](#simplest-workflow-load-data-directly)
    -   [Load Country-Level Data](#load-country-level-data)
-   [Core Workflow](#core-workflow)
    -   [1. Discover Available Data](#1-discover-available-data)
    -   [2. Look Up Specific Codes](#2-look-up-specific-codes)
    -   [3. Download and Load Data](#3-download-and-load-data)
    -   [4. Work with Loaded Data](#4-work-with-loaded-data)
-   [Country-Level Analysis](#country-level-analysis)
    -   [Load Data for Specific Countries](#load-data-for-specific-countries)
    -   [Country Cropping Options](#country-cropping-options)
    -   [Get Country Boundaries](#get-country-boundaries)
-   [Batch Operations](#batch-operations)
    -   [Batch Download Multiple Files](#batch-download-multiple-files)
    -   [Combine Batch Results](#combine-batch-results)
    -   [Export to NetCDF](#export-to-netcdf)
    -   [Country-Level Batch Operations](#country-level-batch-operations)
-   [Common Use Cases](#common-use-cases)
    -   [1. Historical vs Future Comparison](#1-historical-vs-future-comparison)
    -   [2. Scenario Comparison](#2-scenario-comparison)
    -   [3. Irrigated vs Rainfed](#3-irrigated-vs-rainfed)
    -   [4. Country Time Series Analysis](#4-country-time-series-analysis)
    -   [5. Multi-Country Comparison](#5-multi-country-comparison)
    -   [6. Provincial Analysis](#6-provincial-analysis)
-   [File Management](#file-management)
    -   [List Downloaded Files](#list-downloaded-files)
    -   [Verify File Integrity](#verify-file-integrity)
    -   [Clear Cache](#clear-cache)
-   [Advanced Features](#advanced-features)
    -   [Load with Metadata](#load-with-metadata)
    -   [Build Custom URLs](#build-custom-urls)
    -   [Using Year Ranges](#using-year-ranges)
    -   [Validate Climate/SSP Combinations](#validate-climatessp-combinations)
-   [Understanding GAEZ Codes](#understanding-gaez-codes)
    -   [Time Periods](#time-periods)
    -   [Climate Models](#climate-models)
    -   [SSP Scenarios](#ssp-scenarios)
    -   [Water Management](#water-management)
    -   [Common Crop Codes (Theme 4)](#common-crop-codes-theme-4)
-   [Best Practices](#best-practices)
    -   [1. Use Country Cropping for Regional Studies](#1-use-country-cropping-for-regional-studies)
    -   [2. Use ISO3 Codes to Avoid Ambiguity](#2-use-iso3-codes-to-avoid-ambiguity)
    -   [3. Batch Operations with Parallel Downloads](#3-batch-operations-with-parallel-downloads)
    -   [4. Clean Up Global Files When Using Country Data](#4-clean-up-global-files-when-using-country-data)
    -   [5. Export Large Multi-Layer Datasets to NetCDF](#5-export-large-multi-layer-datasets-to-netcdf)
-   [Getting Help](#getting-help)
-   [Troubleshooting](#troubleshooting)
    -   [Country Not Found](#country-not-found)
    -   [Large Files / Out of Memory](#large-files--out-of-memory)
    -   [Download Failures](#download-failures)
-   [Further Reading](#further-reading)
-   [Citation](#citation)

## Introduction {#introduction}

The `gaezv5` package provides comprehensive tools for downloading, processing, and analyzing Global Agro-Ecological Zones (GAEZ) version 5 data from the International Institute for Applied Systems Analysis (IIASA) and the Food and Agriculture Organization (FAO). This vignette covers all major features including data discovery, downloading, country-level cropping, and batch processing.

## What is GAEZ v5? {#what-is-gaez-v5}

GAEZ v5 provides comprehensive global data on:

-   **Agricultural potential** and crop suitability
-   **Attainable and actual** crop yields
-   **Agro-climatic resources** (temperature, precipitation, growing periods)
-   **Land and water resources**
-   **Yield and production gaps**

The data is available at high spatial resolution (\~9km) and includes:

-   Historical periods (1981-2000, 2001-2020)
-   Future projections (2021-2100) under different climate scenarios
-   Multiple input levels (high/low) and water management systems (irrigated/rainfed)
-   Over 60 crop types

## Installation {#installation}

``` r
# Install from GitHub
# devtools::install_github("jpwjoseph/gaezv5")

library(gaezv5)
library(terra)  # For raster operations
```

## Quick Start {#quick-start}

### Simplest Workflow: Load Data Directly {#simplest-workflow-load-data-directly}

The fastest way to get GAEZ data into R:

``` r
# Load maize yield data (automatically downloads if needed)
maize <- load_gaez_data(
  crop = "MZE",           # Maize crop code
  time_period = "HP0120"  # 2001-2020
)

# Plot immediately
plot(maize, main = "Global Maize Attainable Yield (2001-2020)")

# Get statistics
mean_yield <- global(maize, "mean", na.rm = TRUE)
print(paste("Mean global yield:", round(mean_yield, 0), "kg/ha"))
```

### Load Country-Level Data {#load-country-level-data}

Get data for specific countries to reduce file sizes:

``` r
# Load Niger maize data (much smaller than global)
niger_maize <- load_gaez_data(
  crop = "MZE",
  time_period = "HP0120",
  country = "Niger"  # Automatically crops to country boundary
)

plot(niger_maize, main = "Niger Maize Yield")

# File size comparison
# Global: ~800 MB, Niger: ~8 MB (100x reduction!)
```

## Core Workflow {#core-workflow}

### 1. Discover Available Data

Explore available crops and variables:

``` r
# List all available crops (theme 4 = attainable yield)
crops <- list_gaez_crops(theme = 4)
head(crops)

# Filter to cereals only
cereals <- list_gaez_crops(crop_group = "cereal", theme = 4)
print(cereals[, c("gaez_crop_code", "name")])

# Browse all variables
head(gaez_variables[, c("variable_code", "variable_name", "theme_name")])
```

### 2. Look Up Specific Codes

Find exact codes using lookup functions:

``` r
# Look up crop by name (returns code)
lookup_gaez_crop("pearl millet", theme = 4)  # Returns "PMI"

# Look up variable
yield_var <- lookup_gaez_variable("attainable yield")
print(yield_var$variable_code)  # Returns "RES05-YX"
```

### 3. Download and Load Data

**Method 1: Load directly (recommended)**

``` r
# One function does it all: download + load
wheat <- load_gaez_data(
  crop = "WHE",
  time_period = "HP0120",
  climate_model = "AGERA5"
)
```

**Method 2: Download then load separately**

``` r
# Download only
result <- download_gaez_dataset(
  crop = "WHE",
  time_period = "HP0120",
  climate_model = "AGERA5"
)

# Load separately
if (result$success) {
  wheat <- rast(result$file_path)
}
```

### 4. Work with Loaded Data

``` r
# Basic operations
print(wheat)                              # Raster info
plot(wheat, main = "Wheat Yield")         # Visualize
global(wheat, "mean", na.rm = TRUE)       # Statistics
hist(wheat, main = "Yield Distribution")  # Distribution

# Crop to extent
region <- ext(0, 20, 10, 30)  # Lon/lat bounds
wheat_region <- crop(wheat, region)

# Extract values at points
points <- data.frame(lon = c(2, 5, 8), lat = c(13, 15, 12))
points_vect <- vect(points, geom = c("lon", "lat"), crs = "EPSG:4326")
values <- extract(wheat, points_vect)
```

## Country-Level Analysis {#country-level-analysis}

### Load Data for Specific Countries {#load-data-for-specific-countries}

``` r
# By country name
niger_data <- load_gaez_data(
  crop = "SOR",      # Sorghum
  time_period = "HP0120",
  country = "Niger"  # Automatically crops and masks
)

# By ISO3 code (faster, no ambiguity)
burkina_data <- load_gaez_data(
  crop = "SOR",
  time_period = "HP0120",
  country = "BFA"  # Burkina Faso ISO3 code
)

# Case-insensitive
nigeria_data <- load_gaez_data(
  crop = "SOR",
  time_period = "HP0120",
  country = "nigeria"  # Works!
)
```

### Country Cropping Options {#country-cropping-options}

``` r
# Option 1: Mask to exact boundary (default)
data1 <- load_gaez_data(
  crop = "MZE",
  country = "Niger",
  mask_to_boundary = TRUE  # Values outside boundary = NA
)

# Option 2: Crop to extent only (faster)
data2 <- load_gaez_data(
  crop = "MZE",
  country = "Niger",
  mask_to_boundary = FALSE  # Rectangular crop only
)

# Option 3: Delete global file to save space
data3 <- load_gaez_data(
  crop = "MZE",
  country = "Niger",
  keep_global = FALSE  # Removes global file after cropping
)
```

### Get Country Boundaries {#get-country-boundaries}

``` r
# Retrieve boundary for plotting or custom analysis
niger_boundary <- get_country_boundary("Niger")
plot(niger_boundary, main = "Niger Boundary")

# Get provincial boundaries (admin level 1)
provinces <- get_country_boundary("Niger", level = 1)
plot(provinces, main = "Niger Provinces")

# Use in analysis
data <- load_gaez_data(crop = "MZE", country = provinces)
```

## Batch Operations {#batch-operations}

### Batch Download Multiple Files {#batch-download-multiple-files}

``` r
# Download multiple crops
results <- batch_download_gaez_datasets(
  crops = c("MZE", "WHE", "SOR", "PMI"),  # Maize, wheat, sorghum, millet
  time_period = "HP0120",
  climate_model = "AGERA5",
  verbose = FALSE
)

# Check success
successes <- sum(sapply(results, function(x) x$success))
print(paste(successes, "/", length(results), "succeeded"))
```

### Combine Batch Results {#combine-batch-results}

``` r
# Download time series
time_results <- batch_download_gaez_datasets(
  crops = "MZE",
  time_periods = c("HP0120", "FP4160", "FP6180"),  # 3 time periods
  verbose = FALSE
)

# Combine into single multi-layer raster
timeseries <- combine_gaez_batch(time_results)
print(timeseries)  # Shows 3 layers

# Access individual layers
plot(timeseries[[1]], main = "2001-2020")
plot(timeseries[[2]], main = "2041-2060")
plot(timeseries[[3]], main = "2061-2080")

# Calculate changes
change1 <- timeseries[[2]] - timeseries[[1]]
change2 <- timeseries[[3]] - timeseries[[2]]
plot(c(change1, change2), main = c("Change 1", "Change 2"))
```

### Export to NetCDF {#export-to-netcdf}

``` r
# Combine and export to NetCDF
combined <- combine_gaez_batch(
  time_results,
  output_file = "maize_timeseries.nc",
  format = "netcdf"
)

# Load NetCDF later
timeseries <- rast("maize_timeseries.nc")
```

### Country-Level Batch Operations {#country-level-batch-operations}

``` r
# Download global, combine and crop to country
results <- batch_download_gaez_datasets(
  crops = c("MZE", "WHE", "RIC", "SOR"),
  time_period = "HP0120"
)

# Combine with country cropping
niger_crops <- combine_gaez_batch(
  results,
  country = "Niger",
  output_file = "niger_crops.nc",
  keep_global = FALSE  # Save disk space
)

# Result is smaller multi-crop raster for Niger only
plot(niger_crops)
```

## Common Use Cases {#common-use-cases}

### 1. Historical vs Future Comparison

``` r
# Load historical
hist <- load_gaez_data(
  crop = "WHE",
  time_period = "HP0120",
  climate_model = "AGERA5"
)

# Load future
future <- load_gaez_data(
  crop = "WHE",
  time_period = "FP4160",
  climate_model = "ENSEMBLE",
  ssp = "SSP370"
)

# Calculate change
change <- future - hist
plot(change, main = "Wheat Yield Change: 2041-2060 vs 2001-2020")

# Statistics
mean_change <- global(change, "mean", na.rm = TRUE)
print(paste("Mean change:", round(mean_change, 1), "kg/ha"))
```

### 2. Scenario Comparison

``` r
# Download multiple scenarios
scenario_results <- batch_download_gaez_datasets(
  crops = "MZE",
  time_period = "FP4160",
  ssps = c("SSP126", "SSP370", "SSP585")  # Low, medium, high emissions
)

# Combine with custom names
scenarios <- combine_gaez_batch(
  scenario_results,
  layer_names = c("Low_emissions", "Medium_emissions", "High_emissions")
)

# Compare scenarios
plot(scenarios)

# Difference between scenarios
diff_high_low <- scenarios[[3]] - scenarios[[1]]
plot(diff_high_low, main = "Impact of Emissions (SSP585 vs SSP126)")
```

### 3. Irrigated vs Rainfed

``` r
# Compare water management
water_results <- batch_download_gaez_datasets(
  crops = "RIC",  # Rice
  time_period = "HP0120",
  water_management_levels = c("HRLM", "HILM")  # Rainfed vs Irrigated
)

water_comparison <- combine_gaez_batch(
  water_results,
  layer_names = c("Rainfed", "Irrigated")
)

# Calculate irrigation benefit
irrigation_gain <- water_comparison[[2]] - water_comparison[[1]]
plot(irrigation_gain, main = "Rice Yield Gain from Irrigation")
```

### 4. Country Time Series Analysis

``` r
# Download time series for country
time_results <- batch_download_gaez_datasets(
  crops = "MZE",
  time_periods = c("HP0120", "FP4160", "FP6180")
)

# Combine with country cropping
niger_timeseries <- combine_gaez_batch(
  time_results,
  country = "Niger",
  output_file = "niger_maize_timeseries.nc"
)

# Analyze temporal trends
layer1 <- niger_timeseries[[1]]  # 2001-2020
layer2 <- niger_timeseries[[2]]  # 2041-2060
layer3 <- niger_timeseries[[3]]  # 2061-2080

# Mean yields by period
yields <- c(
  global(layer1, "mean", na.rm = TRUE)[[1]],
  global(layer2, "mean", na.rm = TRUE)[[1]],
  global(layer3, "mean", na.rm = TRUE)[[1]]
)

# Plot trend
periods <- c("2001-2020", "2041-2060", "2061-2080")
barplot(yields, names.arg = periods,
        main = "Niger Maize Yield Projections",
        ylab = "Yield (kg/ha)")
```

### 5. Multi-Country Comparison

``` r
# Compare multiple countries
countries <- c("Niger", "Nigeria", "Burkina Faso", "Mali")
country_data <- list()

for (country in countries) {
  country_data[[country]] <- load_gaez_data(
    crop = "SOR",
    time_period = "FP4160",
    country = country,
    verbose = FALSE
  )
}

# Calculate mean yields
mean_yields <- sapply(country_data, function(r) {
  global(r, "mean", na.rm = TRUE)[[1]]
})

# Plot comparison
barplot(mean_yields, main = "Sorghum Yield by Country (2041-2060)",
        ylab = "Yield (kg/ha)", las = 2, col = "lightblue")
```

### 6. Provincial Analysis

``` r
# Get provincial boundaries
provinces <- get_country_boundary("Niger", level = 1)

# Load data
data <- load_gaez_data(
  crop = "PMI",  # Pearl millet
  time_period = "HP0120",
  country = provinces
)

# Extract mean yield by province
province_yields <- extract(data, provinces, fun = mean, na.rm = TRUE)
provinces$yield <- province_yields[, 2]

# Map yields by province
plot(provinces, "yield", main = "Pearl Millet Yield by Province")
```

## File Management {#file-management}

### List Downloaded Files {#list-downloaded-files}

``` r
# View all downloaded files
files <- list_downloaded_files()
print(files[, c("filename", "size_mb", "modified")])

# Calculate total storage
total_gb <- sum(files$size_mb, na.rm = TRUE) / 1024
print(paste("Total:", round(total_gb, 2), "GB"))

# Filter to specific crop
maize_files <- files[grepl("MZE", files$filename), ]
print(maize_files)
```

### Verify File Integrity {#verify-file-integrity}

``` r
# Check if a file is valid
is_valid <- verify_file_integrity(files$path[1])
print(paste("File valid:", is_valid))

# Verify all files
all_valid <- sapply(files$path, verify_file_integrity)
print(paste(sum(all_valid), "out of", length(all_valid), "files are valid"))
```

### Clear Cache {#clear-cache}

``` r
# View cache directory
cache_dir <- get_download_cache()
print(paste("Cache location:", cache_dir))

# Clear all cached files (use with caution!)
# clear_download_cache(confirm = TRUE)

# Or clear specific pattern
# clear_download_cache(pattern = ".*MZE.*", confirm = TRUE)
```

## Advanced Features {#advanced-features}

### Load with Metadata {#load-with-metadata}

``` r
# Get both raster and download information
result <- load_gaez_data(
  crop = "WHE",
  time_period = "HP0120",
  return_metadata = TRUE
)

# Access raster
wheat <- result$raster
plot(wheat)

# Access metadata
print(result$metadata$file_path)
print(result$metadata$file_size)
print(result$metadata$url)
```

### Build Custom URLs {#build-custom-urls}

``` r
# Construct URL manually
url <- build_gaez_url(
  variable = "RES05-YX",
  crop = "SOR",
  time_period = "FP4160",
  climate_model = "MRI-ESM2-0",  # Specific GCM
  ssp = "SSP370",
  water_management_level = "HILM"
)

print(url)

# Check if URL exists
if (check_url_exists(url)) {
  print("URL is valid")
}
```

### Using Year Ranges {#using-year-ranges}

``` r
# Specify years instead of time period codes
data <- load_gaez_data(
  crop = "MZE",
  start_year = 2041,
  end_year = 2060,
  ssp = "SSP370"
)
# Automatically converts to time_period = "FP4160"
```

### Validate Climate/SSP Combinations {#validate-climatessp-combinations}

``` r
# Check if combination is valid
validation <- validate_climate_ssp(
  time_period = "FP4160",
  climate_model = "ENSEMBLE",
  ssp = "SSP370"
)

if (validation$valid) {
  print("Valid combination")
} else {
  print(validation$message)
}
```

## Understanding GAEZ Codes {#understanding-gaez-codes}

### Time Periods {#time-periods}

| Code   | Period    | Description         |
|--------|-----------|---------------------|
| HP8100 | 1981-2000 | Historical baseline |
| HP0120 | 2001-2020 | Recent historical   |
| FP2140 | 2021-2040 | Near future         |
| FP4160 | 2041-2060 | Mid-century         |
| FP6180 | 2061-2080 | Late century        |
| FP8100 | 2081-2100 | End of century      |

### Climate Models {#climate-models}

-   **AGERA5**: Historical periods only (reanalysis data)
-   **ENSEMBLE**: Multi-model mean (recommended for future)
-   Individual GCMs: GFDL-ESM4, IPSL-CM6A-LR, MPI-ESM1-2-HR, MRI-ESM2-0, UKESM1-0-LL

### SSP Scenarios {#ssp-scenarios}

-   **HIST**: Historical scenario
-   **SSP126**: Low emissions (Paris Agreement, \~1.5-2°C)
-   **SSP370**: Medium emissions (current policies, \~3-4°C)
-   **SSP585**: High emissions (worst case, \~4-5°C)

### Water Management {#water-management}

| Code | Description                           |
|------|---------------------------------------|
| HRLM | High input, Rainfed, Low management   |
| HILM | High input, Irrigated, Low management |
| LRLM | Low input, Rainfed, Low management    |
| LILM | Low input, Irrigated, Low management  |

### Common Crop Codes (Theme 4) {#common-crop-codes-theme-4}

| Code | Crop         | Code | Crop    |
|------|--------------|------|---------|
| MZE  | Maize        | WHE  | Wheat   |
| RIC  | Rice         | SOR  | Sorghum |
| PMI  | Pearl millet | COW  | Cowpea  |
| SUN  | Sunflower    | SOY  | Soybean |

## Best Practices {#best-practices}

### 1. Use Country Cropping for Regional Studies

``` r
# Don't download global if you only need one country
# BAD: Large file, slow
# global <- load_gaez_data(crop = "MZE")
# niger <- crop(global, niger_boundary)

# GOOD: Direct country download
niger <- load_gaez_data(crop = "MZE", country = "Niger")
```

### 2. Use ISO3 Codes to Avoid Ambiguity

``` r
# Use ISO3 codes for clarity
data <- load_gaez_data(crop = "MZE", country = "NER")  # Niger

# Instead of potentially ambiguous names
# data <- load_gaez_data(crop = "MZE", country = "niger")  # Could match Niger/Nigeria
```

### 3. Batch Operations with Parallel Downloads

``` r
# Download multiple files efficiently
results <- batch_download_gaez_datasets(
  crops = c("MZE", "WHE", "RIC", "SOR"),
  time_periods = c("HP0120", "FP4160"),
  parallel = TRUE  # Uses parallel downloads (default)
)
```

### 4. Clean Up Global Files When Using Country Data

``` r
# Save storage by deleting global files
data <- load_gaez_data(
  crop = "MZE",
  country = "Niger",
  keep_global = FALSE  # Deletes large global file, keeps small Niger file
)
```

### 5. Export Large Multi-Layer Datasets to NetCDF

``` r
# NetCDF is more efficient for multi-layer data
combined <- combine_gaez_batch(
  results,
  output_file = "timeseries.nc",
  format = "netcdf"  # Compressed storage
)
```

## Getting Help {#getting-help}

``` r
# View examples
show_gaez_examples()

# Function help
?load_gaez_data
?combine_gaez_batch
?get_country_boundary
?batch_download_gaez_datasets

# Browse all documentation
help(package = "gaezv5")
```

## Troubleshooting {#troubleshooting}

### Country Not Found {#country-not-found}

If you get “Country not found” error:

``` r
# Check available countries
library(geodata)
codes <- country_codes()
head(codes[, c("NAME", "ISO3")], 20)

# Use exact ISO3 code
data <- load_gaez_data(crop = "MZE", country = "NER")
```

### Large Files / Out of Memory

For large datasets:

``` r
# 1. Use country cropping
data <- load_gaez_data(crop = "MZE", country = "Niger")  # Much smaller

# 2. Process in chunks
r <- rast("large_file.tif")
chunk1 <- crop(r, ext(0, 10, 10, 20))  # Process region by region
```

### Download Failures {#download-failures}

If downloads fail:

``` r
# 1. Check internet connection
check_url_exists(build_gaez_url(crop = "MZE", time_period = "HP0120"))

# 2. Try again (sometimes server is busy)
result <- download_gaez_dataset(crop = "MZE", time_period = "HP0120")

# 3. Check file integrity
verify_file_integrity(result$file_path)
```

If errors persist, please submit an issue on the gaezv5 R package Github repository.

## Further Reading {#further-reading}

-   [GAEZ v5 Portal](https://gaez.fao.org/)
-   [GAEZ v5 Model Documentation](https://github.com/un-fao/gaezv5/wiki)
-   [Package GitHub](https://github.com/jpwjoseph/gaezv5)
-   [Report Issues](https://github.com/jpwjoseph/gaezv5/issues)

## Citation {#citation}

When using GAEZ v5 data, please cite:

> FAO & IIASA. 2025. Global Agro-ecological Zoning version 5 (GAEZ v5) Model Documentation. <https://github.com/un-fao/gaezv5/wiki>

For the gaezv5 R package:

``` r
citation("gaezv5")
```

------------------------------------------------------------------------

**Package Version**: 0.1.2 **Last Updated**: 2025-10-08