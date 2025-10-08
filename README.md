# gaezv5: Download and Process GAEZ v5 Data

An R package for downloading, processing, and analyzing Global Agro-Ecological Zones (GAEZ) version 5 data from the Food and Agriculture Organization (FAO) and the International Institute for Applied Systems Analysis (IIASA). GAEZ v5 provides comprehensive global data on agricultural potential, crop suitability, attainable yields, and agro-climatic resources.

## Features

### Core Functionality

-   🚀 **Simple Data Loading**: One-function workflow with `load_gaez_data()`
-   🌍 **Country-Level Cropping**: Automatic spatial subsetting to country boundaries
-   📦 **Batch Processing**: Download and combine multiple datasets efficiently
-   💾 **NetCDF Export**: Efficient multi-layer data storage
-   ⚡ **Parallel Downloads**: 3-6x faster batch downloads
-   📊 **Comprehensive Coverage**: Access to all 6 GAEZ themes and 100+ variables
-   🔍 **Smart Caching**: Automatically detects and reuses existing downloads
-   ✅ **Validation**: Built-in checks for parameter compatibility and data integrity
-   📚 **Well Documented**: Extensive help files, vignettes, and examples

## Installation

Install the development version from GitHub:

``` r
# install.packages("devtools")
devtools::install_github("jpwjoseph/gaezv5")
```

## Quick Start

### Simplest Workflow: Load Data Directly

``` r
library(gaezv5)
library(terra)

# Load global maize yield data (automatically downloads if needed)
maize <- load_gaez_data(
  crop = "MZE",           # Maize
  time_period = "HP0120"  # 2001-2020
)

plot(maize, main = "Global Maize Yield (2001-2020)")
```

### Load Country-Level Data

Reduce file sizes by 90-99% with automatic country cropping:

``` r
# Load Niger maize data (much smaller than global)
niger_maize <- load_gaez_data(
  crop = "MZE",
  time_period = "HP0120",
  country = "Niger"  # Automatically crops to country boundary
)

plot(niger_maize, main = "Niger Maize Yield")
# Global: ~800 MB → Niger: ~8 MB (100x reduction!)
```

### Time Series Analysis with Country Cropping

``` r
# Download multiple time periods
results <- batch_download_gaez_datasets(
  crops = "MZE",
  time_periods = c("HP0120", "FP4160", "FP6180")  # 2001-2020, 2041-2060, 2061-2080
)

# Combine and crop to country
niger_timeseries <- combine_gaez_batch(
  results,
  country = "Niger",
  output_file = "niger_maize_timeseries.nc"
)

# Result: 3-layer NetCDF file, Niger extent only
```

## Main Functions

### New in v0.1.2 ⭐

-   **`load_gaez_data()`** - One-function workflow: download + load into R
-   **`combine_gaez_batch()`** - Combine batch results into multi-layer rasters
-   **`get_country_boundary()`** - Retrieve country boundaries for cropping
-   **Country cropping** - Automatic spatial subsetting in `load_gaez_data()` and `combine_gaez_batch()`

### Data Discovery

-   `lookup_gaez_variable()` - Find variable codes by name or code
-   `lookup_gaez_crop()` - Find crop codes by name
-   `list_gaez_crops()` - List all available crops

### Data Download

-   `download_gaez_dataset()` - Download a single dataset
-   `batch_download_gaez_datasets()` - Download multiple datasets with parallel processing
-   `build_gaez_url()` - Construct download URLs

### File Management

-   `list_downloaded_files()` - Inventory of local GAEZ files
-   `check_url_exists()` - Validate URLs before download
-   `verify_file_integrity()` - Check downloaded files
-   `get_download_cache()` - Get cache directory path
-   `clear_download_cache()` - Remove downloaded files

### Utilities

-   `show_gaez_examples()` - Display usage examples
-   `validate_climate_ssp()` - Validate climate/SSP combinations

## Common Use Cases

### 1. Historical vs Future Comparison

``` r
# Load historical data
hist <- load_gaez_data(crop = "WHE", time_period = "HP0120")

# Load future projection
future <- load_gaez_data(
  crop = "WHE",
  time_period = "FP4160",
  ssp = "SSP370"
)

# Calculate change
change <- future - hist
plot(change, main = "Wheat Yield Change (2041-2060 vs 2001-2020)")
```

### 2. Multi-Country Comparison

``` r
countries <- c("Niger", "Nigeria", "Burkina Faso", "Mali")
country_data <- list()

for (country in countries) {
  country_data[[country]] <- load_gaez_data(
    crop = "SOR",  # Sorghum
    time_period = "FP4160",
    country = country
  )
}

# Compare mean yields
yields <- sapply(country_data, function(r) global(r, "mean", na.rm = TRUE)[[1]])
barplot(yields, main = "Sorghum Yield by Country", las = 2)
```

### 3. Scenario Comparison

``` r
# Download multiple SSP scenarios
scenarios <- batch_download_gaez_datasets(
  crops = "MZE",
  time_period = "FP4160",
  ssps = c("SSP126", "SSP370", "SSP585")  # Low, medium, high emissions
)

# Combine with custom names
combined <- combine_gaez_batch(
  scenarios,
  layer_names = c("Low_emissions", "Medium_emissions", "High_emissions")
)

plot(combined)
```

### 4. Provincial-Level Analysis

``` r
# Get provincial boundaries
provinces <- get_country_boundary("Niger", level = 1)

# Load data cropped to provinces
data <- load_gaez_data(
  crop = "PMI",  # Pearl millet
  country = provinces
)

# Extract mean yield by province
stats <- extract(data, provinces, fun = mean, na.rm = TRUE)
provinces$yield <- stats[, 2]
plot(provinces, "yield", main = "Pearl Millet Yield by Province")
```

## GAEZ v5 Themes

GAEZ v5 data is organized into 6 themes:

1.  **Land and Water Resources** - Terrain, soils, land cover
2.  **Agro-climatic Resources** - Temperature, precipitation, growing periods
3.  **Agro-climatic Potential Yield** - Climate-constrained yield potential
4.  **Suitability and Attainable Yield** - Soil-climate yield potential
5.  **Actual Yields and Production** - Observed yields and production
6.  **Yield and Production Gaps** - Difference between potential and actual

## Documentation

### Getting Started

-   📖 [**Getting Started Vignette**](vignettes/getting-started.Rmd) - Comprehensive tutorial with all features
-   🚀 [**Quick Start Guide**](QUICK_START_COUNTRY_CROPPING.md) - Country cropping quick reference
-   📋 [**Package Summary**](PACKAGE_SUMMARY.md) - Overview of all functions

### Advanced Features

-   🌍 [**Country Cropping Guide**](COUNTRY_CROPPING_SUMMARY.md) - Detailed country-level analysis documentation
-   💡 [**Examples**](examples/) - Demonstration scripts
    -   [Country Cropping Demo](examples/country_cropping_demo.R)

### Reference

-   📰 [**NEWS**](NEWS.md) - Changelog and version history
-   ❓ [**Function Help**](man/) - Detailed function documentation

### Quick Reference

``` r
# View all examples
show_gaez_examples()

# Function help
?load_gaez_data
?combine_gaez_batch
?get_country_boundary

# Browse package documentation
help(package = "gaezv5")
```

## Key Features Explained

### Country-Level Cropping (v0.1.2)

Automatically crop GAEZ data to country boundaries:

**Benefits:** - 90-99% file size reduction - Faster loading and processing - Regional focus without manual cropping - Optional deletion of global files to save disk space

**Usage:**

``` r
# By country name
data <- load_gaez_data(crop = "MZE", country = "Niger")

# By ISO3 code (faster, no ambiguity)
data <- load_gaez_data(crop = "MZE", country = "NER")

# Delete global file to save space
data <- load_gaez_data(crop = "MZE", country = "Niger", keep_global = FALSE)

# Use custom boundary
boundary <- get_country_boundary("Niger", level = 1)  # Provinces
data <- load_gaez_data(crop = "MZE", country = boundary)
```

### Batch Operations (v0.1.1+)

Efficiently download and combine multiple datasets:

**Features:** - Parallel downloads (3-6x faster) - Automatic validation of time period/SSP combinations - Multi-layer raster creation - NetCDF export with compression

**Usage:**

``` r
# Download multiple files
results <- batch_download_gaez_datasets(
  crops = c("MZE", "WHE", "SOR"),
  time_periods = c("HP0120", "FP4160"),
  parallel = TRUE  # Default: uses parallel downloads
)

# Combine into single multi-layer raster
combined <- combine_gaez_batch(
  results,
  output_file = "crops_timeseries.nc",
  format = "netcdf"
)
```

### Smart Caching

All functions automatically check for existing downloads:

``` r
# First call: downloads file
data1 <- load_gaez_data(crop = "MZE", time_period = "HP0120")

# Second call: uses cached file (instant!)
data2 <- load_gaez_data(crop = "MZE", time_period = "HP0120")
```

## Best Practices

### 1. Use Country Cropping for Regional Studies

``` r
# ✓ GOOD: Direct country download (small, fast)
niger <- load_gaez_data(crop = "MZE", country = "Niger")

# ✗ BAD: Download global then crop manually (large, slow)
global <- load_gaez_data(crop = "MZE")
niger <- crop(global, niger_boundary)
```

### 2. Use ISO3 Codes to Avoid Ambiguity

``` r
# ✓ GOOD: Unambiguous
data <- load_gaez_data(crop = "MZE", country = "NER")

# ⚠ OKAY: Might match "Niger" or "Nigeria"
data <- load_gaez_data(crop = "MZE", country = "niger")
```

### 3. Export to NetCDF for Multi-Layer Data

``` r
# More efficient storage than multiple GeoTIFFs
combined <- combine_gaez_batch(
  results,
  output_file = "timeseries.nc",
  format = "netcdf"
)
```

### 4. Clean Up Global Files When Using Country Data

``` r
# Save disk space
data <- load_gaez_data(
  crop = "MZE",
  country = "Niger",
  keep_global = FALSE  # Deletes 800 MB global file
)
```

## Data Sources

All data is sourced from the FAO GAEZ v5 Data Portal:

-   **Portal**: https://gaez.fao.org/
-   **Model Documentation**: https://github.com/un-fao/gaezv5/wiki
-   **Google Cloud Storage**: https://storage.googleapis.com/fao-gismgr-gaez-v5-data/

## Citation

When using GAEZ v5 data, please cite:

> FAO & IIASA. 2025. Global Agro-ecological Zoning version 5 (GAEZ v5) Model Documentation. https://github.com/un-fao/gaezv5/wiki

For this R package:

``` r
citation("gaezv5")
```

## Contributing

Contributions are welcome! Please feel free to submit a Pull Request or open an issue.

-   **Issues**: https://github.com/jpwjoseph/gaezv5/issues
-   **Pull Requests**: https://github.com/jpwjoseph/gaezv5/pulls

## Version History

### v0.1.2 (Development) - Country Cropping Release

-   ⭐ NEW: `load_gaez_data()` - One-function download + load workflow
-   ⭐ NEW: `combine_gaez_batch()` - Multi-layer raster combination
-   ⭐ NEW: `get_country_boundary()` - Country boundary retrieval
-   ⭐ NEW: Country-level cropping in both load and batch functions
-   📦 Added `geodata` dependency for GADM boundaries
-   📊 NetCDF export support with compression

### v0.1.1 - Parallel Downloads & Time Series

-   ⚡ Parallel downloads via `curl::multi_download()`
-   📅 Multiple time period support in batch operations
-   🔧 Enhanced parameter handling (accepts lists or vectors)
-   🐛 Bug fixes for batch operations

### v0.1.0 - Initial Release

-   📥 Basic download functionality
-   🔍 Data discovery functions
-   📁 File management utilities
-   ✅ Comprehensive validation

See [NEWS.md](NEWS.md) for complete changelog.

## Author

**Julian Joseph** (IIASA) Email: joseph\@iiasa.ac.at ORCID: [0000-0002-3844-7807](https://orcid.org/0000-0002-3844-7807)

## Acknowledgments

-   FAO and IIASA for creating and maintaining GAEZ v5
-   R Core Team and package developers
-   Contributors to the terra, geodata, and tidyverse packages

## See Also

-   [GAEZ v5 Portal](https://gaez.fao.org/) - Official data portal
-   [GAEZ v5 Wiki](https://github.com/un-fao/gaezv5/wiki) - Model documentation
-   [terra](https://rspatial.org/terra/) - Spatial data handling in R
-   [geodata](https://github.com/rspatial/geodata) - Geographic data access

------------------------------------------------------------------------

**Status**: Active Development \| **Version**: 0.1.2 \| **Last Updated**: 2025-10-07