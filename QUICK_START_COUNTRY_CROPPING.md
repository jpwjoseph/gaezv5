# Quick Start: Country Cropping in gaezv5

## Installation

```r
# Install/update the package
devtools::install_github("jpwjoseph/gaezv5")

# Load package
library(gaezv5)
```

## Basic Usage

### Load country-level data (simplest approach)

```r
# Load Niger maize yield data (2001-2020)
niger_maize <- load_gaez_data(
  crop = "maize",
  time_period = "HP0120",
  country = "Niger"
)

# Plot the data
terra::plot(niger_maize, main = "Niger Maize Yield")
```

### Compare global vs country data

```r
# Global data
global <- load_gaez_data(crop = "wheat", country = NULL)
cat("Global size:", object.size(global) / 1024^2, "MB\n")

# Country data
niger <- load_gaez_data(crop = "wheat", country = "Niger")
cat("Niger size:", object.size(niger) / 1024^2, "MB\n")
cat("Reduction:", round((1 - object.size(niger)/object.size(global)) * 100, 1), "%\n")
```

## Advanced Features

### Option 1: Use ISO3 codes (fastest, no ambiguity)

```r
data <- load_gaez_data(
  crop = "sorghum",
  country = "NER"  # ISO3 code for Niger
)
```

### Option 2: Delete global files to save space

```r
data <- load_gaez_data(
  crop = "rice",
  country = "Niger",
  keep_global = FALSE  # Deletes global file after cropping
)
```

### Option 3: Crop to extent only (no masking)

```r
data <- load_gaez_data(
  crop = "millet",
  country = "Niger",
  mask_to_boundary = FALSE  # Just crop to bounding box
)
```

## Time Series Analysis

### Download and combine multiple time periods for a country

```r
# Step 1: Batch download
results <- batch_download_gaez_datasets(
  crops = "maize",
  time_periods = c("HP0120", "FP4160", "FP6180")
)

# Step 2: Combine with country cropping
timeseries <- combine_gaez_batch(
  results,
  country = "Niger",
  output_file = "niger_maize_timeseries.nc"
)

# Step 3: Analyze changes
change_1 <- timeseries[[2]] - timeseries[[1]]  # HP0120 to FP4160
change_2 <- timeseries[[3]] - timeseries[[2]]  # FP4160 to FP6180

terra::plot(c(change_1, change_2),
            main = c("Change: 2001-2020 to 2041-2060",
                     "Change: 2041-2060 to 2061-2080"))
```

## Multi-Country Comparison

```r
# Compare multiple countries
countries <- c("Niger", "Nigeria", "Burkina Faso", "Mali")
results <- list()

for (country in countries) {
  results[[country]] <- load_gaez_data(
    crop = "sorghum",
    time_period = "FP4160",
    country = country,
    verbose = FALSE
  )
}

# Calculate mean yields
yields <- sapply(results, function(r) mean(terra::values(r), na.rm = TRUE))
barplot(yields, main = "Mean Sorghum Yield by Country",
        ylab = "Yield (kg/ha)", las = 2)
```

## Provincial/Regional Analysis

```r
# Get provincial boundaries (administrative level 1)
provinces <- get_country_boundary("Niger", level = 1)

# Load data cropped to provinces
data <- load_gaez_data(
  crop = "millet",
  country = provinces
)

# Extract mean yield by province
province_yields <- terra::extract(data, provinces, fun = mean, na.rm = TRUE)
provinces$yield <- province_yields[, 2]

# Plot
terra::plot(provinces, "yield", main = "Millet Yield by Province")
```

## Custom Boundaries

```r
# Use your own boundary shapefile
library(terra)
my_boundary <- vect("my_study_area.shp")

# Crop to custom boundary
data <- load_gaez_data(
  crop = "wheat",
  country = my_boundary
)
```

## Common Workflows

### Workflow 1: Single country, single time period

```r
data <- load_gaez_data(
  crop = "maize",
  time_period = "HP0120",
  country = "Niger"
)
```

### Workflow 2: Single country, multiple crops

```r
results <- batch_download_gaez_datasets(
  crops = c("maize", "wheat", "sorghum", "millet"),
  time_period = "HP0120"
)

combined <- combine_gaez_batch(
  results,
  country = "Niger",
  output_file = "niger_crops.nc"
)
```

### Workflow 3: Single country, scenario comparison

```r
results <- batch_download_gaez_datasets(
  crops = "wheat",
  time_period = "FP4160",
  ssps = c("SSP126", "SSP370", "SSP585")
)

scenarios <- combine_gaez_batch(
  results,
  country = "Niger",
  layer_names = c("Low emissions", "Medium emissions", "High emissions")
)
```

### Workflow 4: Storage-efficient (delete global files)

```r
# All crops, country-level only, minimal storage
results <- batch_download_gaez_datasets(
  crops = c("maize", "wheat", "rice", "sorghum"),
  time_period = "HP0120"
)

combined <- combine_gaez_batch(
  results,
  country = "Niger",
  output_file = "niger_all_crops.nc",
  keep_global = FALSE  # Saves ~90% storage space
)
```

## Tips & Tricks

### Tip 1: Check available countries

```r
library(geodata)
codes <- country_codes()
head(codes[, c("NAME", "ISO3")], 20)
```

### Tip 2: Cache boundaries for reuse

```r
# First call downloads boundary
niger <- get_country_boundary("Niger")

# Subsequent calls use cached version (fast!)
niger2 <- get_country_boundary("Niger")
```

### Tip 3: Handle ambiguous country names

```r
# "United States" might match multiple entries
# Use ISO3 code instead:
usa <- get_country_boundary("USA")
```

### Tip 4: Check file sizes

```r
# List downloaded files
files <- list_downloaded_files()
total_gb <- sum(files$size_mb) / 1024
cat("Total storage:", round(total_gb, 2), "GB\n")
```

## Troubleshooting

### Problem: Country not found

```r
# Error: Country 'Nigr' not found
# Solution: Check spelling or use ISO3 code
boundary <- get_country_boundary("NER")
```

### Problem: All values are NA after cropping

This usually means the country doesn't overlap with the raster extent. Check:
- Country name spelling
- Raster extent covers the country
- CRS compatibility (automatically handled)

### Problem: Out of memory

For very large datasets, crop to country first:
```r
# Instead of loading full global data
data <- load_gaez_data(crop = "wheat", country = "Niger")  # Much smaller!
```

## Help & Documentation

```r
# Function help
?get_country_boundary
?load_gaez_data
?combine_gaez_batch

# Package vignettes
browseVignettes("gaezv5")

# Examples
gaezv5::show_gaez_examples()
```

## More Information

- Full documentation: `COUNTRY_CROPPING_SUMMARY.md`
- Demo script: `examples/country_cropping_demo.R`
- Test script: `tests/test_country_cropping.R`
- Package NEWS: `NEWS.md`

---

**Quick Reference Card**

| Task | Code |
|------|------|
| Load country data | `load_gaez_data(crop="X", country="Niger")` |
| Use ISO3 code | `country = "NER"` |
| Delete global file | `keep_global = FALSE` |
| Crop only (no mask) | `mask_to_boundary = FALSE` |
| Provincial level | `get_country_boundary("Niger", level=1)` |
| Batch + country | `combine_gaez_batch(results, country="Niger")` |
| NetCDF export | `output_file = "file.nc"` |
| Custom boundary | `country = my_spatvector` |
