# gaezv5: Download and Process GAEZ v5 Data

<!-- badges: start -->

[![R-CMD-check](https://github.com/jpwjoseph/gaezv5/workflows/R-CMD-check/badge.svg)](https://github.com/jpwjoseph/gaezv5/actions) [![License: GPL v3](https://img.shields.io/badge/License-GPLv3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)

<!-- badges: end -->

An R package for downloading and working with Global Agro-Ecological Zones (GAEZ) version 5 data from the Food and Agriculture Organization (FAO) and the International Institute for Applied Systems Analysis (IIASA). GAEZ v5 provides comprehensive global data on agricultural potential, crop suitability, attainable yields, and agro-climatic resources.

## Features

-   **Intuitive Interface**: Simple functions for searching and downloading GAEZ data
-   **Comprehensive Coverage**: Access to all 6 GAEZ themes and 100+ variables
-   **Batch Downloads**: Download multiple crops, scenarios, or time periods at once
-   **Smart Caching**: Automatically detects existing downloads to avoid duplicates
-   **Validation**: Built-in checks for parameter compatibility and data integrity
-   **Well Documented**: Extensive help files, vignettes, and examples

## Installation

You can install the development version from GitHub:

``` r
# install.packages("devtools")
devtools::install_github("jpwjoseph/gaezv5")
```

## Quick Start

``` r
library(gaezv5)

# 1. Browse available crops
crops <- list_gaez_crops()
print(head(crops))

# 2. Download maize yield data for historical period (2001-2020)
result <- download_gaez_dataset(
  crop = "maize",
  time_period = "HP0120",
  climate_model = "AGERA5"
)

# 3. Load and plot the data
library(terra)
r <- rast(result$file_path)
plot(r, main = "Maize Attainable Yield (2001-2020)")

# 4. Download multiple crops in batch
results <- batch_download_gaez_datasets(
  crops = c("maize", "wheat", "sorghum"),
  time_period = "HP0120",
  climate_model = "AGERA5"
)
```

## Main Functions

### Data Discovery

-   `lookup_gaez_variable()` - Find variable codes by name or code
-   `lookup_gaez_crop()` - Find crop codes by name
-   `list_gaez_crops()` - List all available crops

### Data Download

-   `download_gaez_dataset()` - Download a single dataset
-   `batch_download_gaez_datasets()` - Download multiple datasets
-   `build_gaez_url()` - Construct download URLs

### File Management

-   `list_downloaded_files()` - Inventory of local GAEZ files
-   `check_url_exists()` - Validate URLs before download
-   `verify_file_integrity()` - Check downloaded files
-   `clear_download_cache()` - Remove downloaded files

### Utilities

-   `show_gaez_examples()` - Display usage examples
-   `validate_climate_ssp()` - Validate climate/SSP combinations

## GAEZ v5 Themes

GAEZ v5 data is organized into 6 themes:

1.  **Land and Water Resources** - Terrain, soils, land cover
2.  **Agro-climatic Resources** - Temperature, precipitation, growing periods
3.  **Agro-climatic Potential Yield** - Climate-constrained yield potential
4.  **Suitability and Attainable Yield** - Soil-climate yield potential
5.  **Actual Yields and Production** - Observed yields and production
6.  **Yield and Production Gaps** - Difference between potential and actual

## Examples

### Download Future Climate Scenario

``` r
# Download maize yield for 2041-2060 under SSP3-7.0 scenario
result <- download_gaez_dataset(
  crop = "maize",
  time_period = "FP4160",
  climate_model = "ENSEMBLE",
  ssp = "SSP370"
)
```

### Compare Irrigated vs Rain-fed

``` r
# Download both water management scenarios
results <- batch_download_gaez_datasets(
  crops = "rice",
  water_management_levels = c("HRLM", "HILM"),  # Rain-fed vs Irrigated
  time_period = "HP0120"
)
```

### Search and Download

``` r
# Find yield-related variables
var <- lookup_gaez_variable("attainable yield")

# Look up pearl millet crop code
crop_code <- lookup_gaez_crop("pearl millet", theme = 4)

# Download the data
result <- download_gaez_dataset(
  variable = var$variable_code,
  crop = crop_code,
  time_period = "HP0120"
)
```

## Data Sources

All data is sourced from the FAO GAEZ v5 Data Portal: - Portal: https://gaez.fao.org/ - Documentation: https://www.fao.org/nr/gaez/ - Google Cloud Storage: https://storage.googleapis.com/fao-gismgr-gaez-v5-data/

## Citation

When using GAEZ v5 data, please cite:

> FAO and IIASA. 2025. Global Agro-Ecological Zones v5 (GAEZ v5). FAO, Rome, Italy and IIASA, Laxenburg, Austria.

For this R package:

``` r
citation("gaezv5")
```

## Contributing

Contributions are welcome! Please feel free to submit a Pull Request.

## License

GPL (\>= 3)

## Author

Julian Joseph (IIASA) Email: joseph\@iiasa.ac.at

## Acknowledgments

-   FAO and IIASA for creating and maintaining GAEZ v5
-   R Core Team and package developers

## See Also

-   [GAEZ v5 Portal](https://gaez.fao.org/)
-   [terra](https://rspatial.org/terra/) - Spatial data handling