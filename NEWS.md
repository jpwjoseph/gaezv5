# gaezv5 0.2.1

## Bug fixes

* `build_gaez_url()` and the download functions work for theme 1, theme 2 and
  the annual time-series variables (`*-TS`). Earlier versions stopped with
  "Theme must be 3, 4, 5, or 6" for these variables.
* Theme-1 maps are read from `MAP/GAEZ-V5.<code>.tif`. Variable codes follow
  FAO: `ELEV-ALOS-30M` and `SLOPE-MED` (previously `SOILFER.*`).
* Themes 2 and 3 have no ENSEMBLE layers. Future periods of these themes now
  stop with the list of available climate models instead of requesting a
  file that does not exist.
* Downloads are written to a `.part` file and renamed only after an HTTP 200
  response with the expected size. Error responses leave no file behind and
  are reported as failures in `batch_download_gaez_datasets()` (previously a
  404 response was saved as `.tif` and reported as success). Existing files
  that are not GeoTIFFs are downloaded again.
* Single downloads no longer depend on `options(timeout)`, so large ~1 km
  files complete on slow connections.
* `batch_download_gaez_datasets(parallel = TRUE)` passes `water_supply`,
  `resolution`, `class` and `interactive` from `...` to the URL builder.
* `preview_gaez_map()` previously queried the FAO ArcGIS ImageServer, which
  serves GAEZ v4 data (CRU TS 3.2 baseline, RCP scenarios, 30-year periods).
  It now reads a reduced-resolution overview of the GAEZ v5 Cloud-Optimized
  GeoTIFF directly from the FAO bucket. A global preview of a ~1 km layer
  transfers under 1 MB.

## New arguments

* `resolution = "1km"` selects the ~1 km layers (theme 4 variables available
  at both resolutions, and AEZ33). These layers cover HP8100 and HP0120 only.
* `class` selects one raster of the per-class theme-1 variables `LR-LCC`
  (land cover share, LC01-LC12) and `LR-SLC` (slope class share, SC01-SC10).
* Time-series variables take a single year via `start_year` or `time_period`
  (1980-2022); `batch_download_gaez_datasets(time_periods = <years>)`
  downloads several years.
* `options(gaezv5.gcs_root = ...)` changes the bucket root URL.

## Data

* Crop codes and names come from FAO's code lists
  (`DIMENSION/GAEZ-V5.CROP-RES0x.csv`): 115 crops for theme 3 and 78 for
  theme 4, including avocado, teff, fonio, okra, taro, tannia and watermelon.
  Theme-3 codes `FONI`, `MANGO`, `CYAM1`-`CYAM3` are now `FONIO`, `MANG`,
  `TARODL`, `TAROWL`, `TANN`.
* Added `HWSDV22` and `LR-SLC`; `LR-LCC` is a per-class mapset.
* Input-level descriptions follow FAO (available water content 200 mm/m,
  rain-fed or irrigated, high or low input).

## Other changes

* `validate_gaez_parameters()` is exported and returns the resolved URL.
* `httr::GET`, `httr::content` and `utils::download.file` are no longer used.

# gaezv5 0.2.0

## New Features

### Interactive Map Previews via REST API ⭐

* **`preview_gaez_map()`** - New function for fast, interactive data visualization without downloading
  - Creates interactive leaflet maps using FAO ImageServer REST API
  - 100-300x faster than downloading full datasets for exploration
  - Supports global, country-level, and custom bounding box extents
  - Automatic color scaling with 10-class diverging palettes
  - Display in Positron and RStudio Viewer, browser, or save as HTML widget
  - Bandwidth-efficient: ~100 KB preview vs ~800 MB full dataset

* **REST API Infrastructure** (internal functions)
  - `build_imageserver_url()` - Smart theme-to-service mapping (res01-res07)
  - `fetch_imageserver_tile()` - Downloads preview tiles from ImageServer
  - `check_imageserver_availability()` - Validates REST API access for themes
  - Proper handling of ImageServer georeferencing and extent management
  - Automatic bbox coordinate transformation (terra format ↔ ImageServer format)

### Use Cases

The new preview function is ideal for:
- Quick data exploration before committing to large downloads
- Teaching and demonstrations with interactive maps
- Model validation by visual inspection
- Verifying spatial coverage for regions of interest
- Comparing scenarios side-by-side in reports

### Examples

```r
# Global preview
preview_gaez_map(
  variable = "RES05-YX",
  crop = "wheat",
  time_period = "FP4160",
  ssp = "SSP370"
)

# Country-level preview
preview_gaez_map(
  variable = "RES05-YX",
  crop = "maize",
  time_period = "HP0120",
  country = "China"
)
```

## Changes

* **Added leaflet to Imports**
  - Required for interactive map visualization
  - Version: leaflet (>= 2.1.0)

* **ImageServer Theme Mapping**
  - Fixed incorrect theme-to-service mappings (5 out of 6 themes were wrong)
  - Now uses variable code prefixes as primary mapping method (more reliable)
  - All 6 themes now accessible via REST API

## Improvements

* **Enhanced year validation** across all functions
  - `preview_gaez_map()` now validates year ranges (1981-2100) with informative error messages
  - Consistent validation between preview and download functions
  - Clear listing of available time periods when years are out of range
  - Support for interactive period selection when year range spans multiple periods

## Bug Fixes

* Fixed bbox coordinate order transformation for ImageServer API
  - `terra::ext()` returns (xmin, xmax, ymin, ymax)
  - ImageServer expects (xmin, ymin, xmax, ymax)
  - Proper reordering prevents invalid extent errors

* Fixed extent handling for REST API responses
  - Trust ImageServer georeferencing instead of forcing extents
  - Prevents "[ext] invalid extent" errors on large countries

* Fixed crop lookup for themes 1 & 2
  - Made crop lookup conditional (only for themes 3-6)
  - Prevents errors when previewing non-crop variables

* Fixed data type preservation in country cropping
  - Added `datatype="FLT4S"` parameter to `writeRaster()`
  - Prevents float value corruption when caching country-cropped files

## Performance Improvements

* REST API previews are 100-300x faster than full downloads
* Reduced bandwidth usage: ~100 KB (preview) vs ~800 MB (full dataset)
* Instant country-level previews via bbox + boundary masking

---

# gaezv5 0.1.2

## New Features

### Country-Level Cropping ⭐
* **`get_country_boundary()`** - New helper function for retrieving country boundaries
  - Accepts country names, ISO3 codes, or custom SpatVector objects
  - Downloads GADM boundaries with automatic caching
  - Supports multiple administrative levels (0=country, 1=provinces, etc.)
  - Smart country matching with fuzzy search and error suggestions

* **`load_gaez_data()` now supports country cropping**
  - New `country` parameter for automatic cropping to country-level data
  - New `mask_to_boundary` parameter (TRUE=mask, FALSE=crop to extent only)
  - New `keep_global` parameter to control global file retention
  - Cropped files automatically named with "_[ISO3]" suffix
  - Significantly reduces file sizes for regional analysis
  - Examples:
    - `load_gaez_data(crop = "maize", country = "Niger")`
    - `load_gaez_data(crop = "wheat", country = "NER", keep_global = FALSE)`

* **`combine_gaez_batch()` now supports country cropping**
  - Same country cropping parameters as `load_gaez_data()`
  - Crops all layers in batch to specified country
  - Automatic NetCDF filename updates with ISO3 codes
  - Option to delete global files after cropping
  - Perfect for country-specific time series and scenario analysis

### Data Loading and Combination

* **`load_gaez_data()`** - Streamlined one-function workflow for downloading and loading GAEZ data
  - Automatically checks cache and downloads if needed
  - Returns terra SpatRaster object ready for analysis
  - Optional metadata return with `return_metadata = TRUE`
  - Simplifies workflow: `maize <- load_gaez_data(crop = "maize", time_period = "HP0120")`

* **`combine_gaez_batch()`** - Combine batch downloads into multi-layer datasets
  - Takes `batch_download_gaez_datasets()` output and creates single SpatRaster
  - Automatic layer naming from batch result parameters
  - Optional NetCDF export with compression
  - Ideal for time series analysis and scenario comparisons
  - Example: Download multiple time periods and combine into single file

## Changes

* **Added geodata to Imports**
  - Required for GADM boundary downloads in country cropping features
  - Version: geodata (>= 0.5.0)

* **Moved terra from Suggests to Imports**
  - terra is now a core dependency for new loading/combination functions
  - Existing functions still use `requireNamespace()` for backward compatibility

## Performance Improvements

* **Reduced file sizes**: Country-level data is 10-100x smaller than global datasets
* **Faster loading**: Smaller cropped files load much faster into R
* **Storage efficiency**: Optional deletion of global files saves disk space
* **Smart caching**: Cropped files are cached and reused on subsequent calls

---

# gaezv5 0.1.1

## Major Enhancements

### Parallel Downloads
* **Parallel download support** in `batch_download_gaez_datasets()` using `curl::multi_download()`
  - 3-6x faster for multiple files (depending on network and HTTP/2 support)
  - Built-in progress bar showing download status
  - Automatic HTTP/2 multiplexing when server supports it
  - Resume capability for interrupted downloads
  - Set `parallel = FALSE` to use sequential mode for debugging

### Time Series Support
* **Multiple time period downloads** in single batch operation
  - Download historical and future periods together: `c("HP0120", "FP4160", "FP6180")`
  - Automatic SSP/climate model pairing based on time period
  - Smart validation filters invalid combinations (e.g., historical + future SSP)
  - Ideal for climate change impact studies and time series analysis

### Enhanced Parameter Handling
* **Flexible parameter types** - now accepts both vectors and lists
  - `crops = c("MZE", "WHE")` and `crops = list("MZE", "WHE")` both work
  - Automatic type conversion for all vector parameters
  - Fully backward compatible with existing code

### Improved Documentation
* Updated `batch_download_gaez_datasets()` documentation
  - Added "Parameter Flexibility" section
  - Added "Parallel Downloads" section with performance notes
  - Added "Time Period and SSP/Climate Model Validation" details
  - New examples showing time series downloads and parallel options

## Bug Fixes

* Fixed sequential mode parameter passing in `batch_download_gaez_datasets()`
  - Resolved "unused arguments" error when calling `download_gaez_dataset()`
  - Proper dots filtering to prevent parameter conflicts
* Fixed `expand.grid()` type errors with list parameters
  - Added automatic conversion of lists to atomic vectors
  - Function now handles mixed list/vector parameter input gracefully
* Enhanced validation logic for time period/SSP/climate model combinations

## Performance Improvements

* Parallel downloads using curl's native multi-download capability
* Reduced redundant file existence checks
* Optimized URL building for batch operations
* Total download time reporting for batch operations

## Dependencies

* Added `curl (>= 5.0.0)` to Imports for parallel download support

---

# gaezv5 0.1.0

## Initial Release

The `gaezv5` package provides comprehensive tools for downloading and working with Global Agro-Ecological Zones (GAEZ) version 5 data from the Food and Agriculture Organization (FAO) and the International Institute for Applied Systems Analysis (IIASA).

### Core Features

* **Data Download**:
  - `download_gaez_dataset()` - Download single datasets with comprehensive error handling
  - `batch_download_gaez_datasets()` - Efficiently download multiple datasets
  - `build_gaez_url()` - Construct GAEZ v5 download URLs

* **Data Discovery**:
  - `lookup_gaez_variable()` - Find variable codes with fuzzy matching
  - `lookup_gaez_crop()` - Find crop codes across different themes
  - `list_gaez_crops()` - List available crops with optional filtering

* **File Management**:
  - `list_downloaded_files()` - Inventory of local GAEZ files
  - `check_url_exists()` - Validate URLs before download
  - `verify_file_integrity()` - Check downloaded file validity
  - `get_download_cache()` - Get cache directory path
  - `clear_download_cache()` - Remove downloaded files

* **Validation and Utilities**:
  - `validate_climate_ssp()` - Validate climate model and SSP combinations
  - `show_gaez_examples()` - Display usage examples

### Data Objects

* `gaez_variables` - Table of all GAEZ variables with codes, names, themes, and requirements
* `gaez_crops` - Complete crop code tables for themes 3-6
* `gaez_scenarios` - Time periods, climate models, SSP scenarios, and water management codes
* `gaez_url_structure` - URL construction patterns

### Documentation

* Comprehensive roxygen2 documentation for all functions
* Package-level documentation with getting started guide
* Vignette: "Getting Started with gaezv5" - Basic usage and common workflows
* README with installation instructions and quick start examples

### Testing

* Comprehensive test suite using testthat framework
* Unit tests covering lookup functions, validation, utilities, and data integrity

## Future Plans

### Version 0.2.0 (Planned)

* **Map Preview & Query via REST API**:
  - `preview_gaez_map()` - Visual inspection of maps without downloading full datasets
  - `query_gaez_value()` - Extract pixel values at specific coordinates
  - `sample_gaez_data()` - Batch sampling at multiple locations for validation
  - `get_gaez_stats()` - Regional summary statistics without processing large files
  - Leverages FAO's ArcGIS ImageServer REST API for bandwidth-efficient data exploration
  - 100-300x faster for exploratory analysis compared to full downloads

* **Spatial Operations**:
  - Extract zonal statistics

* **Visualization**:
  - Quick visualization functions
  - Side-by-side scenario comparison

### Version 0.3.0 (Planned)

* **Advanced Analytics**:
  - Yield gap analysis functions
  - Climate change impact assessment tools
  - Multi-crop suitability analysis

## Notes

This is the first public release of the gaezv5 package. Feedback and contributions are welcome via GitHub issues and pull requests at https://github.com/jpwjoseph/gaezv5.
