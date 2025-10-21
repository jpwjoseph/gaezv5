## Release Summary

This is the first CRAN submission of the gaezv5 package, version 0.2.0.

The package provides tools for downloading, processing, and analyzing Global Agro-Ecological Zones (GAEZ) version 5 data from the FAO. It is a data handling package (not statistical processing) that simplifies access to comprehensive global agricultural data.

## Test Environments

* Local: Windows 10, R 4.3.x / 4.4.x
* GitHub Actions CI/CD: Windows, macOS, Ubuntu (latest)
* WinBuilder: Windows (current, old-release, development versions)
* R-hub: Multiple platforms and R versions

## R CMD check Results

✓ No ERRORs
✓ No WARNINGs
✓ No NOTEs

Local check performed with: `R CMD check --as-cran gaezv5_0.2.0.tar.gz`

## Reverse Dependencies

Not applicable - this is a new package with no known reverse dependencies.

## Data Handling Notes

This package is designed to download and process geographic raster data from external sources (Google Cloud Storage). The package itself does not include large data files - example/test data are downloaded at runtime as needed. All internal reference data (variable tables, crop codes, scenarios) are stored as compact R data objects (~5.4 KB total).

Package size: ~200 KB (without downloads)

### Smart Vignette with Conditional Code Execution

The vignette is included in the CRAN submission with intelligent code evaluation:

**Functions that Execute on CRAN** (eval=TRUE):
- `list_gaez_crops()` - Lists available crops
- `get_gaez_variables()` - Retrieves all variables
- `list_gaez_scenarios()` - Shows available scenarios
- `lookup_gaez_crop()` - Looks up crop codes
- `lookup_gaez_variable()` - Looks up variable codes
- `validate_climate_ssp()` - Validates parameter combinations
- `show_gaez_examples()` - Displays package examples

These functions access only internal package data (no internet required) and demonstrate the package's core data discovery capabilities.

**Functions that Only Execute Locally** (eval=identical(Sys.getenv("NOT_CRAN"), "true")):
- `build_gaez_url()` - URL construction
- `check_url_exists()` - URL validation
- `download_gaez_dataset()` - Downloads data
- `load_gaez_data()` - Loads and processes data
- `batch_download_gaez_datasets()` - Batch operations
- `preview_gaez_map()` - Interactive map previews
- `combine_gaez_batch()` - Combines multiple datasets

These functions require internet access to Google Cloud Storage and FAO ImageServer, so they only execute when NOT_CRAN environment variable is set (i.e., locally or in CI/CD, not during CRAN checks).

**Benefits of This Approach**:
- ✓ Vignette included in CRAN package
- ✓ Safe functions demonstrate core functionality on CRAN
- ✓ No errors from external services during checks
- ✓ Users installing from GitHub see full vignette with all examples
- ✓ Follows R-Packages best practices for data-access packages

## Notes for CRAN Maintainers

### Package Purpose

gaezv5 is a data access and processing tool for the GAEZ v5 dataset. Key features include:
- Interactive map previews via REST API (100-300x faster than full downloads)
- Batch downloading with parallel processing
- Country-level spatial subsetting
- Comprehensive validation and error handling
- Time series support and scenario management

### Dependencies

All dependencies are from CRAN:
- `terra` (>= 1.5.0): Spatial raster operations
- `curl` (>= 5.0.0): Efficient parallel downloads
- `httr`, `stringr`, `dplyr`, `tidyr`, `purrr`, `tibble`: Data manipulation
- `leaflet` (>= 2.1.0): Interactive map visualization
- `geodata` (>= 0.5.0): Country boundary retrieval
- `tools`: File utilities

### Platform Compatibility

Tested and confirmed working on:
- Windows 11
- Ubuntu Linux 

No platform-specific code; uses only cross-platform R packages and functions.

### External Service Dependencies

The package interacts with legitimate, stable external services:
- **Google Cloud Storage**: GAEZ data repository (read-only)
- **FAO ImageServer REST API**: Map tile service (read-only)
- **GADM Database**: Country boundary data (via geodata package)

All external calls include:
- Connection timeouts (default: 30 seconds)
- Error handling and user-friendly messages
- Validation of URLs before download
- File integrity checks

No data is sent externally without explicit user request via function calls.

### Performance

- Multithreading uses maximum 2 concurrent connections 
- Average function execution time < 5 seconds for validation/lookup operations
- Download functions scale with file size (typical: 10-30 seconds for ~800 MB files)
- All tests complete in < 10 seconds

### Code Quality

- Full roxygen2 documentation 
- Comprehensive @examples for all exported functions
- Defensive programming with input validation
- Appropriate use of warnings, messages, and errors
