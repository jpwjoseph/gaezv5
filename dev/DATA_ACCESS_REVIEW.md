# GAEZ v5 data access in `gaezv5`: review and revision plan

Date of tests: 2026-11-04. Package state: branch `claude/nice-mayer-2npq25`, version 0.2.0.
All tests can be rerun with the scripts in `dev/data-access-review/` (section 8).

## 1. Summary

1. **Bulk path (Google Cloud Storage).** It works for themes 3–6 and fails for themes 1, 2 and the
   annual time series. 8 of 15 test cases returned HTTP 200. The other 7 never reach the network,
   because `build_gaez_url()` runs a crop lookup for every theme. Three further defects make
   downloads fail or report false success:
   - Theme‑1 paths are wrong.
   - The default climate model `ENSEMBLE` does not exist for themes 2–3.
   - The parallel batch path records HTTP 404 responses as successful downloads and leaves an
     XML stub saved as `.tif`.
2. **Map path (ArcGIS ImageServer, `gaez-services.fao.org`).** It could not be called from the
   test environment, because the host is outside its network allow‑list. Static analysis shows it
   cannot deliver GAEZ v5 data in any case:
   - The package translates v5 parameters into GAEZ v4 attributes: CRU TS 3.2 history, RCP
     scenarios, 30‑year v4 periods.
   - FAO's service metadata labels the res01/res02/res05/res07/LR services as GAEZ v4.
   - When the request succeeds, `preview_gaez_map()` therefore renders v4 data under a v5 title.
3. **FAO's current guidance** directs users to the same GCS bucket. That guidance covers the
   [Platform User Guide](https://github.com/un-fao/gaezv5/wiki/21.-GAEZ-v5-Platform-User-Guide)
   and the [Data Access Notebooks](https://github.com/un-fao/gaezv5/tree/main/GAEZ-v5-Platform/Data-Access-Notebooks),
   last updated 2026‑09‑29. FAO resolves file names from a published catalog rather than from
   hard‑coded rules. The bucket also carries a machine‑readable catalog (`DIMENSION/*.csv`, one
   JSON sidecar per raster), and every raster tested is a Cloud‑Optimized GeoTIFF (COG). The
   package uses neither.
4. **Revision direction.**
   - Resolve files from FAO's live catalog, with a cache.
   - Read COG windows and overviews remotely instead of downloading global files.
   - Drop the v4 ImageServer.
   - Add the FAO GISMGR 2.0 API as a second URL provider.
   - Add a weekly canary that detects upstream changes.

   Measured effect for a 1 km layer:
   - Country subset: 5.07 MB transferred instead of 287 MB.
   - Global preview: 0.34 MB in 2.8 s, against 58–86 s for terra's default resampling path.

## 2. How the package pulls data today

| Path | Call chain | Endpoint |
|---|---|---|
| Single download | `download_gaez_dataset()` → `build_gaez_url()` assembles a URL from internal tables (`R/sysdata.rda`) → `httr::HEAD()` → `utils::download.file()` → `Data/GAEZ/` in the working directory | `https://storage.googleapis.com/fao-gismgr-gaez-v5-data/DATA/GAEZ-V5/<MAP\|MAPSET>/<data_code>/GAEZ-V5.<parts>.tif` (`R/url_builder.R:139`, `:221`) |
| Batch download | `batch_download_gaez_datasets(parallel = TRUE)` → URLs built upfront → `curl::multi_download(resume = TRUE)` (`R/download.R:871`) | same |
| Load + crop | `load_gaez_data()` → full global download → `terra::rast()` → `crop()`/`mask()` → writes `<file>_<ISO3>.tif` | same |
| Map preview | `preview_gaez_map()` → `build_imageserver_url()` (`res01`–`res07`) → v5→v4 attribute mapping → `POST exportImage` with `mosaicRule`, `renderingRule = Clip`, `pixelType = "S16"` → `GET href` → `terra::rast()`; on failure falls back to `load_gaez_data()` (`R/preview.R:345`, `:424`) | `https://gaez-services.fao.org/server/rest/services/<res0X>/ImageServer/exportImage` (`R/rest_api.R:45`) |

## 3. Test results

### 3.1 GCS bulk path: URL construction and existence (HEAD)

| Case | Package result | HTTP | Size |
|---|---|---|---|
| T1 `AEZ57`, `LR-IRR`, `HWSDV201`, `SOILFER.SLOPE-MED` | error `Theme must be 3, 4, 5, or 6` (`R/url_builder.R:127` → `R/lookup.R:197`) | — | — |
| T2 `RES01-PRC` (HP0120, FP4160) | same error | — | — |
| T2 time series `RES01-PRC-TS` | same error | — | — |
| T3 `RES02-YLD` maize HP0120 | `MAPSET/RES02-YLD/GAEZ-V5.RES02-YLD.HP0120.AGERA5.HIST.MAIZ.HRLM.tif` | 200 | 5.6 MB |
| T4 `RES05-YX` defaults (FP4160, ENSEMBLE, SSP370, WHE, HRLM) | `MAPSET/RES05-YXX/…` | 200 | 4.6 MB |
| T4 `RES05-YX` maize HP0120 | `MAPSET/RES05-YXX/…HIST.MZE.HRLM.tif` | 200 | 3.6 MB |
| T4 `RES05-SI`, `RES05-YC` | `RES05-SIX`, `RES05-YCX` (10 km) | 200 | 0.6, 4.9 MB |
| T5 `RES06-YLD` maize, `RES06-HAR` rice WST | `…RES06-YLD.MZE.WSR.tif`, `…RES06-HAR.RCW.WST.tif` | 200 | 4.8, 3.5 MB |
| T6 `RES07-YGA-CL` maize | `…RES07-YGA-CL.MZE.WSR.tif` | 200 | 0.4 MB |

The table below gives the paths the builder would produce once the crop‑lookup error is
removed, next to the paths the bucket holds:

| Package path | HTTP | Bucket path | HTTP |
|---|---|---|---|
| `MAP/AEZ57/GAEZ-V5.AEZ57.tif` | 404 | `MAP/GAEZ-V5.AEZ57.tif` (no sub‑folder) | 200 |
| `MAP/SOILFER.SLOPE-MED/GAEZ-V5.SOILFER.SLOPE-MED.tif` | 404 | `MAP/GAEZ-V5.SLOPE-MED.tif` | 200 |
| `MAP/LR-LCC/GAEZ-V5.LR-LCC.tif` | 404 | `MAPSET/LR-LCC/GAEZ-V5.LR-LCC.LC01.tif` (classes LC01–LC12) | 200 |
| `MAPSET/RES01-PRC/GAEZ-V5.RES01-PRC.FP4160.ENSEMBLE.SSP370.tif` | 404 | `ENSEMBLE` count = 0 for `RES01-PRC` and `RES02-YLD` (FP4160); 456 files for `RES05-YXX` | — |

Further findings from the bucket listing:
- `RES05-SXX30AS` (1 km) has 0 files for FP4160 and 282 for HP0120. Whether a 1 km layer
  exists depends on the period, so it has to be checked per request rather than assumed.
- The `resolution` argument of `build_gaez_url()` is never used. Ambiguous codes such as
  `RES05-YC`, `RES05-SI` and `RES05-SX` resolve to the first table row (10 km). A user cannot
  request the 1 km layer through `resolution = "1km"`.
- `MAP/` holds 9 rasters: `AEZ33-1KM`, `AEZ33`, `AEZ57`, `ELEV-ALOS-30M`, `EXCL`, `HWSDV201`,
  `HWSDV22`, `LR-IRR`, `SLOPE-MED`. `HWSDV22` is absent from the package. `LR-LCC` and `LR-SLC`
  are MAPSETs, not MAPs.
- The package labels `HRLM` as "High input, Rain-fed, Low management". FAO's `INPUT` code list
  defines it as "available water content of 200 mm/m (rain-fed) and high input level of
  management".

### 3.2 Catalog drift (package tables vs FAO `DIMENSION/*.csv`)

| Theme (code list) | Package | FAO | FAO codes missing in package | Package codes absent from FAO list |
|---|---|---|---|---|
| 3 (`CROP-RES02`, updated 2026‑01‑21) | 105 | 115 | FONIO, MANG, TANN, TARODL, TAROWL, HBRL, HWHE, SRAP, TBRL, TWHE, WRAP, AVOC, AVOCST, AVOCTH, AVOCTL | FONI, MANGO, CYAM1, CYAM2, CYAM3 |
| 4 (`CROP-RES05`, updated 2026‑01‑07) | 65 | 78 | FIML, FONIO, OKRA, PST, TANN, TAROW, TAROD, TEF, WMEL, AVOC, AVOST, AVOTH, AVOTL | — |
| 5 (`CROP-RES06`) | 38 | 38 | — | — |
| 6 (`CROP-RES07`) | 24 | 24 | — | — |

FAO added crops (avocado on 2026‑01‑07) and changed land‑class codes (`SLC`, 2026‑10‑16) after
the package tables were frozen. Any hard‑coded table goes stale on the next upstream release.

### 3.3 Transfer behaviour

Tests ran from a cloud container with roughly 30 MB/s throughput to GCS. Byte counts do not
depend on the connection; durations do.

| Test | Result |
|---|---|
| `download_gaez_dataset()`, one 10 km file | success, 3.63 MB, 2.8 s |
| `batch_download_gaez_datasets()`, 6 crops | parallel 2.2 s; sequential 10.5 s (4.8×) |
| `curl::multi_download()` on a non‑existent file (curl 5.2.0) | `success = TRUE`, `status_code = 404`, 270‑byte XML stub saved as `.tif`. The package reads only `success` (`R/download.R:904`), so the batch reports success and the next run skips the stub as "already exists". |
| `download.file()` (`R/download.R:301`) | Honours `options(timeout)`, which defaults to 60 s. Large objects such as `LR-LCC.LC03` (896 MB) need more than 15 MB/s to finish within that limit. The error handler assigns `result$message` inside a closure (`:327`), so the message never reaches the returned object. |
| Default directory | Writes to `Data/GAEZ/` under the working directory. It also searches a project‑specific `Data/GAEZ_Niger_Analysis/` (`:229`). |

### 3.4 Cloud‑Optimized GeoTIFF reads (terra / GDAL `/vsicurl/`)

All files inspected are COGs: `LAYOUT=COG`, LZW compression, 256×256 tiles (512×512 for `AEZ57`),
and 5–8 internal overview levels.

| Test (1 km layer, `RES05-YCX30AS`, HP0120) | Bytes transferred | Time |
|---|---|---|
| Full download, alfalfa (210 MB) / maize (287 MB) | 210 / 287 MB | 6.9 s (alfalfa) |
| Niger window, alfalfa, `crop(rast("/vsicurl/…"))` | 0.17 MB | 3.9 s; values identical to the local crop |
| Nigeria window, maize, 94.9 % valid cells | 5.07 MB | — |
| Global 900×450 preview, GDAL overview selection | 0.34 MB | 3.0 s |
| Global preview, `terra::rast(url, opts = "OVERVIEW_LEVEL=4")` (1350×675) | — | 2.8 s |
| Global preview, `terra::project()` to a 900×450 template | — | 59.8 s |
| Global preview, `terra::plot(maxcell = 5e5)` / `spatSample(regular)` | — | 58 s / 86 s |

At 50 Mbit/s (6.25 MB/s), the maize subset takes 0.8 s of transfer against 46 s for the full
file. Remote previews need an explicit overview level: terra's generic resampling functions
(`project()`, `plot()`, `spatSample()`) took 58–86 s on the same file.

### 3.5 Catalog listing cost

The public GCS JSON API lists objects without credentials. With `matchGlob=**.tif`:
- `RES05-YXX`: 11,507 rasters, 12 pages, 6.6–12.2 s.
- `RES02-YLD`: 14,276 rasters, 15 pages, 8.1–14.6 s.
- Without the glob filter the page count doubles, because each `.tif` has a `.json` sidecar.

The bucket holds 123 MAPSET folders. The User Guide gives roughly 225,000 raster layers in total.

### 3.6 ArcGIS ImageServer path

Live calls were not possible. The environment's network policy denies `gaez-services.fao.org`,
and also `data.apps.fao.org` and `gaez.fao.org`. The following evidence does not depend on
those calls:

- `map_climate_model_to_imageserver()` returns `"CRUTS32"` for every historical period
  (`R/rest_api.R:512`). CRU TS 3.2 is the GAEZ v4 baseline; v5 uses AgERA5.
- `map_ssp_to_rcp()` maps SSP126 → RCP2.6 and SSP370/SSP585 → RCP8.5 (`R/rest_api.R:555`).
  GAEZ v5 does not use RCP scenarios.
- `map_time_period_to_year()` maps HP8100 and HP0120 to `1981-2010`, and future periods to
  `2011-2040`, `2041-2070` and `2071-2100`. These are the GAEZ v4 30‑year periods; v5 uses
  20‑year periods.
- The ArcGIS item‑info records for `res01`, `res02`, `res05`, `res07` and `LR` on
  `gaez-services.fao.org`, as indexed by web search on 2026‑11‑04, describe them as GAEZ v4
  services.
- `pixelType = "S16"` (`R/rest_api.R:217`) truncates the Float32 yield and index values to
  16‑bit integers.
- Independent projects that query `res05/ImageServer/query` in 2026 filter on
  `year = '1981-2010'` and `model = 'CRUTS32'`. The `download_url` values they receive point to
  GAEZ v4 files (`data.gaezdev.aws.fao.org/res05/CRUTS32/Hist/8110H/...`). Examples:
  [MurrayLane/carrying-capacity-dashboard](https://github.com/MurrayLane/carrying-capacity-dashboard/blob/HEAD/analysis/gaez_2025/README.md),
  [omrigalor/long-run-development-dynamics-simulator](https://github.com/omrigalor/long-run-development-dynamics-simulator/blob/HEAD/Tools/download_gaez.py),
  [onaxyzogden/atlas](https://github.com/onaxyzogden/atlas/blob/HEAD/apps/api/scripts/download-gaez.ts).
- The v5 counterpart is the FAO GISMGR WMTS (`data.apps.fao.org/map/wmts/wmts`). FAO's
  [SoilFER app](https://github.com/un-fao/SoilFER/blob/HEAD/crop-suitability-app/src/components/layout/MapView.tsx)
  selects one raster of a mapset with `dim_<DIMENSION>=<member>` parameters; `gaez_wmts_url()`
  (0.2.1) builds such templates from the metadata file next to each GeoTIFF.

Conclusion: when the REST call succeeds, `preview_gaez_map()` returns GAEZ v4 data. The
defect is in the data source, not in the parameter mapping, so no change to the mapping fixes it.

## 4. How FAO documents GAEZ v5 access (November 2026)

| Channel | Content | Status for R users |
|---|---|---|
| [GAEZ v5 Platform](https://data.apps.fao.org/gaez/?lang=en) (FAO GISMGR 2.0) | Browse, visualise, download; ISO 19115 metadata; "cloud‑native formats via APIs" | Interactive |
| [Platform User Guide (wiki §21)](https://github.com/un-fao/gaezv5/wiki/21.-GAEZ-v5-Platform-User-Guide) | Download from bucket `fao-gismgr-gaez-v5-data`; naming conventions per theme; per‑theme README `.xlsx` lists file names; `gsutil -m cp` for bulk | Same bucket and naming as the package |
| [Data Access Notebooks](https://github.com/un-fao/gaezv5/tree/main/GAEZ-v5-Platform/Data-Access-Notebooks) (Python, RES02 and RES05) | Fetch `_readme_res05.xlsx` from the FAO data catalog (CKAN), sheet `LIST_FILES` (`MAPSET_CODE`, `FILE_NAME`); URL = `…/MAPSET/<MAPSET_CODE>/<FILE_NAME>.tif`; optional HEAD check; streamed download | Catalog‑driven; the package uses fixed rules |
| FAO GISMGR 2.0 API (`https://data.apps.fao.org/gismgr/api/v2`) | Public, no token. `catalog/workspaces/GAEZ-V5/{mapsets,maps}`, `…/mapsets/<code>/rasters`, `…/maps/<code>/raster`; each raster item carries `downloadUrl`, scale, offset, unit and noData. The [FAO Downloader QGIS plugin](https://github.com/bvissers/FAO-Downloader) v2.1 uses it for GAEZ and reads COG subsets via `/vsicurl/` | Not used by the package; untested here (host denied) |
| [FAO data catalog](https://data.apps.fao.org/catalog/dataset/4925a45e-df71-4d66-b05c-063d94e1644d) entries | GCS download links; WMTS layers `fao-gismgr/GAEZ-V5/mapsets/<CODE>` at `data.apps.fao.org/map/wmts/wmts`; SLD styles from the GISMGR API; licence CC BY 4.0 | WMTS is a v5‑native tile source for previews |
| Bucket self‑description | `GAEZ-V5.json`; `DIMENSION/*.csv` (16 code lists); one `.json` sidecar per raster with `dimensionMembers` (MAP sidecars add `downloadUrl`, `gsutilUri`, scale, offset, noData, classes); `STYLE/*.sld` | Machine‑readable; reachable wherever GCS is reachable |

## 5. Alignment assessment

| Dimension | Package | FAO practice | Gap |
|---|---|---|---|
| Host and naming (themes 3–6) | GCS HTTPS, `GAEZ-V5.<parts>.tif` | Same | None |
| Host and naming (themes 1–2, TS) | Error before request; wrong `MAP/` paths; wrong codes | Flat `MAP/`, `LR-LCC`/`LR-SLC` as MAPSET | Broken |
| Code lists | Frozen tables in `sysdata.rda` | Live catalog (README `.xlsx`, `DIMENSION/*.csv`, API) | 28 crop codes missing, 5 obsolete |
| Availability checks | Rule‑based (`validate_climate_ssp()`); ENSEMBLE assumed for all themes | Files listed per combination | 404 for themes 2–3 future defaults |
| Transfer | Parallel via curl; no status, size or md5 check; partial files possible | Streamed download with status check (notebooks) | False success; cache poisoning |
| Regional extraction | Download global file, crop locally | COG + `/vsicurl/` (FAO Downloader) | Up to 57× more bytes than needed |
| Preview | ArcGIS ImageServer (GAEZ v4) | GISMGR platform, WMTS, COG overviews | Wrong dataset version |
| Resilience to URL changes | One hard‑coded root per path; no fallback | API returns `downloadUrl`; bucket self‑describing | No discovery or fallback |
| Storage location | Working directory | — | Conflicts with the CRAN policy on writing outside `tempdir()` without user consent |

## 6. Revision plan

### 6.1 Design rules

1. **Discover, do not encode.** File existence and code lists come from FAO's published catalog
   at run time and are cached. A shipped snapshot serves only as an offline fallback.
2. **One root, overridable.** Every host lives in one internal function. An option or
   environment variable overrides it.
3. **Read only what is needed.** Requests for a region or a preview read COG windows and
   overviews remotely. Global files are downloaded only on explicit request.
4. **Fail atomically.** No file reaches its final `.tif` name unless the transfer returned HTTP
   200 and its size and MD5 match the catalog.
5. **Detect drift before users do.** A scheduled canary tests every theme and diffs FAO's code
   lists.

### 6.2 Target architecture

```
gaez_find(variable, crop, period, climate_model, ssp, input, water_supply, year, class)
   │  validates against ↓ and returns tibble(mapset, file, url, size, md5, <dimensions>)
   ├── gaez_codes(dimension)   DIMENSION/<code>.csv  → cache (fallback: sysdata snapshot)
   ├── gaez_catalog(mapset)    provider chain, first success wins:
   │        1. "gismgr"        GISMGR 2.0 API, returns downloadUrl            (WP6)
   │        2. "gcs_listing"   GCS JSON API listing, matchGlob=**.tif          (WP1)
   │        3. "template"      naming convention + gaez_root()                (WP2)
   │                          → cache in tools::R_user_dir("gaezv5", "cache"), TTL 30 days
   ├── gaez_download(files)    curl::multi_download → .part → status/size/md5 → rename
   ├── gaez_rast(files, region) /vsicurl/ COG windows; local cache used when present
   └── preview_gaez_map(...)   COG overview level chosen from requested width → leaflet
```

### 6.3 Work packages

**WP0: Hotfix release 0.2.1** (no API changes; fixes verified defects)

| Task | Files | Acceptance test |
|---|---|---|
| Skip crop lookup for themes 1, 2 and TS | `R/url_builder.R:127`, `R/validation.R` | `01_url_checks.R`: T1, T2 and TS cases return URLs |
| Theme‑1 path `MAP/GAEZ-V5.<code>.tif`; codes `ELEV-ALOS-30M`, `SLOPE-MED`; add `HWSDV22`; move `LR-LCC`/`LR-SLC` to MAPSET with a `class` argument (LC01–LC12, SC01–SC10) | `R/url_builder.R:221`, `data-raw/prepare_data.R` | HEAD 200 for every MAP raster and one class per land MAPSET |
| Themes 2–3 future periods: stop with the list of the five available GCMs instead of defaulting to `ENSEMBLE` | `R/validation.R:70` | Message names GFDL‑ESM4, IPSL‑CM6A‑LR, MPI‑ESM1‑2‑HR, MRI‑ESM2‑0, UKESM1‑0‑LL |
| Batch: success means `status_code == 200`; delete non‑200 files; download to `<dest>.part` and rename | `R/download.R:871–910` | `02_transfer_bench.R`: 404 gives `success = FALSE` and no file on disk |
| Single download: replace `download.file()` with `curl::curl_download()` (no 60 s cap) plus `.part`/rename; return errors through the handler's value | `R/download.R:268–330` | 896 MB `LR-LCC.LC03` completes with the default options |
| Remove the `Data/GAEZ_Niger_Analysis` lookup | `R/download.R:229` | — |
| Implement `resolution = "1km"` as a mapping to `*30AS` mapsets, with an error when no 1 km layer exists for the requested period | `R/url_builder.R` | `RES05-SX`, `resolution = "1km"`, HP0120 → `RES05-SXX30AS` |
| Preview: remove the ImageServer branch; read the COG overview (global) or window (country/bbox) from GCS | `R/preview.R:300–420`, `R/rest_api.R` | Global 1 km preview ≤ 5 s, ≤ 1 MB |
| Regenerate crop tables from `DIMENSION/CROP-RES0x.csv`; correct `INPUT` labels | `data-raw/prepare_data.R` | `05_catalog_drift.R` reports no differences |
| Live canary tests with `skip_on_cran()` and `skip_if_offline()` | `tests/testthat/test-live.R` | One HEAD per theme passes |

**WP1: Catalog layer** (0.3.0)

- `gaez_codes(dimension)` reads `DIMENSION/GAEZ-V5.<dimension>.csv` and caches it. The
  `data-raw` script builds the shipped `sysdata` snapshot from the same files, so offline and
  online code lists share one schema.
- `gaez_mapsets()` lists the `MAP/` rasters and `MAPSET/` prefixes and joins theme, sub‑theme
  and caption from a curated alias table. The table maps package codes to FAO codes
  (`RES05-YX` → `RES05-YXX`) and also accepts FAO codes directly.
- `gaez_catalog(mapset)` lists `.tif` objects (GCS JSON API, `matchGlob=**.tif`, 1000 per page)
  and parses file names into dimension columns. Dimension order comes from one sidecar's
  `dimensionMembers`, which absorbs new or reordered dimensions without code changes. It
  returns `tibble(mapset, file, url, size, md5, updated, period, clim, ssp, crop, input, …)`.
- Cache: `tools::R_user_dir("gaezv5", "cache")`, overridable through
  `options(gaezv5.cache_dir)` or `GAEZV5_CACHE_DIR`. TTL comes from
  `options(gaezv5.catalog_ttl = 30)` (days). `gaez_catalog_refresh()` forces a refresh.
- Cost: 7–15 s on the first call for the largest mapsets, under 1 s for small ones, zero while
  cached. The package does not ship the full list of roughly 225,000 files.

**WP2: Resolver** (0.3.0)

- `gaez_find()` is vectorised; `NULL` means all values, and arguments accept names (`"maize"`)
  or codes. Unknown or unavailable values produce an error that lists the available values,
  given the other filters (e.g. "`ENSEMBLE` is not available for `RES02-YLD`; available:
  GFDL‑ESM4, …").
- Provider chain: `options(gaezv5.providers = c("gcs_listing", "template"))`. Every provider
  returns the same tibble schema, so adding `"gismgr"` (WP6) changes no caller.
- Single root: `gaez_root()` returns `getOption("gaezv5.gcs_root", Sys.getenv("GAEZV5_GCS_ROOT", <default>))`.
- Self‑healing: a 404 on a cached URL triggers one listing refresh for that mapset. A failure at
  bucket level moves to the next provider. The final error names the root in use and how to
  override it.
- `build_gaez_url()` remains as a wrapper that returns the URL of exactly one `gaez_find()` row.
  `validate_climate_ssp()` becomes a thin check against the catalog.

**WP3: Transfer** (0.3.0)

- `gaez_download(files, dir = gaez_cache_dir(), overwrite = FALSE)` runs
  `curl::multi_download(multiplex = TRUE)` into `.part` files. It requires status 200, checks
  the size against the catalog and the MD5 against the GCS `md5Hash` (base64 → hex, compared
  with `tools::md5sum()`), then renames. It retries 429/5xx with exponential backoff (1, 2, 4,
  8 s).
- An existing file is skipped only when its size (or MD5 under `verify = "md5"`) matches the
  catalog.
- Returns a tibble (`file`, `path`, `status`, `bytes`, `seconds`, `verified`).
  `download_gaez_dataset()` and `batch_download_gaez_datasets()` become wrappers that keep
  their current list output until 0.4.0.

**WP4: Remote reads** (0.3.1)

- `gaez_rast(x, region = NULL, cache = FALSE)` accepts a `gaez_find()` result or URLs. It opens
  `/vsicurl/` sources and crops to a country (name/ISO3), a `SpatVector` or a bbox. With
  `cache = TRUE` it writes the cropped raster to the cache directory.
- GDAL settings are applied through a scoped helper that restores the previous values on exit:
  `GDAL_DISABLE_READDIR_ON_OPEN=EMPTY_DIR`, `CPL_VSIL_CURL_ALLOWED_EXTENSIONS=.tif`,
  `GDAL_HTTP_MULTIRANGE=YES`, `GDAL_HTTP_MERGE_CONSECUTIVE_RANGES=YES`, `GDAL_HTTP_VERSION=2`,
  `GDAL_HTTP_MAX_RETRY=4`, `GDAL_HTTP_RETRY_DELAY=2`, `VSI_CACHE=TRUE`.
- `load_gaez_data(country = …)` reads the window remotely when no global file is cached.
  Multi‑crop or multi‑period requests open one stack (`rast(c(urls))`) and crop it once.
- Acceptance: Nigeria 1 km maize window ≤ 6 MB transferred (measured 5.07 MB); results equal a
  local crop of the full file.

**WP5: Preview engine** (0.3.1)

- Overview selection: choose the smallest level whose width is at least the requested width
  (overview `k` has width `ceiling(W / 2^(k+1))`), open it with
  `rast(url, opts = "OVERVIEW_LEVEL=k")`, then crop to the region.
- Delete `R/rest_api.R` and its v4 mappings. Remove `raster` from Suggests: `leaflet` ≥ 2.1.2
  accepts `SpatRaster` in `addRasterImage()`.
- Legends and class colours come from `STYLE/GAEZ-V5.<style>.json` (styleCode in the sidecar),
  so class maps (AEZ, suitability classes) match the FAO platform.
- Acceptance: global preview of a 1 km layer ≤ 5 s and ≤ 1 MB (measured 2.8 s, 0.34 MB).

**WP6: GISMGR API provider and WMTS** (0.4.0; needs network access to `data.apps.fao.org`)

- Provider `"gismgr"`: `GET …/catalog/workspaces/GAEZ-V5/mapsets/<code>/rasters` (paged via
  `links[rel = next]`) and `…/maps/<code>/raster`. It maps `downloadUrl`, `scale`, `offset`,
  `measureUnit` and `noDataValue` into the catalog schema. If FAO relocates storage, the API's
  `downloadUrl` values follow without a package release.
- Captions and units from the API populate `gaez_mapsets()`.
- Optional preview mode: leaflet tiles from the FAO WMTS (`fao-gismgr/GAEZ-V5/mapsets/<CODE>`),
  which needs no raster transfer. The dimension parameters for selecting one raster in a mapset
  still have to be documented from live responses.
- Acceptance: recorded API responses stored as test fixtures; the provider passes the WP2
  resolver tests unchanged.

**WP7: Tests, monitoring, documentation** (alongside WP1–WP6)

- Offline unit tests use stored fixtures (one listing page, one sidecar, one `DIMENSION` CSV), so
  no test needs network access on CRAN.
- Live canaries (`skip_on_cran`, `skip_if_offline`): one HEAD per theme (1, 2, TS, 3, 4, 5, 6),
  one mapset listing, one single‑pixel `/vsicurl/` read.
- `.github/workflows/gaez-canary.yaml`, weekly: runs the canaries, diffs `DIMENSION/*.csv` and
  the mapset prefix list against the snapshot, and opens an issue on any difference.
- Vignette "Data access": `gaez_find()` → `gaez_rast()` / `gaez_download()`; GDAL and proxy
  settings (`HTTPS_PROXY`, `GDAL_HTTP_PROXY`); citation "FAO & IIASA (2025), GAEZ v5, CC BY 4.0,
  accessed <date>".
- Progress output moves from `cat()` to `message()`, so `suppressMessages()` works.

### 6.4 Dependency changes

| Change | Reason |
|---|---|
| `jsonlite`: Suggests → Imports | Catalog and sidecar parsing |
| Remove `raster` | `leaflet` ≥ 2.1.2 handles `SpatRaster` |
| Remove `httr` (superseded) after WP3 | `curl` covers HEAD, listing and multi‑download |
| `leaflet` (and optionally `geodata`): Imports → Suggests | Only the preview and country boundaries need them; installation becomes lighter |

### 6.5 Release sequence

| Release | Content | Breaking changes |
|---|---|---|
| 0.2.1 | WP0 | None (defaults for themes 2–3 future change from a 404 to an informative error) |
| 0.3.0 | WP1, WP2, WP3, WP7 (tests) | New functions; old functions become wrappers; default directory moves to `R_user_dir()` with a one‑time message |
| 0.3.1 | WP4, WP5 | `preview_gaez_map()` engine change, same arguments |
| 0.4.0 | WP6, WP7 (canary workflow, vignette) | Download wrappers return tibbles |

### 6.6 Risks and mitigations

| Risk | Mitigation |
|---|---|
| FAO disables public bucket listing | Provider chain falls through to `gismgr`, then `template`; the canary reports within a week |
| Bucket renamed or moved | `gaez_root()` override; `gismgr` returns the new `downloadUrl` values |
| File‑name convention changes | Dimension order read from sidecars; canary diff |
| Rate limiting (HTTP 429) | Backoff in `gaez_download()` and GDAL retries; cap concurrent connections per host |
| Corporate proxies | curl and GDAL honour `HTTPS_PROXY`; vignette documents `GDAL_HTTP_PROXY` |
| CRAN checks | No network calls in non‑skipped tests; cache only under `R_user_dir()` |

### 6.7 Decisions for the maintainer

1. **Themes 2–3, future periods.** FAO publishes no `ENSEMBLE` layer for these themes. The
   default can stop with an error (proposed), select one GCM, or return all five GCMs as a
   stack.
2. **Breaking changes.** Should the return‑type changes in 0.3.0/0.4.0 (tibbles instead of
   lists) proceed, or should list output stay indefinitely?
3. **Network access for WP6.** Should `data.apps.fao.org` and `gaez-services.fao.org` be added
   to the development environment's allowed domains? That would allow capturing GISMGR API
   fixtures and confirming the v4 status of the ImageServer live.
4. **Dependencies.** Should `leaflet` and `geodata` move to Suggests?

## 7. Sources

- FAO & IIASA (2025). GAEZ v5 Platform User Guide. https://github.com/un-fao/gaezv5/wiki/21.-GAEZ-v5-Platform-User-Guide
- FAO (2026). GAEZ v5 Data Access Notebooks (repository commit 6c712bf, 2026‑09‑29). https://github.com/un-fao/gaezv5/tree/main/GAEZ-v5-Platform/Data-Access-Notebooks
- FAO. GAEZ v5 Platform. https://data.apps.fao.org/gaez/?lang=en
- FAO data catalog, GAEZ v5 dataset entries (e.g. mean annual precipitation). https://data.apps.fao.org/catalog/dataset/4925a45e-df71-4d66-b05c-063d94e1644d
- FAO. GISMGR 2.0 API Reference. https://openknowledge.fao.org/server/api/core/bitstreams/fdc8f1cc-f8fe-4af0-86c0-7d44d947d131/content
- Vissers, B. FAO Downloader QGIS plugin v2.1 (GISMGR 2.0 client with `/vsicurl/` subsetting). https://github.com/bvissers/FAO-Downloader
- ArcGIS item info, `gaez-services.fao.org` `res05` service (GAEZ v4). https://gaez-services.fao.org/server/rest/services/res05/ImageServer/info/iteminfo?f=pjson
- Public bucket: `gs://fao-gismgr-gaez-v5-data/DATA/GAEZ-V5/` (listed via the GCS JSON API, 2026‑11‑04)

## 8. Reproducing the tests

Run from the package root. Requirements: R ≥ 4.1 with `dplyr`, `stringr`, `tibble`, `tidyr`,
`purrr`, `httr`, `curl`, `jsonlite` and `terra`, plus the GDAL command‑line tools for script 04.

```bash
Rscript dev/data-access-review/01_url_checks.R      # URL construction + HEAD checks (sections 3.1)
Rscript dev/data-access-review/02_transfer_bench.R  # download functions, 404 handling (3.3)
Rscript dev/data-access-review/03_cog_reads.R       # remote window and overview reads (3.4)
bash    dev/data-access-review/04_cog_bytes.sh      # bytes transferred per read (3.4)
Rscript dev/data-access-review/05_catalog_drift.R   # code-list drift, listing cost (3.2, 3.5)
```

Output files go to `$GAEZV5_REVIEW_OUT` (default: a folder in `tempdir()`).
