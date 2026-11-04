# Remote Cloud-Optimized GeoTIFF reads with terra/GDAL (/vsicurl/).
source("dev/data-access-review/harness.R")
suppressPackageStartupMessages(library(terra))
gdal_opts <- c(GDAL_DISABLE_READDIR_ON_OPEN = "EMPTY_DIR",
               CPL_VSIL_CURL_ALLOWED_EXTENSIONS = ".tif",
               GDAL_HTTP_MULTIRANGE = "YES",
               GDAL_HTTP_MERGE_CONSECUTIVE_RANGES = "YES")
for (o in names(gdal_opts)) setGDALconfig(o, gdal_opts[[o]])
tm <- function(expr) { t0 <- Sys.time(); v <- force(expr); list(v = v, s = as.numeric(Sys.time() - t0, units = "secs")) }

u1k <- file.path(GCS_ROOT, "MAPSET/RES05-YCX30AS/GAEZ-V5.RES05-YCX30AS.HP0120.AGERA5.HIST.ALF.HRLM.tif")
ner <- ext(0.1, 16.0, 11.7, 23.5)

cat("## 1 km window (Niger) read remotely\n")
r1 <- tm({ y <- crop(rast(paste0("/vsicurl/", u1k)), ner); values(y); y })
cat("dims", dim(r1$v)[1:2], " secs", round(r1$s, 2), "\n")

cat("\n## same window after full download\n")
f1k <- file.path(out_dir, basename(u1k))
r2 <- tm(curl::curl_download(u1k, f1k, quiet = TRUE))
r3 <- tm({ y2 <- crop(rast(f1k), ner); values(y2); y2 })
cat("download MB", round(file.size(f1k) / 1e6, 1), " secs", round(r2$s, 2),
    "; local crop secs", round(r3$s, 2),
    "; identical values:", isTRUE(all.equal(values(r1$v), values(r3$v))), "\n")

cat("\n## global preview from the same 1 km file\n")
r4 <- tm({ x <- rast(paste0("/vsicurl/", u1k), opts = "OVERVIEW_LEVEL=4"); values(x); x })
cat("rast(opts = 'OVERVIEW_LEVEL=4'): dims", dim(r4$v)[1:2], " secs", round(r4$s, 2), "\n")
r5 <- tm({ x <- rast(paste0("/vsicurl/", u1k)); t <- rast(ext(x), nrows = 450, ncols = 900, crs = crs(x)); project(x, t, method = "near") })
cat("project() to a 900x450 template: secs", round(r5$s, 2), "\n")
