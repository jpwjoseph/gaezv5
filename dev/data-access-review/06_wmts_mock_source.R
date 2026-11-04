# Writes the position-encoded RGBA source image used by 06_wmts_mock_server.py.
# Usage: Rscript dev/data-access-review/06_wmts_mock_source.R <out.tif>
suppressPackageStartupMessages(library(terra))
out <- commandArgs(trailingOnly = TRUE)[1]
r <- rast(ext(-180, 180, -90, 90), nrows = 1800, ncols = 3600, nlyrs = 4, crs = "EPSG:4326")
xy <- xyFromCell(r, seq_len(ncell(r)))
values(r) <- cbind(round((xy[, 1] + 180) / 360 * 255), round((xy[, 2] + 90) / 180 * 255), 128, 255)
tmp <- tempfile(fileext = ".tif")
writeRaster(r, tmp, datatype = "INT1U")
system2("gdal_translate", c("-q", "-a_nodata", "none", tmp, out))
