# Benchmark the package's download functions and check 404 handling.
source("dev/data-access-review/harness.R")
dl <- file.path(out_dir, "dl"); unlink(dl, recursive = TRUE); dir.create(dl)
tm <- function(expr) { t0 <- Sys.time(); v <- force(expr); list(v = v, s = as.numeric(Sys.time() - t0, units = "secs")) }

cat("## download_gaez_dataset(): one 10 km file\n")
r1 <- tm(suppressMessages(download_gaez_dataset(crop = "maize", time_period = "HP0120", ssp = "HIST",
                                                download_dir = dl, verbose = FALSE, interactive = FALSE)))
cat("success:", r1$v$success, " MB:", round(r1$v$file_size / 1e6, 2), " secs:", round(r1$s, 2), "\n")

cat("\n## batch_download_gaez_datasets(): 6 crops, parallel vs sequential\n")
crops <- c("maize", "wheat", "sorghum", "soybean", "cassava", "barley")
r2 <- tm(suppressMessages(batch_download_gaez_datasets(crops = crops, time_periods = "HP0120", parallel = TRUE,
                                                       download_dir = file.path(dl, "par"), verbose = FALSE)))
cat("parallel ok:", sum(sapply(r2$v, `[[`, "success")), "/", length(r2$v), " secs:", round(r2$s, 2), "\n")
r3 <- tm(suppressMessages(capture.output(batch_download_gaez_datasets(crops = crops, time_periods = "HP0120",
                                         parallel = FALSE, download_dir = file.path(dl, "seq"),
                                         verbose = FALSE, interactive = FALSE))))
cat("sequential secs:", round(r3$s, 2), "\n")

cat("\n## curl::multi_download() on a 404 (as used by the parallel batch path)\n")
bad <- file.path(GCS_ROOT, "MAPSET/RES05-YXX/GAEZ-V5.RES05-YXX.FP4160.ENSEMBLE.SSP370.NOPE.HRLM.tif")
dest <- file.path(dl, basename(bad))
md <- curl::multi_download(bad, dest, resume = TRUE, progress = FALSE)
print(md[, c("success", "status_code", "error")])
cat("curl version:", as.character(packageVersion("curl")),
    " stub left on disk:", file.exists(dest), " bytes:", file.size(dest), "\n")
