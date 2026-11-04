# Compare the package's hard-coded code tables with FAO's DIMENSION code lists,
# and time a full listing of the largest mapsets.
source("dev/data-access-review/harness.R")
dims <- gcs_list("DATA/GAEZ-V5/DIMENSION/", glob = "**.csv")
cat("DIMENSION code lists:\n"); print(as.data.frame(dims[, c("name", "updated")]))
rd <- function(code) utils::read.csv(file.path(GCS_ROOT, "DIMENSION", paste0("GAEZ-V5.", code, ".csv")),
                                     stringsAsFactors = FALSE)
map <- c(`3` = "CROP-RES02", `4` = "CROP-RES05", `5` = "CROP-RES06", `6` = "CROP-RES07")
for (th in names(map)) {
  pk  <- unique(gaez_crops$gaez_crop_code[gaez_crops$gaez_theme == as.integer(th)])
  fao <- rd(map[[th]])$code
  cat(sprintf("\nTheme %s (%s): package=%d FAO=%d\n  missing in package: %s\n  not in FAO list:    %s\n",
              th, map[[th]], length(pk), length(fao),
              paste(setdiff(fao, pk), collapse = ", "), paste(setdiff(pk, fao), collapse = ", ")))
}
cat("\nMapset folders:", nrow(jsonlite::fromJSON(rawToChar(curl::curl_fetch_memory(
  paste0(GCS_API, "?prefix=DATA/GAEZ-V5/MAPSET/&delimiter=/&fields=prefixes"))$content))$prefixes |> as.data.frame()), "\n")
for (m in c("RES05-YXX", "RES02-YLD")) {
  t0 <- Sys.time(); x <- gcs_list(paste0("DATA/GAEZ-V5/MAPSET/", m, "/"))
  cat(sprintf("%s: %d tif in %d pages, %.1f s\n", m, nrow(x), attr(x, "pages"),
              as.numeric(Sys.time() - t0, units = "secs")))
}
