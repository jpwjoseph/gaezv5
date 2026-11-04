# Build package URLs across all themes and HEAD-check them against GCS.
source("dev/data-access-review/harness.R")
q <- function(expr) suppressMessages(tryCatch(expr, error = function(e) paste("ERROR:", conditionMessage(e))))
cases <- list(
  list("T1 AEZ57",               list(variable = "AEZ57")),
  list("T1 LR-IRR",              list(variable = "LR-IRR")),
  list("T1 HWSDV201",            list(variable = "HWSDV201")),
  list("T1 SOILFER.SLOPE-MED",   list(variable = "SOILFER.SLOPE-MED")),
  list("T2 RES01-PRC hist",      list(variable = "RES01-PRC", time_period = "HP0120", ssp = "HIST")),
  list("T2 RES01-PRC fut",       list(variable = "RES01-PRC", time_period = "FP4160", ssp = "SSP370")),
  list("T2-TS RES01-PRC-TS",     list(variable = "RES01-PRC-TS", time_period = "2010")),
  list("T3 RES02-YLD maize",     list(variable = "RES02-YLD", crop = "maize", time_period = "HP0120", ssp = "HIST")),
  list("T4 RES05-YX default",    list()),
  list("T4 RES05-YX maize hist", list(crop = "maize", time_period = "HP0120", ssp = "HIST")),
  list("T4 RES05-SI",            list(variable = "RES05-SI", crop = "maize")),
  list("T4 RES05-YC",            list(variable = "RES05-YC", crop = "sorghum")),
  list("T5 RES06-YLD",           list(variable = "RES06-YLD", crop = "maize")),
  list("T5 RES06-HAR WST",       list(variable = "RES06-HAR", crop = "rice", water_supply = "WST")),
  list("T6 RES07-YGA-CL",        list(variable = "RES07-YGA-CL", crop = "maize"))
)
res <- lapply(cases, function(cs) {
  url <- q(do.call(build_gaez_url, c(cs[[2]], list(interactive = FALSE))))
  st <- NA_integer_; mb <- NA_real_
  if (startsWith(url, "http")) {
    r <- tryCatch(httr::HEAD(url, httr::timeout(30)), error = function(e) NULL)
    if (!is.null(r)) {
      st <- httr::status_code(r)
      if (st == 200) mb <- round(as.numeric(httr::headers(r)$`content-length`) / 1e6, 1)
    }
  }
  tibble(case = cs[[1]], http = st, MB = mb, url = sub(paste0(GCS_ROOT, "/"), "", url))
})
print(as.data.frame(bind_rows(res)), right = FALSE)

cat("\n## Path variants for themes 1-2 (what the bucket actually holds)\n")
paths <- c("MAP/AEZ57/GAEZ-V5.AEZ57.tif", "MAP/GAEZ-V5.AEZ57.tif",
           "MAP/SOILFER.SLOPE-MED/GAEZ-V5.SOILFER.SLOPE-MED.tif", "MAP/GAEZ-V5.SLOPE-MED.tif",
           "MAP/LR-LCC/GAEZ-V5.LR-LCC.tif", "MAPSET/LR-LCC/GAEZ-V5.LR-LCC.LC01.tif",
           "MAPSET/RES01-PRC/GAEZ-V5.RES01-PRC.FP4160.ENSEMBLE.SSP370.tif",
           "MAPSET/RES01-PRC-TS/GAEZ-V5.RES01-PRC-TS.2010.tif")
for (p in paths) cat(httr::status_code(httr::HEAD(file.path(GCS_ROOT, p))), " ", p, "\n")

cat("\n## ENSEMBLE / 1 km availability by prefix\n")
for (p in c("RES01-PRC/GAEZ-V5.RES01-PRC.FP4160.ENSEMBLE",
            "RES02-YLD/GAEZ-V5.RES02-YLD.FP4160.ENSEMBLE",
            "RES05-YXX/GAEZ-V5.RES05-YXX.FP4160.ENSEMBLE",
            "RES05-SXX30AS/GAEZ-V5.RES05-SXX30AS.FP4160",
            "RES05-SXX30AS/GAEZ-V5.RES05-SXX30AS.HP0120")) {
  cat(sprintf("%-50s %6d files\n", p, nrow(gcs_list(paste0("DATA/GAEZ-V5/MAPSET/", p)))))
}
