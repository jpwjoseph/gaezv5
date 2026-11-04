# Load gaezv5 source code without installing the package.
# Run every script in this folder from the package root, e.g.
#   Rscript dev/data-access-review/01_url_checks.R
# Requires: dplyr, stringr, tibble, tidyr, purrr, httr, curl, jsonlite, terra.
# leaflet and geodata are not needed for the download path.
suppressPackageStartupMessages({
  library(dplyr); library(stringr); library(tibble); library(tidyr)
  library(purrr); library(httr); library(curl)
})
pkg_root <- normalizePath(Sys.getenv("GAEZV5_ROOT", "."))
if (!file.exists(file.path(pkg_root, "DESCRIPTION"))) {
  stop("Run from the gaezv5 package root or set GAEZV5_ROOT.", call. = FALSE)
}
gaez <- new.env()
load(file.path(pkg_root, "R", "sysdata.rda"), envir = gaez)
for (f in list.files(file.path(pkg_root, "R"), pattern = "\\.R$", full.names = TRUE)) {
  sys.source(f, envir = gaez)
}
options(gaez_testing_mode = TRUE)
attach(gaez, name = "gaezv5_src", warn.conflicts = FALSE)

GCS_ROOT <- "https://storage.googleapis.com/fao-gismgr-gaez-v5-data/DATA/GAEZ-V5"
GCS_API  <- "https://storage.googleapis.com/storage/v1/b/fao-gismgr-gaez-v5-data/o"
out_dir  <- Sys.getenv("GAEZV5_REVIEW_OUT", file.path(tempdir(), "gaezv5-review"))
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

# List .tif objects under a bucket prefix (public GCS JSON API, paginated).
gcs_list <- function(prefix, glob = "**.tif") {
  tok <- NULL; items <- list(); pages <- 0
  repeat {
    q <- list(prefix = prefix, fields = "items(name,size,md5Hash,updated),nextPageToken",
              maxResults = 1000, matchGlob = glob, pageToken = tok)
    q <- q[!vapply(q, is.null, logical(1))]
    url <- paste0(GCS_API, "?", paste(names(q), vapply(q, curl::curl_escape, ""), sep = "=", collapse = "&"))
    d <- jsonlite::fromJSON(rawToChar(curl::curl_fetch_memory(url)$content))
    pages <- pages + 1
    if (length(d$items)) items[[pages]] <- d$items
    tok <- d$nextPageToken
    if (is.null(tok)) break
  }
  res <- dplyr::bind_rows(items)
  attr(res, "pages") <- pages
  res
}
