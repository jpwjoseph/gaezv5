## Pre-compile vignettes for GitHub rendering
##
## This script pre-renders the package vignettes to GitHub markdown format
## so they display properly on GitHub while keeping the .Rmd source for
## package builds.
##
## Run this script whenever the vignette content changes:
##   source("vignettes/precompile.R")

library(knitr)
library(rmarkdown)

# Get the vignettes directory
vig_dir <- "vignettes"

# List of vignettes to pre-render
vignettes <- c("getting-started.Rmd")

# Pre-render each vignette
for (vig in vignettes) {
  message("\n========================================")
  message("Pre-rendering: ", vig)
  message("========================================\n")

  input_file <- file.path(vig_dir, vig)
  output_file <- file.path(vig_dir, sub("\\.Rmd$", ".md", vig))

  # Render to GitHub-flavored markdown
  rmarkdown::render(
    input = input_file,
    output_format = rmarkdown::github_document(
      html_preview = FALSE,
      toc = TRUE,
      toc_depth = 3
    ),
    output_file = basename(output_file),
    output_dir = vig_dir,
    quiet = FALSE
  )

  message("\n✓ Successfully created: ", output_file)
}

message("\n========================================")
message("Pre-compilation complete!")
message("========================================")
message("\nRendered vignettes:")
for (vig in vignettes) {
  md_file <- file.path(vig_dir, sub("\\.Rmd$", ".md", vig))
  if (file.exists(md_file)) {
    message("  ✓ ", md_file)
  }
}
message("\nRemember to commit the .md files to git!")
