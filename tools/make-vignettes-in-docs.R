#!/usr/bin/env Rscript

# Generate vignettes in docs/
#
# This script renders the HTML and PDF versions of the main huxtable vignette
# from vignettes/huxtable.Rmd and places them in docs/ for backwards compatibility.

# Ensure we're in the package root directory
if (!file.exists("DESCRIPTION")) {
  stop("This script must be run from the package root directory")
}

# Load required packages
suppressPackageStartupMessages({
  library(rmarkdown)
  library(huxtable)
})

# Check if source vignette exists
vignette_source <- "vignettes/huxtable.Rmd"
if (!file.exists(vignette_source)) {
  stop("Source vignette not found: ", vignette_source)
}

# Create docs directory if it doesn't exist
if (!dir.exists("docs")) {
  dir.create("docs", recursive = TRUE)
}

# Set working directory to vignettes for rendering
old_wd <- getwd()
on.exit(setwd(old_wd))

cat("Rendering HTML version of huxtable vignette...\n")
# Render HTML version
html_file <- render(
  vignette_source,
  output_format = "html_document",
  output_file = file.path("..", "docs", "huxtable-html.html"),
  quiet = TRUE
)

cat("Rendering PDF version of huxtable vignette...\n")
# Render PDF version
pdf_file <- render(
  vignette_source,
  output_format = "pdf_document",
  output_file = file.path("..", "docs", "huxtable-pdf.pdf"),
  quiet = TRUE
)

cat("Vignettes generated successfully:\n")
cat("  HTML:", normalizePath(file.path("docs", "huxtable-html.html")), "\n")
cat("  PDF: ", normalizePath(file.path("docs", "huxtable-pdf.pdf")), "\n")
cat("Now add docs/*, commit and push\n")
