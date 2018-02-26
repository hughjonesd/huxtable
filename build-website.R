#!/usr/local/bin/Rscript

# script to rebuild website files

install.packages('huxtable') # so that the website is always synced with cran. This affects e.g. versions

for (f in list.files("docs", pattern = "*.Rmd", full.names = TRUE)) {
  message("Rendering ", f)
  rmarkdown::render(f, output_format = "html_document")
  rmarkdown::render(f, output_format = "pdf_document")
}

knitr::knit("docs/index.Rhtml", "docs/index.html")
pkgdown::build_reference()
pkgdown::build_reference_index()
pkgdown::build_news()
message("Now commit and push to github. Don't forget to reinstall the dev version!")
