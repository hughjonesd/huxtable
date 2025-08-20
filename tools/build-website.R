#!/usr/local/bin/Rscript

# script to rebuild website files

library(rmarkdown)

# pkgdown works from your tree, not from installed versions.
# But rmarkdown::render works with installed versions.
# So:
# install the version you want
# optionally create a branch (website-x.y.z)
# update index.Rhtml appropriately
# run this script
# commit any changes
# optionally checkout master, merge in the new branch (git merge website-x.y.z)
# push to github


pdf_output_formats <- list(
  "design-principles.Rmd" = pdf_document(latex_engine = "xelatex"),
  "huxreg.Rmd"            = pdf_document(latex_engine = "xelatex"),
  "huxtable.Rmd"          = pdf_document(
                              latex_engine = "xelatex",
                              toc = TRUE,
                              toc_depth = 2
                            )
)
for (f in list.files("docs", pattern = "*.Rmd", full.names = TRUE)) {
  filename <- basename(f)
  message("Rendering ", f)
  filename_no_ext <- sub("\\.Rmd$", "", filename)
  rmarkdown::render(f,
                    output_format = "html_document",
                    output_file   = paste0(filename_no_ext, "-html"))
  rmarkdown::render(f,
                    output_format = pdf_output_formats[[filename]],
                    output_file   = paste0(filename_no_ext, "-pdf"))
}
setwd('docs')
knitr::knit("index.Rhtml", "index.html")
knitr::knit("themes.Rhtml", "themes-html.html")
setwd('..')

pkgdown::build_reference_index()
pkgdown::build_reference(lazy = FALSE)
pkgdown::build_news()
pkgdown::build_tutorials()

message("Now commit and push to github.")
