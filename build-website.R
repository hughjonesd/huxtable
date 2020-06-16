#!/usr/local/bin/Rscript

# script to rebuild website files

library(rmarkdown)

# pkgdown works from your tree, not from installed versions.
# But rmarkdown::render works with installed versions.
# So:
# check this includes the new version:

pkgdown:::pkg_timeline("huxtable")

#   (otherwise, it'll be marked as 'unreleased' in the news page)
# checkout the version that you want on the web
# ** Install the package and restart **
# create a branch if there isn't one (git checkout -b website-x.y.z)
# update index.Rhtml appropriately
# run this script;
# commit any changes and push to github;
# checkout master;
# merge in the new branch (git merge website-x.y.z)
# push to github

pdf_output_formats <- list(
  "huxreg.Rmd"            = pdf_document(
                              includes = includes(
                                in_header = "placeins-header.tex"
                              )
                            ),
  "themes.Rmd"            = pdf_document(),
  "huxtable.Rmd"          = pdf_document(
                              latex_engine = "xelatex",
                              toc = TRUE,
                              toc_depth = 2,
                              includes = includes(
                                in_header = "placeins-header.tex"
                              )
                            ),
  "design-principles.Rmd" = pdf_document()
)

for (f in list.files("docs", pattern = "*.Rmd", full.names = TRUE)) {
  filename <- basename(f)
  message("Rendering ", f)
  rmarkdown::render(f, output_format = "html_document")
  rmarkdown::render(f, output_format = pdf_output_formats[[filename]])
}

setwd('docs')
knitr::knit("index.Rhtml", "index.html")
knitr::knit("themes.Rhtml", "themes.html")
setwd('..')

pkgdown::build_reference_index()
pkgdown::build_reference(lazy = FALSE)
pkgdown::build_news()
pkgdown::build_tutorials()
message("Now commit and push to github. Don't forget to reinstall the dev version!")
