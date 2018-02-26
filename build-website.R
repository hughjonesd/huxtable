#!/usr/local/bin/Rscript


# script to rebuild website files

# replace the below with the tag you want:
version_tag <- "v3.0.0-rc1"

library(git2r)
repo <- repository()
if (! version_tag %in% (nms <- names(tags()))) stop("version_tag must be one of", nms)
# we will build from this version:
tag_obj <- tags()[[version_tag]]
checkout(tag_obj)
detach('package:git2r') # stops it interfering with psych functions
devtools::install()
# checkout master again so our changes in docs will apply to that branch
git2r::checkout(repo, "master")

for (f in list.files("docs", pattern = "*.Rmd", full.names = TRUE)) {
  message("Rendering ", f)
  rmarkdown::render(f, output_format = "html_document")
  rmarkdown::render(f, output_format = "pdf_document")
}

knitr::knit("docs/index.Rhtml", "docs/index.html")

# back to the tagged version to build the reference with pkgdown
git2r::checkout(repo, tag_obj)
pkgdown::build_reference()
pkgdown::build_reference_index()
pkgdown::build_news()

library(git2r)
add(repo, "docs/*")
pkgdown_commit <- commit(repo, message = paste("Updating website for version tag", version_tag))
merge(repo, pkgdown_commit)

