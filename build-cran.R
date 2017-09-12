#!/usr/local/bin/Rscript

library(devtools)
library(git2r)
library(glue)

setwd('~/huxtable')

# Check git is up to date -----------------------------------------------------------------------------------------


gdiff <- git2r::diff(tree(commits()[[1]]))
if (length(gdiff) > 0) stop('Working tree differs from last commit, please make commits!')


# Check vignette files are same in vignettes and docs -------------------------------------------------------------


for (f in list.files('vignettes')) {
  out <- system2('diff', args = c('-q', file.path('vignettes', f), file.path('docs', f)), stdout = TRUE)
  if (length(out) > 0) stop(glue('vignettes and docs file {f} differs, please fix!'))
}



# Run R CMD check -------------------------------------------------------------------------------------------------

# this automatically builds the package
chk <- devtools::check(env_vars = c('RSTUDIO_PANDOC' = '/Applications/RStudio.app/Contents/MacOS/pandoc'),
      document = FALSE, check_version = TRUE)
if (length(chk$errors) > 0 || length(chk$warnings) > 0) {
  cat('\n\nR CMD CHECK errors:\n')
  cat(chk$errors)
  cat('\n\nR CMD CHECK warnings:\n')
  cat(chk$warnings)
  stop('Not tagging built release.')
}

if (length(chk$notes)) {
  cat('R CMD CHECK notes:\n')
  cat(chk$notes)
  cat('\nTag this version? y[n]')
  yn <- if (interactive()) readline() else readLines(con="stdin", 1)
  if (! yn %in% c('Y', 'y')) stop('OK, stopping.')
}


# Tag new version -------------------------------------------------------------------------------------------------


v <- devtools::as.package('.')$version
newtag <- paste0('v', v, '-rc')
tags <- tags()
tags <- grep(newtag, names(tags), fixed = TRUE, value = TRUE)
tags <- as.numeric(gsub(newtag, '', tags))
rc <- if (length(tags)) as.integer(max(tags) + 1) else 1L
newtag <- paste0(newtag, rc)
cat('\nTagging current version as: ', newtag, '\n')
tag(repository('.'), newtag, message = paste('CRAN release candidate for', v))

# Build package

devtools::build()
