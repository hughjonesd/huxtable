#!/usr/local/bin/Rscript

library(devtools)
library(git2r)

gdiff <- git2r::diff(tree(commits()[[1]]))
if (length(gdiff) > 0) stop('Working tree differs from last commit, please make commits!')

v <- devtools::as.package('.')$version
huxtar <- paste0('huxtable_', v, '.tar.gz')
chk <- devtools::check(env_vars = c('RSTUDIO_PANDOC' = '/Applications/RStudio.app/Contents/MacOS/pandoc'),
      document = FALSE, check_version = TRUE)
# system("R", args = c('CMD', 'CHECK', '--as-cran', huxtar),
#      env = c('RSTUDIO_PANDOC' = '/Applications/RStudio.app/Contents/MacOS/pandoc'))
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
  cat('Tag this version? y[n]')
  yn <- readLines(con="stdin", 1)
  if (! yn %in% c('Y', 'y')) stop('OK, stopping')
}

newtag <- paste0('v', v, '-rc')
tags <- tags()
tags <- grep(newtag, names(tags), fixed = TRUE, value = TRUE)
tags <- as.numeric(gsub(v, '', tags))
rc <- if (length(tags)) as.integer(max(tags) + 1) else 1L
newtag <- paste0(newtag, rc)
cat('Tagging current version as ', newtag)
tag(repository('.'), newtag, message = paste('CRAN release candidate for', v))
