#!/usr/local/bin/Rscript

library(devtools)
library(git2r)

gdiff <- git2r::diff(tree(commits()[[1]]))
if (length(gdiff) > 0) stop('Working tree differs from last commit, please make commits!')

for (f in list.files('vignettes', pattern = '.*\\.Rmd$')) {
  out <- system2('diff', args = c('-q', file.path('vignettes', f), file.path('docs', f)), stdout = TRUE)
  if (length(out) > 0) stop('vignettes and docs Rmd files differ, please fix!')
}


devtools::build()
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
  yn <- readLines(con="stdin", 1)
  if (! yn %in% c('Y', 'y')) stop('OK, stopping.')
}

newtag <- paste0('v', devtools::as.package('.')$version, '-rc')
tags <- tags()
tags <- grep(newtag, names(tags), fixed = TRUE, value = TRUE)
tags <- as.numeric(gsub(newtag, '', tags))
rc <- if (length(tags)) as.integer(max(tags) + 1) else 1L
newtag <- paste0(newtag, rc)
cat('\nTagging current version as: ', newtag, '\n')
tag(repository('.'), newtag, message = paste('CRAN release candidate for', v))
