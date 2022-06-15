#!/usr/local/bin/Rscript

library(devtools)
library(git2r)
library(glue)
library(rhub)
library(rstudioapi)


# Check version number is up to date----------------------------------------------------------------

v <- devtools::as.package('.')$version
if (grepl(v, "9000")) stop('Still using development version. Use usethis::use_version()')


# Check git is up to date --------------------------------------------------------------------------

gdiff <- git2r::diff(repository())
if (length(gdiff) > 0) stop('Working tree differs from last commit, please make commits!')
# nb this may fail for some reason



# Check vignette files are same in vignettes and docs ----------------------------------------------


for (f in list.files('vignettes', pattern = "(Rmd|cvs)$")) {
  out <- system2('diff', args = c('-q', file.path('vignettes', f), file.path('docs', f)), stdout = TRUE)
  if (length(out) > 0) stop(glue('vignettes and docs file {f} differs, please fix and commit!'))
}



# Run R CMD check ----------------------------------------------------------------------------------

# this automatically builds the package
chk <- devtools::check(
        env_vars = c('RSTUDIO_PANDOC' = '/Applications/RStudio.app/Contents/MacOS/pandoc'),
        document = FALSE,
        remote   = TRUE
      )
if (length(chk$errors) > 0 || length(chk$warnings) > 0) {
  cat('\n\nR CMD CHECK errors:\n')
  cat(chk$errors)
  cat('\n\nR CMD CHECK warnings:\n')
  cat(chk$warnings)
}

if (length(chk$notes)) {
  cat('R CMD CHECK notes:\n')
  cat(chk$notes)
}


# manual section --------

# run checks

rhc <- rhub::check_for_cran(show_status = FALSE)

# asks manual questions
devtools::check_win_devel()

# asks manual questions
devtools::check_win_release()

# may not work; if so just run the script in the main window.
rstudioapi::jobRunScript("check-reverse-dependencies.R", exportEnv = "revdep_results")


# update CRAN-comments.md

# Tag new version

newtag <- paste0('v', v, '-rc')
tags <- tags()
tags <- grep(newtag, names(tags), fixed = TRUE, value = TRUE)
tags <- as.numeric(gsub(newtag, '', tags))
rc <- if (length(tags)) as.integer(max(tags) + 1) else 1L
newtag <- paste0(newtag, rc)
cat('\nTagging current version as: ', newtag, '\n')
tag(repository('.'), newtag, message = paste('CRAN release candidate for', v))
system2('git', c('push', '--tags'))


# now release!

devtools::release()

# after release:
revdepcheck::revdep_reset()
