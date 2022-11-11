
Release with bugfixes. Resubmitting again to fix a check failure.

## Test environments

* local OS X install (R 4.2.0)
* rhub (windows/linux, release/devel)
* github (windows/linux, release/devel)
* windows on appveyor (release and stable)
* win-builder release/devel


## R CMD check results

A note about undeclared namespaces. This is a false positive: R6 and
xml2 are indeed used.

On Github, win-builder, Appveyor and locally: OK.


## revdepcheck results

glmmTMB had problems. These seem more likely to be do with different versions of
Matrix on my machine than with huxtable: errors are in functions which don't use huxtable.

Full report:

We checked 16 reverse dependencies, comparing R CMD check results across CRAN and dev versions of this package.

 * We saw 1 new problems
 * We failed to check 1 packages

Issues with CRAN packages are summarised below.

### New problems
(This reports the first line of each new failure)

* glmmTMB
  checking examples ... ERROR
  checking running R code from vignettes ...

### Failed to check

* bmstdr (NA)
