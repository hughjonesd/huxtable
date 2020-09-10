
This is an update with some bugfixes and new features.


## Test environments

* local OS X install (R 4.0.2)
* travis-ci (oldrel, devel and release)
* windows on appveyor (release and stable)
* rhub for CRAN (Windows, devel)
* win-builder (devel and release)


## R CMD check results

3 notes on various platforms:

*  Missing or unexported object: ‘huxtable::.(fun)’
This is a false positive, due to the use of `bquote()`.

*  Unable to verify current time
Presumably due to platform rather than to huxtable.

*  Notes about URLs missing a final slash, e.g. 
  "URL: https://commonmark.org/help (moved to https://commonmark.org/help/)"
I think these are false positives, unless we are being very pernickety.


## Reverse dependencies

3 failed to check (expss, glmmTMB, nlmixr) due to errors with the current CRAN
version. 8 others were all OK.

