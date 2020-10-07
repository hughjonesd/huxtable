
This is an update with a trivial test fix for an updated version of broom,
now on CRAN.

R CMD check generates a warning. As explained below, this is
a false positive. (The same warning was seen for the previous version, and
it was accepted by the CRAN team as a false positive.)


## Test environments

* local OS X install (R 4.0.2)
* travis-ci (oldrel, devel and release)
* windows on appveyor (release and stable)
* rhub for CRAN (Windows, devel)
* win-builder (devel and release)


## R CMD check results

Warnings/notes on various platforms:

*  Missing or unexported object: ‘huxtable::.(fun)’

This is a false positive, due to the use of `bquote()`.


## Reverse dependencies

3 failed to check (expss, glmmTMB, nlmixr) due to errors with the current CRAN
version. 8 others were all OK.

