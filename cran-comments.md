
This is an update with some bugfixes and new features.


## Test environments

* local OS X install (R 4.0.2)
* travis-ci (oldrel, devel and release)
* windows on appveyor (release and stable)
* rhub for CRAN (Windows, devel; failed to build on Linux due to rhub issues)
* win-builder (devel and release)


## R CMD check results

2 notes:
  Missing or unexported object: ‘huxtable::.(fun)’
This is a false positive due to the use of `bquote()`.
  Unable to verify current time
Presumably due to platform rather than to huxtable.


## Reverse dependencies

