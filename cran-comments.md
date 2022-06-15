
Release with some new features.

## Test environments

* local OS X install (R 4.2.0)
* rhub (windows/linux, release/devel)
* github (windows/linux, release/devel)
* windows on appveyor (release and stable)
* win-builder release/devel


## R CMD check results

A note about undeclared namespaces. This is a false positive: R6 and
xml2 are indeed used.

On rhub, a note about detritus in the temp directory.

Github fails to build one vignette. Debugging is hard.

On win-builder, OK.

On Appveyor, OK.

## revdepcheck results

We checked 15 reverse dependencies, comparing R CMD check results across CRAN 
and dev versions of this package.

 * We saw 0 new problems
 * We failed to check 3 packages

Issues with CRAN packages are summarised below.

### Failed to check

* bmstdr  (NA)
* glmmTMB (NA)
* nlmixr  (NA)
