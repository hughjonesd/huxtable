
This release addresses test failures due to a recent update to the 
"stringi" package. "stringi" is being updated too, but this update
should make the package more robust. It also fixes a few other bugs
and introduces some small features.

## Test environments

* local OS X install (R 4.0.3)
* rhub (windows/linux, release/devel)
* github (windows/linux, release/devel)
* windows on appveyor (release and stable)
* win-builder devel


## R CMD check results

No errors, warnings or notes on rhub, github, appveyor, win-builder devel, 
or locally. Two rhub platforms failed while preparing for the build.

## revdepcheck results

We checked 12 reverse dependencies, comparing R CMD check results across CRAN and dev versions of this package.

 * We saw 0 new problems
 * We failed to check 4 packages

Issues with CRAN packages are summarised below.

### Failed to check

* glmmTMB      (NA)
* gtsummary    (NA)
* interactions (NA)
* nlmixr       (NA)
