
This release addresses test failures due to a recent update to the 
"stringi" package. "stringi" is being updated too, but this update
should make the package more robust. It also fixes a few other bugs
and introduces some small features.

## Test environments

* local OS X install (R 4.0.3)
* rhub (windows/linux, release/devel)
* github (windows/linux, release/devel)
* windows on appveyor (release and stable)
* win-builder release/devel


## R CMD check results

One note on rhub and github: "Unable to find GhostScript executable...". Two 
failures-to-build on rhub. Everything else OK.


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
