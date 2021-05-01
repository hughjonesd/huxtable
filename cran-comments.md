
A bugfix release.

## Test environments

* local OS X install (R 4.0.3)
* rhub (windows/linux, release/devel)
* github (windows/linux, release/devel)
* windows on appveyor (release and stable)
* win-builder devel


## R CMD check results

No errors, warnings or notes on rhub, github, appveyor, win-builder devel, 
or locally. Two rhub platforms failed while preparing for the build.


## Reverse dependencies

Of 9 reverse dependencies, 7 were fine. Two failed to check due to R CMD check
timing out (both with CRAN huxtable and this version).
