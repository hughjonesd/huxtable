
Release for compatibility with dplyr 1.1.0.

## Test environments

* local OS X install (R 4.2.0)
* github (windows/linux, release/devel)
* windows on appveyor (release and stable)
* win-builder release/devel
* mac-builder release


## R CMD check results

A note about undeclared namespaces. This is a false positive: R6 and
xml2 are indeed used.

On Github, win-builder, Appveyor and locally: OK.


## revdepcheck results

glmmTMB had problems. These seem more likely to be do with different versions of
Matrix on my machine than with huxtable: errors are in functions which don't use huxtable.

Full report:

