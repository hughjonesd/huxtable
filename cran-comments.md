
This fixes a testing bug in r-devel on CRAN (maybe down to the new PCRE2 library?)
but also adds a couple of features.

## Test environments

* local OS X install (R 3.6.1)
* ubuntu 14.04 on travis-ci (oldrel, devel and release)
* windows on appveyor (devel, release and stable)
* win-builder (devel and release)

## R CMD check results

0 errors | 0 warnings | 0 notes on all the above platforms.

Reverse dependencies were also checked (jtools, interactions, envirem).
nlmixr failed to install.
