
This adds a couple of backwards-compatible features.

## Test environments

* local OS X install (R 3.5.2)
* ubuntu 14.04 on travis-ci (oldrel, devel and release)
* windows on appveyor (devel, release and stable)
* rhub on windows (rhub had its own problems on other platforms)
* win-builder (devel and release)

## R CMD check results

0 errors | 0 warnings | 0 notes on all the above platforms.

Reverse dependencies were also checked (envirem, expss, interactions, jtools).
expss showed a single warning (it also gets one on CRAN, on some platforms). 
nlmixr failed to install, as previously. Others showed only notes or nothing.

