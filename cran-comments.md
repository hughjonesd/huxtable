
Bugfix release.

## Test environments

* local OS X install (R 4.3.2)
* win-builder release/devel
* mac-builder release


## R CMD check results

A note about undeclared namespaces. This is a false positive: R6 and
xml2 are indeed used. 

win-builder release gave an error about a test failure involving quarto. I think
this is probably a temporary or configuration problem on win-builder, since 
the quarto tests work fine elsewhere, including on win-builder devel.

## revdepcheck results

19 out of 19 packages were OK.


