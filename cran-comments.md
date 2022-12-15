
Release for compatibility with dplyr 1.1.0.

## Test environments

* local OS X install (R 4.2.0)
* github (windows/linux/mac, release/devel)
* win-builder release/devel/oldrelease
* mac-builder release


## R CMD check results

A note about undeclared namespaces. This is a false positive: R6 and
xml2 are indeed used. On oldrelease a note about RTF being misspelled,

Otherwise OK.


## revdepcheck results

Two packages failed to check due to R CMD check timeouts. The other 14 packages 
were fine.

Full report:

