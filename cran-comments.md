
Major update. This should also fix an existing bug revealed by R-devel. 
Resubmitting with a bumped patch version to fix a bug exposed by a 
reverse dependency failure.

## Test environments

* local OS X install (R 4.6.1)
* win-builder devel
* mac-builder devel
* GitHub Windows/Mac/Linux


## R CMD check results

OK on all platforms.


## revdepcheck results

24 reverse dependencies checked. pharmaRTF remains broken, its maintainers
were notified two weeks ago. glmmTMB should now be fixed.



