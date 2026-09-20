Patch release in response to a CRAN additional check failure on the M1mac
check service. The failure was caused by a malformed embedded ICC profile in
a PNG test fixture, newly reported by that platform's ImageMagick/libpng stack.
The fixture has been re-encoded without the profile; its pixels are unchanged.

## Test environments

* local macOS Tahoe 26.6.2 (R 4.6.1)
* win-builder devel
* GitHub Actions on Windows, macOS and Linux

## R CMD check results

0 errors | 0 warnings | 1 note

The NOTE reports that the previous CRAN update was five days ago. This quick
resubmission fixes the additional check failure described above.

## Downstream dependencies

Reverse dependency checks were not rerun because the functional change is
limited to knitr caption handling and the CRAN fix only re-encodes a test
fixture.
