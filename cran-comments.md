
This is a major new release, with new features, which breaks backwards 
compatibility in (documented) ways.


## Test environments

* local OS X install (R 4.0.0)
* travis-ci (oldrel, devel and release)
* windows on appveyor (release and stable)
* rhub for CRAN (Windows, Linux; release, devel)
* win-builder (devel and release)

## R CMD check results

0 errors | 0 warnings | 0 notes on Mac.

win-builder showed a note about "RTF" as a possibly misspelled word in 
DESCRIPTION. I think RTF is a well-known acronym, and doubt that putting it
in quote marks would aid readability.

travis gives errors on latest r-devel about "non-file package anchored links"
in (e.g.) `dplyr-verbs.Rd`. 



I informed reverse dependency package owners of the upcoming release by email 
on 12 May.

Ten reverse dependencies were checked. Most were fine.

* pharmaRTF showed problems. I have been in touch with the developers and
  they plan to fix them "within the week". I'd wait longer, but huxtable just
  got archived for failing a check (which this update should fix).
  
* texreg has a trivial test failure using `testthat::expect_known_value`. However,
  I manually checked the code within their `huxtablereg` function and did not
  find any problems.

* nlmixr failed to compile on my machine, as last time, so could not be checked.
