
This update adds a new feature, and fixes a test which failed due to a bug in
broom and was presumably not caught by a reverse dependency check.


## Test environments

* local OS X install (R 4.0.3)
* github (windows/linux, release/devel)
* windows on appveyor (release and stable)
* win-builder (devel and release)


## R CMD check results

No warnings on local, win-builder, github or appveyor. Notes relate to 
'misspelled word' RTF (false positive) and are otherwise about the package
being archived, including one non-working URL which will work again when
the package is in place on CRAN.


## Reverse dependencies

Reverse dependencies couldn't be checked because the CRAN package was archived.
It's unlikely that the changes introduce problems for dependencies.
