
<table class="huxtable" style="border-collapse: collapse; width: 60pt; margin-left: auto; margin-right: auto;height: 60pt;">
<col style="width: 40%;">
<col style="width: 30%;">
<col style="width: 30%;">
<tr>
<td rowspan="2" style="vertical-align: top; text-align: left; white-space: nowrap; border-width:0.5pt 0.5pt 0.5pt 0.5pt; border-style: solid; border-top-color: NA;  border-right-color: NA;  border-bottom-color: NA;  border-left-color: NA; padding: 2pt 4pt 2pt 4pt; background-color: rgb(232, 58, 188); ">
<span style="font-size:42pt; font-family: Palatino, Palatino Linotype, Palatino LT STD, Book Antiqua, Georgia, serif; ">h</span>
</td>
<td style="vertical-align: top; text-align: left; white-space: nowrap; border-width:0.5pt 0.5pt 0.5pt 0.5pt; border-style: solid; border-top-color: NA;  border-right-color: NA;  border-bottom-color: NA;  border-left-color: NA; padding: 2pt 4pt 2pt 4pt; ">
<span style="font-size:24pt; font-family: Palatino, Palatino Linotype, Palatino LT STD, Book Antiqua, Georgia, serif; ">u</span>
</td>
<td style="vertical-align: top; text-align: left; white-space: nowrap; border-width:0.5pt 0.5pt 0.5pt 0.5pt; border-style: solid; border-top-color: NA;  border-right-color: NA;  border-bottom-color: NA;  border-left-color: NA; padding: 2pt 4pt 2pt 4pt; background-color: rgb(0, 0, 0); ">
<span style="color: rgb(255, 255, 255); font-size:24pt; font-family: Palatino, Palatino Linotype, Palatino LT STD, Book Antiqua, Georgia, serif; ">x</span>
</td>
</tr>
<tr>
<td colspan="2" style="vertical-align: top; text-align: center; white-space: nowrap; border-width:0.5pt 0.5pt 0.5pt 0.5pt; border-style: solid; border-top-color: NA;  border-right-color: NA;  border-bottom-color: NA;  border-left-color: NA; padding: 2pt 4pt 1pt 4pt; ">
<span style="font-size:20pt; font-family: Palatino, Palatino Linotype, Palatino LT STD, Book Antiqua, Georgia, serif; ">table</span>
</td>
</tr>
</table>
<!-- README.md is generated from README.Rmd. Please edit that file -->
<br>

[![CRAN Status Badge](http://www.r-pkg.org/badges/version/huxtable)](https://cran.r-project.org/package=huxtable) [![CRAN Downloads Per Month](http://cranlogs.r-pkg.org/badges/huxtable)](https://CRAN.R-project.org/package=huxtable) [![Travis-CI Build Status](https://travis-ci.org/hughjonesd/huxtable.svg?branch=master)](https://travis-ci.org/hughjonesd/huxtable) [![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/hughjonesd/huxtable?branch=master&svg=true)](https://ci.appveyor.com/project/hughjonesd/huxtable) [![Coverage Status](https://img.shields.io/codecov/c/github/hughjonesd/huxtable/master.svg)](https://codecov.io/github/hughjonesd/huxtable?branch=master)

Huxtable is an R package for creating HTML and LaTeX tables. It provides similar functionality to xtable, but does more, with a simpler interface. Features include:

-   Control over text styling, number format, background colour, borders, padding and alignment.
-   Table cells can span multiple rows and/or columns.
-   Table manipulation via standard R subsetting, or using `dplyr`.
-   `huxreg()` function for quick creation of regression tables.
-   Output to Microsoft Word, Excel or Powerpoint using the `officer` and `openxlsx` packages.
-   `quick_pdf()`, `quick_docx()`, `quick_html()` and `quick_xlsx()` one-liners to print data frames into a new document.
-   Formatted table display in the R console, including borders, colour, and text styles.

Installing
==========

To install from CRAN:

``` r
install.packages('huxtable')
```

To install the latest version from github:

``` r
install.packages('remotes')
remotes::install_github('hughjonesd/huxtable')
```

Learning more
=============

Check out [the website](https://hughjonesd.github.io/huxtable), read the [documentation](https://hughjonesd.github.io/huxtable/reference/index.html) or read the vignette in [HTML](https://hughjonesd.github.io/huxtable/huxtable.html) or [PDF](https://hughjonesd.github.io/huxtable/huxtable.pdf).
