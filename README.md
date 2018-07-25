
<table class="huxtable" style="border-collapse: collapse; margin-bottom: 2em; margin-top: 2em; width: 60pt; margin-left: auto; margin-right: auto;height: 60pt;">
<col style="width: 40%;">
<col style="width: 30%;">
<col style="width: 30%;">
<tr>
<td rowspan="2" style="vertical-align: top; text-align: left; white-space: nowrap; border-width:0.5pt 0.5pt 0.5pt 0.5pt; border-style: solid; padding: 2pt 4pt 2pt 4pt; background-color: rgb(232, 58, 188); ">
<span style="font-size:42pt; font-family: Palatino; ">h</span>
</td>
<td style="vertical-align: top; text-align: left; white-space: nowrap; border-width:0.5pt 0.5pt 0.5pt 0.5pt; border-style: solid; padding: 2pt 4pt 2pt 4pt; ">
<span style="font-size:24pt; font-family: Palatino; ">u</span>
</td>
<td style="vertical-align: top; text-align: left; white-space: nowrap; border-width:0.5pt 0.5pt 0.5pt 0.5pt; border-style: solid; padding: 2pt 4pt 2pt 4pt; background-color: rgb(0, 0, 0); ">
<span style="color: rgb(255, 255, 255); font-size:24pt; font-family: Palatino; ">x</span>
</td>
</tr>
<tr>
<td colspan="2" style="vertical-align: top; text-align: center; white-space: nowrap; border-width:0.5pt 0.5pt 0.5pt 0.5pt; border-style: solid; padding: 2pt 4pt 1pt 4pt; ">
<span style="font-size:20pt; font-family: Palatino; ">table</span>
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
-   Automatic formatting for knitr/rmarkdown documents.
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

Examples
========

Standard style:

``` r

ht <- hux(
        Employee = c('John Smith', 'Jane Doe', 'David Hugh-Jones'),
        Salary = c(50000, 50000, 40000),
        add_colnames = TRUE
      )

bold(ht)[1,]           <- TRUE
bottom_border(ht)[1,]  <- 1
align(ht)[,2]          <- 'right'
right_padding(ht)      <- 10
left_padding(ht)       <- 10
width(ht)              <- 0.35
number_format(ht)      <- 2

ht
```

<!--html_preserve-->
<table class="huxtable" style="border-collapse: collapse; margin-bottom: 2em; margin-top: 2em; width: 35%; margin-left: auto; margin-right: auto;">
<col style="width: NA;">
<col style="width: NA;">
<tr>
<td style="vertical-align: top; text-align: left; white-space: nowrap; border-width:0pt 0pt 1pt 0pt; border-style: solid; padding: 4pt 10pt 4pt 10pt; ">
<span style="font-weight: bold; ">Employee</span>
</td>
<td style="vertical-align: top; text-align: right; white-space: nowrap; border-width:0pt 0pt 1pt 0pt; border-style: solid; padding: 4pt 10pt 4pt 10pt; ">
<span style="font-weight: bold; ">Salary</span>
</td>
</tr>
<tr>
<td style="vertical-align: top; text-align: left; white-space: nowrap; border-width:0pt 0pt 0pt 0pt; border-style: solid; padding: 4pt 10pt 4pt 10pt; ">
John Smith
</td>
<td style="vertical-align: top; text-align: right; white-space: nowrap; border-width:0pt 0pt 0pt 0pt; border-style: solid; padding: 4pt 10pt 4pt 10pt; ">
50000.00
</td>
</tr>
<tr>
<td style="vertical-align: top; text-align: left; white-space: nowrap; border-width:0pt 0pt 0pt 0pt; border-style: solid; padding: 4pt 10pt 4pt 10pt; ">
Jane Doe
</td>
<td style="vertical-align: top; text-align: right; white-space: nowrap; border-width:0pt 0pt 0pt 0pt; border-style: solid; padding: 4pt 10pt 4pt 10pt; ">
50000.00
</td>
</tr>
<tr>
<td style="vertical-align: top; text-align: left; white-space: nowrap; border-width:0pt 0pt 0pt 0pt; border-style: solid; padding: 4pt 10pt 4pt 10pt; ">
David Hugh-Jones
</td>
<td style="vertical-align: top; text-align: right; white-space: nowrap; border-width:0pt 0pt 0pt 0pt; border-style: solid; padding: 4pt 10pt 4pt 10pt; ">
40000.00
</td>
</tr>
</table>
<!--/html_preserve-->
Tidyverse style:

``` r

library(magrittr)

ht <- hux(
        Employee = c('John Smith', 'Jane Doe', 'David Hugh-Jones'),
        Salary = c(50000, 50000, 40000),
        add_colnames = TRUE
      )

ht                                        %>%
      set_bold(1, everywhere, TRUE)       %>%
      set_bottom_border(1, everywhere, 1) %>%
      set_align(everywhere, 2, 'right')   %>%
      set_right_padding(10)               %>%
      set_left_padding(10)                %>%
      set_width(0.35)                     %>%
      set_number_format(2)
```

<!--html_preserve-->
<table class="huxtable" style="border-collapse: collapse; margin-bottom: 2em; margin-top: 2em; width: 35%; margin-left: auto; margin-right: auto;">
<col style="width: NA;">
<col style="width: NA;">
<tr>
<td style="vertical-align: top; text-align: left; white-space: nowrap; border-width:0pt 0pt 1pt 0pt; border-style: solid; padding: 4pt 10pt 4pt 10pt; ">
<span style="font-weight: bold; ">Employee</span>
</td>
<td style="vertical-align: top; text-align: right; white-space: nowrap; border-width:0pt 0pt 1pt 0pt; border-style: solid; padding: 4pt 10pt 4pt 10pt; ">
<span style="font-weight: bold; ">Salary</span>
</td>
</tr>
<tr>
<td style="vertical-align: top; text-align: left; white-space: nowrap; border-width:0pt 0pt 0pt 0pt; border-style: solid; padding: 4pt 10pt 4pt 10pt; ">
John Smith
</td>
<td style="vertical-align: top; text-align: right; white-space: nowrap; border-width:0pt 0pt 0pt 0pt; border-style: solid; padding: 4pt 10pt 4pt 10pt; ">
50000.00
</td>
</tr>
<tr>
<td style="vertical-align: top; text-align: left; white-space: nowrap; border-width:0pt 0pt 0pt 0pt; border-style: solid; padding: 4pt 10pt 4pt 10pt; ">
Jane Doe
</td>
<td style="vertical-align: top; text-align: right; white-space: nowrap; border-width:0pt 0pt 0pt 0pt; border-style: solid; padding: 4pt 10pt 4pt 10pt; ">
50000.00
</td>
</tr>
<tr>
<td style="vertical-align: top; text-align: left; white-space: nowrap; border-width:0pt 0pt 0pt 0pt; border-style: solid; padding: 4pt 10pt 4pt 10pt; ">
David Hugh-Jones
</td>
<td style="vertical-align: top; text-align: right; white-space: nowrap; border-width:0pt 0pt 0pt 0pt; border-style: solid; padding: 4pt 10pt 4pt 10pt; ">
40000.00
</td>
</tr>
</table>
<!--/html_preserve-->
 

Learning more
=============

Check out [the website](https://hughjonesd.github.io/huxtable), read the [documentation](https://hughjonesd.github.io/huxtable/reference/index.html) or read the vignette in [HTML](https://hughjonesd.github.io/huxtable/huxtable.html) or [PDF](https://hughjonesd.github.io/huxtable/huxtable.pdf).
