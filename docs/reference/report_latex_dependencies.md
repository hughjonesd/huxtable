# Manage LaTeX dependencies for huxtables

`report_latex_dependencies` prints out and/or returns a list of LaTeX
dependencies for adding to a LaTeX preamble.

`check_latex_dependencies` checks whether the required LaTeX packages
are installed.

`install_latex_dependencies` is a utility function to install and/or
update the LaTeX packages that huxtable requires. It calls
[`tinytex::tlmgr_install()`](https://rdrr.io/pkg/tinytex/man/tlmgr.html)
if possible, or `tlmgr install` directly.

## Usage

``` r
report_latex_dependencies(quiet = FALSE, as_string = FALSE)

check_latex_dependencies(quiet = FALSE)

install_latex_dependencies()
```

## Arguments

- quiet:

  Logical. For `report_latex_dependencies`, suppress printing of
  dependencies. For `check_latex_dependencies`, suppress messages.

- as_string:

  Logical: return dependencies as a string.

## Value

If `as_string` is `TRUE`, `report_latex_dependencies` returns a string
of `"\\\\usepackage\\{...\\}"` statements; otherwise it returns a list
of
[`rmarkdown::latex_dependency`](https://pkgs.rstudio.com/rmarkdown/reference/latex_dependency.html)
objects, invisibly.

`check_latex_dependencies()` returns `TRUE` or `FALSE`.

`install_latex_dependencies` returns `TRUE` if `tlmgr` returns 0.

## Examples

``` r
report_latex_dependencies()
#> \usepackage{array}
#> \usepackage{caption}
#> \usepackage{graphicx}
#> \usepackage{siunitx}
#> \usepackage[normalem]{ulem}
#> \usepackage{colortbl}
#> \usepackage{multirow}
#> \usepackage{hhline}
#> \usepackage{calc}
#> \usepackage{tabularx}
#> \usepackage{threeparttable}
#> \usepackage{wrapfig}
#> \usepackage{adjustbox}
#> \usepackage{hyperref}
#> % These are LaTeX packages. You can install them using your LaTex management software,
#> % or by running `huxtable::install_latex_dependencies()` from within R.
#> % Other packages may be required if you use non-standard tabulars (e.g. tabulary).

if (FALSE) { # \dontrun{
check_latex_dependencies()
} # }
if (FALSE) { # \dontrun{
install_latex_dependencies()
} # }
```
