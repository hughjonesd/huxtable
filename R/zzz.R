
# package documentation and onLoad function --------------------------------------------------------


#' Quick introduction to huxtable
#'
#' Huxtable is a package for creating HTML and LaTeX tables. It provides similar
#' functionality to xtable, with a simpler interface.
#'
#' @section Quick start:
#'
#' To create a huxtable object, use [huxtable()] or [as_huxtable()]:
#'
#' ```
#'  library(huxtable)
#'  employees <- huxtable(
#'          Names    = c("Hadley", "Yihui", "Dirk"),
#'          Salaries = c(1e5, 1e5, 1e5),
#'          add_colnames = TRUE
#'        )
#'  car_hux <- as_hux(mtcars, add_colnames = TRUE)
#'
#' ```
#'
#' You can then set properties which affect how the huxtable is displayed:
#'
#' ```
#'  # make the first row bold:
#'  bold(employees)[1, ] <- TRUE
#'
#'  # change the font size everywhere:
#'  font_size(employees) <- 10
#' ```
#'
#' Or you can use a tidyverse style with the pipe operator:
#'
#' ```
#' library(magrittr)
#' employees <- employees %>%
#'       set_font_size(10) %>%
#'       set_bold(1, everywhere, TRUE)
#'
#' ```
#'
#' For more information, see \href{https://hughjonesd.github.io/huxtable/}{the website} or
#' read the vignette with `vignette('huxtable')`.
#'
#' See [huxtable-FAQ] for frequently asked questions, including ways to get
#' help.
#'
#' To report a bug, or suggest an enhancement, visit
#' \href{https://github.com/hughjonesd/huxtable/issues}{github}.
#'
#' @name huxtable-package
#' @aliases huxtable_package
"_PACKAGE"


#' Package options
#'
#' * `options('huxtable.add_colnames')` sets the default value for `add_colnames` in
#'   [huxtable()] and [as_huxtable()]. If it is unset, `add_colnames` defaults to
#'   `FALSE`; in a future release, the default will become `TRUE`.
#' * `options('huxtable.print')` sets the print method for huxtable objects. See [print.huxtable()].
#' * `options('huxtable.knitr_output_format')` overrides the default output format when huxtable
#'   objects are printed by knitr. Set to "html", "latex", "md" or "screen". If `NULL` (the default),
#'   huxtable guesses the format using [guess_knitr_output_format()].
#' * `options('huxtable.color_screen')`. If `TRUE` and package `crayon` is available, huxtables
#'    will be printed in color on screen.
#' * `options('huxtable.bookdown')`. Set to `TRUE` within a bookdown document to automatically
#'   print bookdown-style labels. If unset, huxtable will try to guess if we are in a bookdown
#'   document.
#' * `options('huxtable.knit_print_df')`. If `TRUE` (the default), data frames in knitr will be
#'   pretty-printed using huxtable.
#' * `options('huxtable.knit_print_df_theme')`. A function applied to data frames
#'    before printing in knitr. The function should take one argument (a data frame) and return a
#'    huxtable. Defaults to [theme_plain()].
#' * `options('huxtable.autoformat')` sets the default value for `autoformat` in [huxtable()] and
#'   [as_huxtable()]. It defaults to `TRUE`.
#' * `options('huxtable.latex_use_fontspec')`. If `TRUE`, use the "fontspec" package, which allows
#'    you to use the same font names in TeX and HTML. This requires the the xetex or xelatex
#'    engine, which can be set using an .rmd header option. Note that [quick_pdf()]
#'    may use pdflatex. It defaults to `FALSE`.
#' * `options('huxtable.autoformat_number_format')` and `options('huxtable.autoformat_align')` are
#'   lists. The list names are base R classes. [huxtable()] with `autoformat = TRUE` will set
#'   `number_format()` and `align()` for data columns according to the corresponding list values.
#'   For example, to center-align `Date` objects you could set `"huxtable.autoformat_align"` to
#'   something like `list(..., Date = "center", ...)`.
#' @name huxtable-options
#' @docType package
#' @aliases huxtable_options
NULL



#' Frequently Asked Questions, including how to get help
#'
#' A FAQ of common issues.
#'
#' * LaTeX output isn't working.
#'
#' Have you installed the LaTeX packages you need? LaTeX packages are different from R packages. Run
#' [check_latex_dependencies()] to find out if you are missing any. Then install them using your
#' system's LaTeX management application. Or you can try [install_latex_dependencies()].
#'
#' * Numbers in my cells look weird!
#'
#' You can change numeric formatting using [number_format()]. Base R options like
#' [`scipen`][base::options()] usually have no effect.
#'
#' * I ran `caption(ht) <- "Something"` and got an error message:
#'
#' ```
#' Error in UseMethod("caption<-") :
#' no applicable method for 'caption<-' applied to an object of class "c('huxtable', 'data.frame')"
#' ```
#'
#' You may have loaded another package with a `caption` method, e.g. "xtable". Try loading huxtable
#' after xtable.
#'
#' * My tables aren't centered correctly (LaTeX).
#'
#' Try adjusting `width(ht)`.
#'
#' * How can I change the font size, font etc. of captions?
#'
#' There are no direct commands for this. You have to use raw HTML/TeX/other commands within the
#' caption itself. For example to have a bold caption in HTML, you might do something like:
#'
#' ```
#' set_caption(jams, "<b>Jam Prices</b>")
#'
#' ```
#'
#' * How do I refer to tables in bookdown?
#'
#' As of version 4.3.0, this is handled automatically for you. Just
#' set the label using [label()], then in markdown text do e.g.:
#'
#' ```
#' \\@ref(tab:my-table-label).
#'
#' ```
#'
#' * I have another problem.
#'
#' If you have a bug - i.e. a problem with the software - or have a feature request, please report
#' it to <https://github.com/hughjonesd/huxtable/issues>. Otherwise, ask a question on
#' [StackOverflow](https://stackoverflow.com) or <https://community.rstudio.com>. That way, other
#' people will benefit from the answers you get.
#'
#' * Can I email you directly?
#'
#' I'd rather you asked on a public website. If you then email me a link, I may be able to help.
#'
#' @name huxtable-FAQ
#' @docType package
#' @aliases huxtable_FAQ
NULL


#' Deprecated functions
#'
#' These functions are deprecated and will be removed in future versions of huxtable.
#'
#' To replace the 3 argument form of `set_xxx` functions, use
#' \code{\link[=mapping-functions]{map_xxx}}.
#' @name huxtable-deprecated
NULL

#' Prices of 3 jams
#'
#' A huxtable of jams.
#'
#' @format A huxtable with 4 rows and 2 columns ("Type" and "Price").
"jams"

.onLoad <- function(libname, pkgname) {

  set_default_option <- function (opt, value) {
    ol <- list(getOption(opt, value))
    names(ol) <- opt
    options(ol)
  }

  set_default_option('huxtable.print', print_screen)
  set_default_option('huxtable.knit_print_df', TRUE)
  set_default_option('huxtable.knit_print_df_theme', theme_plain)
  set_default_option('huxtable.color_screen', requireNamespace('crayon', quietly = TRUE))
  set_default_option('huxtable.bookdown', NULL)
  set_default_option('huxtable.autoformat', TRUE)
  set_default_option('huxtable.latex_use_fontspec', FALSE)
  set_default_option('huxtable.autoformat_number_format', list(
          integer = 0,
          numeric = "%.3g",
          complex = "%.3g"
        ))
  set_default_option('huxtable.autoformat_align', list(
          numeric = getOption('OutDec', '.'),
          complex = getOption('OutDec', '.'),
          integer = 'right',
          Date    = 'right',
          POSIXct = 'right',
          POSIXlt = 'right'
        ))

  if (requireNamespace('dplyr', quietly = TRUE)) {
    register_s3_method('dplyr', 'arrange')
    register_s3_method('dplyr', 'arrange_')
    register_s3_method('dplyr', 'filter')
    register_s3_method('dplyr', 'filter_')
    register_s3_method('dplyr', 'mutate')
    register_s3_method('dplyr', 'mutate_')
    register_s3_method('dplyr', 'rename')
    register_s3_method('dplyr', 'rename_')
    register_s3_method('dplyr', 'select')
    register_s3_method('dplyr', 'select_')
    register_s3_method('dplyr', 'slice')
    register_s3_method('dplyr', 'slice_')
    register_s3_method('dplyr', 'transmute')
    register_s3_method('dplyr', 'transmute_')
  }
  if (requireNamespace('knitr', quietly = TRUE)) {
    register_s3_method('knitr', 'knit_print')
    register_s3_method('knitr', 'knit_print', class = 'data.frame')
  }
}
