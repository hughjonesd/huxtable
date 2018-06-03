
# package documentation and onLoad function --------------------------------------------------------


#' Huxtable: simply create LaTeX and HTML tables
#'
#' Huxtable is a package for creating HTML and LaTeX tables. It provides similar
#' functionality to xtable, with a simpler interface.
#'
#' @details
#'
#' To create a huxtable object, use [huxtable()] or [as_huxtable()].
#'
#' For more information, see \href{https://hughjonesd.github.io/huxtable/}{the website} or
#' read the vignette with `vignette('huxtable')`.
#'
#' @section Package options
#'
#' * `huxtable.add_colnames` sets the default value for `add_colnames` in
#'   [huxtable()] and [as_huxtable()]. If it is unset, `add_colnames` defaults to
#'   `FALSE`; in a future release, the default will become `TRUE`.
#' * `huxtable.print` sets the print method for huxtable objects. See [print.huxtable()].
#' * `huxtable.color_screen`. If `TRUE` and package `crayon` is available, huxtables
#'   will be printed in color on screen.
#' * `huxtable.knit_print_df`. If `TRUE` (the default), data frames in knitr will be
#'   pretty-printed using huxtable.
#' * `huxtable.knit_print_df_theme`. A one-argument function applied to theme the
#'   huxtableized data frame before printing in knitr. Defaults to [theme_plain()].
#' * `huxtable.use_resizebox_for_width`. If `TRUE`, LaTeX tables will be forced to their
#'   defined width using a `\resizebox`. This may be useful. At present it defaults to `FALSE`,
#'   which may change.
#' * `huxtable.autoformat` sets the default value for `autoformat` in [huxtable()] and
#'   [as_huxtable()]. It defaults to `TRUE`.
#' * `huxtable.autoformat_number_format` and `huxtable.autoformat_align` are
#'   lists. The list names are base R classes. [huxtable()] with `autoformat = TRUE` will set
#'   `number_format()` and `align()` for data columns according to the corresponding list values.
#'   For example, to center-align `Date` objects you could set `huxtable.autoformat_align` to
#'   something like `list(..., Date = "center", ...)`.
#'
#' @name huxtable-package
NULL


.onLoad <- function(libname, pkgname) {
  options(
    huxtable.print                         = getOption('huxtable.print', print_screen),
    huxtable.knit_print_df                 = getOption('huxtable.knit_print_df', TRUE),
    huxtable.knit_print_df_theme           = getOption('huxtable.knit_print_df_theme', theme_plain),
    huxtable.use_resizebox_for_width = getOption('huxtable.use_resizebox_for_width',
          FALSE),
    huxtable.color_screen                  = getOption('huxtable.color_screen',
          requireNamespace('crayon', quietly = TRUE)),
    huxtable.autoformat                    = getOption('huxtable.autoformat', TRUE),
    huxtable.autoformat_number_format      = getOption('huxtable.autoformat_number_format', list(
            integer = 0,
            numeric = "%.3g",
            complex = "%.3g"
          )),
    huxtable.autoformat_align              = getOption('huxtable.autoformat_align', list(
            numeric = getOption("OutDec", "."),
            complex = getOption("OutDec", "."),
            integer = "right",
            Date    = "right",
            POSIXct = "right",
            POSIXlt = "right"
          ))
  )
}


