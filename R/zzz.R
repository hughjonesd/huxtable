
# package documentation and onLoad function --------------------------------------------------------


#' Huxtable: simply create LaTeX and HTML tables
#'
#' Huxtable is a package for creating HTML and LaTeX tables. It provides similar
#' functionality to xtable, with a simpler interface.
#'
#' To create a huxtable object, use [huxtable()] or [as_huxtable()].
#'
#' For more information, see \href{https://hughjonesd.github.io/huxtable/}{the website} or
#' read the vignette with `vignette('huxtable')`.
#'
#' @seealso huxtable-options
#' @name huxtable-package
NULL


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
#' * `options('huxtable.knit_print_df')`. If `TRUE` (the default), data frames in knitr will be
#'   pretty-printed using huxtable.
#' * `options('huxtable.knit_print_df_theme')`. A function applied to data frames
#'    before printing in knitr. The function should take one argument (a data frame) and return a
#'    huxtable. Defaults to [theme_plain()].
#' * `options('huxtable.autoformat')` sets the default value for `autoformat` in [huxtable()] and
#'   [as_huxtable()]. It defaults to `TRUE`.
#' * `options('huxtable.autoformat_number_format')` and `options('huxtable.autoformat_align')` are
#'   lists. The list names are base R classes. [huxtable()] with `autoformat = TRUE` will set
#'   `number_format()` and `align()` for data columns according to the corresponding list values.
#'   For example, to center-align `Date` objects you could set `"huxtable.autoformat_align"` to
#'   something like `list(..., Date = "center", ...)`.
#' @name huxtable-options
NULL

.onLoad <- function(libname, pkgname) {
  options(
    huxtable.print                    = getOption('huxtable.print', print_screen),
    huxtable.knit_print_df            = getOption('huxtable.knit_print_df', TRUE),
    huxtable.knit_print_df_theme      = getOption('huxtable.knit_print_df_theme', theme_plain),
    huxtable.color_screen             = getOption('huxtable.color_screen', requireNamespace('crayon',
          quietly = TRUE)),
    huxtable.autoformat               = getOption('huxtable.autoformat', TRUE),
    huxtable.autoformat_number_format = getOption('huxtable.autoformat_number_format', list(
            integer = 0,
            numeric = "%.3g",
            complex = "%.3g"
          )),
    huxtable.autoformat_align         = getOption('huxtable.autoformat_align', list(
            numeric = getOption("OutDec", "."),
            complex = getOption("OutDec", "."),
            integer = "right",
            Date    = "right",
            POSIXct = "right",
            POSIXlt = "right"
          ))
  )

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


