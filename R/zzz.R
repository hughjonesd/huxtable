
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
#' @section Package options:
#'
#' * `options('huxtable.add_colnames')` sets the default value for `add_colnames` in
#'   [huxtable()] and [as_huxtable()]. If it is unset, `add_colnames` defaults to
#'   `FALSE`; in a future release, the default will become `TRUE`.
#' * `options('huxtable.print')` sets the print method for huxtable objects. See [print.huxtable()].
#' * `options('huxtable.color_screen')`. If `TRUE` and package `crayon` is available, huxtables
#'   will be printed in color on screen.
#' * `options('huxtable.knit_print_df')`. If `TRUE` (the default), data frames in knitr will be
#'   pretty-printed using huxtable.
#' * `options('huxtable.knit_print_df_theme')`. A one-argument function applied to theme the huxtableized
#'   data frame before printing in knitr. Defaults to [theme_plain()].
#'
#'
#' @name huxtable-package
NULL

# onLoad function ------------------------------------------------------------------------------------------------------

.onLoad <- function(libname, pkgname) {
  options(
    huxtable.print               = getOption('huxtable.print', print_screen),
    huxtable.knit_print_df       = getOption('huxtable.knit_print_df', TRUE),
    huxtable.knit_print_df_theme = getOption('huxtable.knit_print_df_theme', theme_plain),
    huxtable.color_screen        = getOption('huxtable.color_screen', requireNamespace('crayon', quietly = TRUE))
  )
}


