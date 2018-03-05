
#' Huxtable: simply create LaTeX and HTML tables
#'
#' Huxtable is a package for creating HTML and LaTeX tables. It provides similar
#' functionality to xtable, with a simpler interface.
#'
#' @details
#'
#' To create a huxtable object, use \code{\link{huxtable}} or \code{\link{as_huxtable}}.
#'
#' For more information, see \href{https://hughjonesd.github.io/huxtable/}{the website} or
#' read the vignette with \code{vignette('huxtable')}.
#'
#' @section Package options:
#'
#' \itemize{
#'   \item \code{options('huxtable.print')} sets the print method for huxtable objects. See \code{\link{print.huxtable}}.
#'   \item \code{options('huxtable.color_screen')}. If \code{TRUE} and package \code{crayon} is available, huxtables
#'   will be printed in color on screen.
#'
#'   \item \code{options('huxtable.knit_print_df')}. If \code{TRUE} (the default), data frames in knitr will be
#'   pretty-printed using huxtable.
#'
#'   \item \code{options('huxtable.knit_print_df_theme')}. A one-argument function applied to theme the huxtableized
#'   data frame before printing in knitr. Defaults to \code{\link{theme_plain}}.
#' }
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
