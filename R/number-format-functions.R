#' @description
#' `fmt_` functions are designed to work with [number_format()].
#'
#' @return An object you can pass into [number_format()].
#' @name fmt-numbers
NULL

#' Format numbers as percent
#'
#'
#' @param digits How many digits to print.
#' @param format,... Passed into [formatC()].
#'
#' @inherit fmt-numbers description return
#'
#' @export
#'
#' @family format functions
#'
#' @examples
#'
#' jams$Sugar <- c(
#'   "Sugar content",
#'   0.4, 0.35, 0.45
#' )
#' set_number_format(
#'   jams, -1, "Sugar",
#'   fmt_percent(1)
#' )
#'
fmt_percent <- function(digits = 1, format = "f", ...) {
  list(function(x) {
    paste0(formatC(round(x * 100, digits), format = format, ..., digits = digits), "%")
  })
}


#' Use `prettyNum()` to format numbers
#'
#' @param big.mark,scientific,... Passed to [prettyNum()].
#'
#' @inherit fmt-numbers return
#'
#' @export
#'
#' @family format functions
#'
#' @examples
#'
#' jams$Sales <- c(
#'   "Sales", 35000,
#'   55500, 20000
#' )
#' set_number_format(
#'   jams, -1, "Sales",
#'   fmt_pretty()
#' )
#'
fmt_pretty <- function(big.mark = ",", ..., scientific = FALSE) {
  list(function(x) {
    prettyNum(x, big.mark = big.mark, scientific = scientific, ...)
  })
}
