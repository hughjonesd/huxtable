

#' @return An object you can pass into [number_format()].
#' @name fmt-numbers
NULL

#' Format numbers as percent
#'
#' @param digits How many digits to print.
#'
#' @inherit fmt-numbers return
#'
#' @export
#'
#' @family format functions
#'
#' @examples
#'
#' jams$Sugar <-c ("Sugar content",
#'       0.4, 0.35, 0.45)
#' set_number_format(jams, -1, "Sugar",
#'       fmt_percent(1))
#'
fmt_percent <- function (digits = 1) {
  list(function (x) {
    paste0(formatC(round(x * 100, digits), format = "f", digits = digits), "%")
  })
}


#' Use `prettyNum()` to format numbers
#'
#' @param ... Passed to [prettyNum()].
#'
#' @inherit fmt-numbers return
#'
#' @export
#'
#' @family format functions
#'
#' @examples
#'
#' jams$Sales <- c("Sales", 35000,
#'       55500, 20000)
#' set_number_format(jams, -1, "Sales",
#'       fmt_pretty(big.mark = ","))
#'
fmt_pretty <- function (...) {
  list(function (x) {
    prettyNum(x, ...)
  })
}


#' Format dates
#'
#' @param format Format string passed to [format.Date()]. See [strptime()] for
#'   details.
#' @param ... Other arguments to [format.Date()].
#'
#' @inherit fmt-numbers return
#'
#' @export
#'
#' @family format functions
#'
#' @examples
#' jams$best_before <- c("Best before",
#'       as.Date(c("2022-01-01", "2022-05-01", "2022-10-01")))
#' set_number_format(jams, -1 , "best_before", fmt_date())
fmt_date <- function (format = "%e %b %Y", ...) {
  list(function (x) {
    format(as.Date(x, origin = "1970-01-01"), format = format, ...)
  })
}


#' Format date-times
#'
#' @param format Format string passed to [format.POSIXct()].
#' @param ... Other arguments to [format.POSIXct()]
#'
#' @inherit fmt-numbers return
#'
#' @export
#'
#' @family format functions
#'
#' @examples
#' made_at <- as.POSIXct(
#'       c("2019-06-30 10:00:00",
#'       "2019-07-20 12:00:00",
#'       "2019-08-31 11:00:00"),
#'       format = "%Y-%m-%d %H:%M:%S")
#' jams$made_at <- c("Made at", made_at)
#' set_number_format(jams, -1, "made_at", fmt_datetime())
fmt_datetime <- function (format = "%e %b %Y %H:%M:%S", ...) {
  list(function (x) {
    format(as.POSIXct(x, origin = "1970-01-01"), format = format, ...)
  })
}

