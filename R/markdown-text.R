

#' Create markdown text
#'
#' @param x A character vector.
#'
#' @return Markdown text.
#' @export
md <- function (x, ...) {
  class(x) <- c("md", class(x))
  x
}
