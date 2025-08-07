#' Count characters or width of strings
#'
#' Uses stringi to ensure consistent character width calculations.
#'
#' @noRd
ncharw <- function(x, type = "width") {
  if (requireNamespace("crayon", quietly = TRUE)) {
    x <- crayon::strip_style(x)
  }
  # we use stringi throughout to keep the same concept of width
  switch(type,
    width = stringi::stri_width(x),
    chars = stringi::stri_length(x),
    stop("Unrecognized type in ncharw")
  )
}


#' Replace elements with blanks where a condition is TRUE
#'
#' @noRd
blank_where <- function(text, cond) {
  stopifnot(length(text) == length(cond))
  text[cond] <- ""
  text
}


#' Repeat strings elementwise
#'
#' @noRd
str_rep <- function(x, times) {
  mapply(function(s, t) paste0(rep(s, t), collapse = ""), x, times)
}
