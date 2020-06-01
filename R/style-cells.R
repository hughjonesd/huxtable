
#' Set multiple properties on headers
#'
#' These functions set arbitrary cell properties on cells in header rows
#' and/or columns.
#'
#' @inherit left_border params return
#' @param ... Named list of cell properties.
#'
#' @details
#'
#' * `style_headers` sets properties on all header cells.
#' * `style_header_rows` sets properties on header row cells.
#' * `style_header_cols` sets properties on header column cells.
#' * `style_cells` sets properties on all selected cells.
#'
#' @examples
#'
#' style_headers(jams, text_color = "red")
#' jams <- set_header_cols(jams, 1, TRUE)
#' style_header_cols(jams,
#'   text_color = c(NA, "red",
#'     "darkred", "purple")
#'   )
#'
#' style_cells(jams, everywhere, 2, bold = TRUE)
#' @name style-functions
NULL

#' @export
#' @rdname style-functions
style_headers <- function (ht, ...) {
  ht <- style_header_rows(ht, ...)
  ht <- style_header_cols(ht, ...)

  ht
}


#' @export
#' @rdname style-functions
style_header_rows <- function (ht, ...) {
  style_cells(ht, header_rows(ht), everywhere, ...)
}


#' @export
#' @rdname style-functions
style_header_cols <- function (ht, ...) {
  style_cells(ht, everywhere, header_cols(ht),  ...)
}


#' @export
#' @rdname style-functions
style_cells <- function (ht, row, col, ...) {
  props <- list(...)
  if (! all(names(props) %in% huxtable_cell_attrs)) stop("Unrecognized properties: ", paste(setdiff(names(props),
    huxtable_cell_attrs), collapse = ", "))
  call <- match.call(expand.dots = FALSE)
  call[["..."]] <- NULL
  call[["ht"]] <- quote(ht)
  for (prop_name in names(props)) {
    call[[1]] <- as.symbol(paste0("set_", prop_name))
    call$value <- props[[prop_name]]
    ht <- eval(call, list(ht = ht), parent.frame())
  }

  ht
}


#' @export
#' @rdname style-functions
#' @details
#' `set_cell_properties` is a deprecated alias for `style_cells`. Don't use it.
set_cell_properties <- style_cells

