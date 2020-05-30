
#' Set padding
#'
#' These functions set the space around the edges of cells, within the borders.
#'
#' @eval make_border_aliases("padding")
#'
#' @template property-params
#' @param value Numeric: padding width/height in points.

#' @seealso [set-multiple], [set-outer].
#'
#' @examples
#'
#' left_padding(jams) <- 2
#' left_padding(jams)
#'
#' jams <- set_left_padding(jams, 2)
#' left_padding(jams)
#'
#' @name padding
NULL
for (val in paste0(c("left", "right", "top", "bottom"), "_padding")) make_getter_setters(val, "cell")


#' @name left_padding
#' @rdname padding
#' @templateVar attr_name left_padding
#' @template cell-property-usage
NULL

#' @name right_padding
#' @rdname padding
#' @templateVar attr_name right_padding
#' @template cell-property-usage
NULL


#' @name top_padding
#' @rdname padding
#' @templateVar attr_name top_padding
#' @template cell-property-usage
NULL


#' @name bottom_padding
#' @rdname padding
#' @templateVar attr_name bottom_padding
#' @template cell-property-usage
NULL
