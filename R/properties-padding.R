

#' @name left_padding
#' @template getset-cell
#' @templateVar attr_name left_padding
#' @templateVar attr_desc Cell padding
#' @templateVar value_param_desc
#' A vector or matrix. Characters must be valid CSS or LaTeX lengths. Numbers will be interpreted as lengths in points.
#' @templateVar morealiases right_padding top_padding bottom_padding
#' @template getset-example
#' @templateVar attr_val 20
#' @template getset-rowspec-example
#' @templateVar attr_val2 10
NULL
for (val in paste0(c("left", "right", "top", "bottom"), "_padding")) make_getter_setters(val, "cell")


#' @name right_padding
#' @rdname left_padding
#' @return Similarly for the other functions.
#' @templateVar attr_name right_padding
#' @template cell-property-usage
NULL


#' @name top_padding
#' @rdname left_padding
#' @templateVar attr_name top_padding
#' @template cell-property-usage
NULL


#' @name bottom_padding
#' @rdname left_padding
#' @templateVar attr_name bottom_padding
#' @template cell-property-usage
NULL
