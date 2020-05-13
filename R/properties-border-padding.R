
#' @template getset-cell
#' @templateVar attr_name left_border
#' @templateVar attr_desc Borders
#' @templateVar value_param_desc A numeric vector or matrix giving border widths in points. Set to 0 for no border.
#' @templateVar morealiases right_border top_border bottom_border
#' @templateVar default 0.4
#' @details
#' Currently in LaTeX, all non-zero border widths on a given line must be the
#' same.
#'
#' @seealso [set_all_borders()]
#' @template getset-example
#' @templateVar attr_val 1
#' @templateVar extra jams
#' @templateVar attr_val2 0
#' @template getset-visible-rowspec-example
#' @template border-warning
NULL
for (val in paste0(c("left", "right", "top", "bottom"), "_border")) make_getter_setters(val, "cell",
  check_fun = is.numeric, default = 0.4)


#' @name right_border
#' @rdname left_border
#' @return Similarly for the other functions.
#' @templateVar attr_name right_border
#' @templateVar default 0.4
#' @template cell-property-usage
NULL


#' @name top_border
#' @rdname left_border
#' @templateVar attr_name top_border
#' @templateVar default 0.4
#' @template cell-property-usage
NULL


#' @name bottom_border
#' @rdname left_border
#' @templateVar attr_name bottom_border
#' @templateVar default 0.4
#' @template cell-property-usage
NULL


#' @template getset-cell
#' @templateVar attr_name left_border_color
#' @templateVar attr_desc Border colors
#' @templateVar value_param_desc A vector or matrix of colors.
#' @templateVar morealiases right_border_color top_border_color bottom_border_color
#' @templateVar attr_val "red"
#' @details
#' Huxtable collapses borders and border colors. Right borders take priority over left borders, and
#' top borders take priority over bottom borders.
#'
#' @seealso [set_all_border_colors()]
#' @templateVar attr_val2 "blue"
#' @examples
#' ht <- huxtable(a = 1:3, b = 3:1)
#' ht <- set_all_borders(ht, 1)
#' set_left_border_color(ht, "red")
#' set_left_border_color(ht,
#'       1:2, 1, "red")
#'
#' @template border-warning
#'
NULL
for (val in paste0(c("left", "right", "top", "bottom"), "_border_color")) make_getter_setters(val,
  "cell")


#' @name right_border_color
#' @rdname left_border_color
#' @return Similarly for the other functions.
#' @templateVar attr_name right_border_color
#' @template cell-property-usage
NULL


#' @name top_border_color
#' @rdname left_border_color
#' @templateVar attr_name top_border_color
#' @template cell-property-usage
NULL


#' @name bottom_border_color
#' @rdname left_border_color
#' @templateVar attr_name bottom_border_color
#' @template cell-property-usage
NULL


#' @templateVar attr_name left_border_style
#' @templateVar attr_desc Border styles
#' @templateVar value_param_desc A character vector or matrix of styles, which may be "solid", "double", "dashed" or "dotted".
#' @templateVar morealiases right_border_style top_border_style bottom_border_style
#' @templateVar attr_val "solid"
#' @template getset-cell
#' @details
#' Huxtable collapses borders and border colors. Right borders take priority over left borders, and
#' top borders take priority over bottom borders.
#'
#' Border styles only apply if the border width is greater than 0.
#'
#' @section Quirks:
#'
#' * In HTML, you will need to set a width of at least 3 to get a double border.
#' * Only "solid" and "double" styles are currently implemented in LaTeX.
#'
#' @templateVar attr_val2 "double"
#' @examples
#' ht <- huxtable(a = 1:3, b = 3:1)
#' ht <- set_all_borders(ht, 1)
#' set_left_border_style(ht, "double")
#' set_left_border_style(ht, 1:2, 1,
#'       "double")
#'
#' @template border-warning
NULL
for (val in paste0(c("left", "right", "top", "bottom"), "_border_style")) make_getter_setters(val,
  "cell", check_values = c("solid", "double", "dashed", "dotted"))


#' @name right_border_style
#' @rdname left_border_style
#' @return Similarly for the other functions.
#' @templateVar attr_name right_border_style
#' @template cell-property-usage
NULL


#' @name top_border_style
#' @rdname left_border_style
#' @templateVar attr_name top_border_style
#' @template cell-property-usage
NULL


#' @name bottom_border_style
#' @rdname left_border_style
#' @templateVar attr_name bottom_border_style
#' @template cell-property-usage
NULL


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
