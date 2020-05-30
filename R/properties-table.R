
#' @template getset-table
#' @templateVar attr_name position
#' @templateVar attr_desc Table position
#' @templateVar value_param_desc
#' A length-one character vector which may be "left", "center", "right", "wrapleft" or "wrapright".
#' @details
#' `"wrapleft"` and `"wrapright"` position the table to the left or right, and allow text to
#' wrap around the table.
#'
#' If your tables are too far to the right under LaTeX, try setting their [width()]
#' explicitly.
#' @template getset-example
#' @templateVar attr_val "right"
NULL
make_getter_setters("position", "table", check_values = c("left", "center", "centre", "right",
  "wrapleft", "wrapright"),
  extra_code = value[value == "centre"] <- "center")


#' @template getset-table
#' @templateVar attr_name caption_pos
#' @templateVar attr_desc Caption position
#' @templateVar value_param_desc
#' A length-one character vector, one of "top", "bottom", "topleft", "topcenter", "topright", "bottomleft", "bottomcenter", "bottomright".
#' @details
#' If `caption_pos` is "top" or "bottom", then the horizontal position ("left", "center" or "right")
#' will be determined by the huxtable"s [position()].
#' @family caption properties
#' @examples
#' caption(jams) <- "Price list"
#' jams
#' caption_pos(jams) <- "top"
#' jams
NULL
make_getter_setters("caption_pos", "table", check_values = c("top", "bottom", "topleft", "topcenter", "topcentre",
  "topright", "bottomleft", "bottomcenter", "bottomcentre", "bottomright"), extra_code = {
    value[value == "topcentre"] <- "topcenter"
    value[value == "bottomcentre"] <- "bottomcenter"
  })



#' @template getset-table
#' @templateVar attr_name caption_width
#' @templateVar attr_desc Caption width
#' @templateVar value_param_desc
#' A length-one numeric or character. Numerics are interpreted as proportions of text width in LaTeX, or of table width in HTML. If `NA`, the caption will be set to the same width as the table.
#'
#' @family caption properties
NULL
make_getter_setters("caption_width", "table")


#' @template getset-table
#' @templateVar attr_name width
#' @templateVar attr_desc Table width
#' @templateVar value_param_desc
#' A length-one vector. If numeric, `value` is treated as a proportion of the surrounding block width (HTML) or text width (LaTeX). If character, it must be a valid CSS or LaTeX width.
#' @template getset-example
#' @templateVar attr_val 0.8
#' @family table measurements
NULL
make_getter_setters("width", "table")


#' @template getset-table
#' @templateVar attr_name height
#' @templateVar attr_desc Table height
#' @templateVar value_param_desc
#' A length-one vector. If numeric, it is treated as a proportion of the containing block height for HTML, or of text height (`\\textheight`) for LaTeX. If character, it must be a valid CSS or LaTeX width.
#' @template getset-example
#' @templateVar attr_val 0.4
#' @family table measurements
NULL
make_getter_setters("height", "table")

#' @template getset-table
#' @templateVar attr_name caption
#' @templateVar attr_desc Caption
#' @templateVar value_param_desc
#' A length-one character vector.
#' @details
#' Captions are not escaped. See the example for a workaround.
#' @template getset-example
#' @templateVar attr_val "An example table"
#' @templateVar extra jams
#' @family caption properties
#' @examples
#'
#' # escape caption characters:
#' caption(jams) <- sanitize(
#'       "Make $$$ with jam",
#'       type = "latex")
NULL
make_getter_setters("caption", "table", check_fun = is.character)


#' @template getset-table
#' @templateVar attr_name tabular_environment
#' @templateVar attr_desc Tabular environment
#' @templateVar value_param_desc
#' A length-one character vector.
#' @template getset-example
#' @templateVar attr_val "longtable"
#' @details No features are guaranteed to work if you set this to a non-default value. Use at your own risk!
NULL
make_getter_setters("tabular_environment", "table", check_fun = is.character)


#' @template getset-table
#' @templateVar attr_name label
#' @templateVar attr_desc Table label
#' @templateVar value_param_desc
#' A length-one character vector to be used as a table label in LaTeX, or as an ID for the table in HTML.
#' @template getset-example
#' @templateVar attr_val "tab:mytable"
#' @seealso huxtable-options
#' @details
#' LaTeX table labels typically start with `"tab:"`.
#'
#' Within knitr, labels will default to the chunk label. Multiple huxtables
#' within a single chunk will be numbered sequentially, like
#' `chunk, chunk-1, chunk-2, ...`. To turn off this behaviour, set
#' `options(huxtable.autolabel = FALSE)`.
#'
#' If you use \href{http://bookdown.org}{bookdown}, and set a label on your table,
#' the caption will automatically be prefixed with `(#label)`. You can then
#' refer to the table using `@ref(label)`. `label` needs to start with `"tab:"`; if it doesn't,
#' the `"tab"` prefix will be added automatically. To turn off this behaviour,
#' set `options(huxtable.bookdown = FALSE)`.
NULL
make_getter_setters("label", "table", check_fun = is.character)


#' @template getset-table
#' @templateVar attr_name latex_float
#' @templateVar attr_desc Float position for LaTeX
#' @templateVar value_param_desc
#' A length-one character vector, used by LaTeX for positioning the float.
#' @template getset-example
#' @templateVar attr_val "b"
#' @details
#' Quick reference: "h" here, "h!" definitely here, "t" top of page, "ht" here
#' or at top, "b" bottom of page, "p" page of floats. See LaTeX documentation
#' for more details.
NULL
make_getter_setters("latex_float", "table", check_fun = is.character)
