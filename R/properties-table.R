
#' Set the table's position with respect to surrounding content
#'
#' Table position may be "left", "right" or "center". If you want text to
#' wrap around the table, use "wrapleft" or "wrapright".
#'
#' @template getset-table
#' @templateVar attr_name position
#' @templateVar attr_desc Table position
#' @templateVar value_param_desc
#' String. "left", "center", "right", "wrapleft" or "wrapright".
#' @details
#' `"wrapleft"` and `"wrapright"` position the table to the left or right, and allow text to
#' wrap around the table.
#'
#' @template getset-example
#' @templateVar attr_val "right"
#' @examples
#'
#' set_position(jams, "left")
#' set_position(jams, "right")
#' set_position(jams, "center")
#'
NULL
position <- function (ht) .prop_get(ht, "position")
`position<-` <- function (ht, value) {
  .prop_replace(ht, value, "position",
        check_values = c("left", "center", "centre", "right", "wrapleft", "wrapright"),
        extra = quote({ value[value == "centre"] <- "center" }))
}
set_position <- function (ht, value) {
  .prop_set_table(ht, value, "position",
        check_values = c("left", "center", "centre", "right", "wrapleft", "wrapright"),
        extra = quote({ value[value == "centre"] <- "center" }))
}
map_position <- function (ht, fn) {
  .prop_map_table(ht, fn, "position",
        check_values = c("left", "center", "centre", "right", "wrapleft", "wrapright"),
        extra = quote({ value[value == "centre"] <- "center" }))
}


#' Position the table's caption
#'
#' If `caption_pos` is "top" or "bottom", then the horizontal position ("left",
#' "center" or "right") will be determined by the huxtable"s [position()].
#'
#' @template getset-table
#' @templateVar attr_name caption_pos
#' @templateVar attr_desc Caption position
#' @templateVar value_param_desc
#' String: "top", "bottom", "topleft", "topcenter", "topright", "bottomleft", "bottomcenter" or "bottomright".
#'
#' @template getset-example
#' @templateVar attr_val "topleft"
#'
#' @family caption properties
#' @examples
#'
#' caption(jams) <- "Jam for sale"
#' jams
#' set_caption_pos(jams, "bottom")
#'
NULL
caption_pos <- function (ht) .prop_get(ht, "caption_pos")
`caption_pos<-` <- function (ht, value) {
  .prop_replace(ht, value, "caption_pos",
        check_values = c("top", "bottom", "topleft", "topcenter", "topcentre",
          "topright", "bottomleft", "bottomcenter", "bottomcentre", "bottomright"),
        extra = quote({
          value[value == "topcentre"] <- "topcenter"
          value[value == "bottomcentre"] <- "bottomcenter"
        }))
}
set_caption_pos <- function (ht, value) {
  .prop_set_table(ht, value, "caption_pos",
        check_values = c("top", "bottom", "topleft", "topcenter", "topcentre",
          "topright", "bottomleft", "bottomcenter", "bottomcentre", "bottomright"),
        extra = quote({
          value[value == "topcentre"] <- "topcenter"
          value[value == "bottomcentre"] <- "bottomcenter"
        }))
}
map_caption_pos <- function (ht, fn) {
  .prop_map_table(ht, fn, "caption_pos",
        check_values = c("top", "bottom", "topleft", "topcenter", "topcentre",
          "topright", "bottomleft", "bottomcenter", "bottomcentre", "bottomright"),
        extra = quote({
          value[value == "topcentre"] <- "topcenter"
          value[value == "bottomcentre"] <- "bottomcenter"
        }))
}



#' Set the width of the table caption
#'
#' A numeric widths is interpreted as a proportion of text width in LaTeX, or of
#' width of the containing element in HTML. A character width must be a valid
#' LaTeX or CSS dimension. The default, `NA`, makes the caption the same width
#' as the table.
#'
#' @template getset-table
#' @templateVar attr_name caption_width
#' @templateVar value_param_desc Number or string.
#'
#' @family caption properties
#'
#' @template getset-example
#' @templateVar attr_val 0.5
NULL
caption_width <- function (ht) .prop_get(ht, "caption_width")
`caption_width<-` <- function (ht, value) {
  .prop_replace(ht, value, "caption_width", check_fun = is_numeric_or_character)
}
set_caption_width <- function (ht, value) {
  .prop_set_table(ht, value, "caption_width", check_fun = is_numeric_or_character)
}
map_caption_width <- function (ht, fn) {
  .prop_map_table(ht, fn, "caption_width", check_fun = is_numeric_or_character)
}


#' Set the table width
#'
#' `width()` sets the width of the entire table, while [col_width()] sets the
#' width of individual columns. A numeric width is treated as a proportion of
#' f the surrounding block width (HTML) or text width (LaTeX). A character width
#' must be a valid CSS or LaTeX dimension.
#'
#' @template getset-table
#' @templateVar attr_name width
#' @templateVar value_param_desc A number or string.
#'
#' @template getset-example
#' @templateVar attr_val 0.8
#' @family table measurements
NULL
width <- function (ht) .prop_get(ht, "width")
`width<-` <- function (ht, value) {
  .prop_replace(ht, value, "width", check_fun = is_numeric_or_character)
}
set_width <- function (ht, value) {
  .prop_set_table(ht, value, "width", check_fun = is_numeric_or_character)
}
map_width <- function (ht, fn) {
  .prop_map_table(ht, fn, "width", check_fun = is_numeric_or_character)
}


#' Set the table height
#'
#' `height()`` sets the height of the entire table, while [row_height()] sets the
#' height of individual rows. A numeric height is treated as a proportion of
#' the containing block (HTML) or `\\textheight` (LaTeX). A character height
#' must be a valid CSS or LaTeX dimension.
#'
#' @template getset-table
#' @templateVar attr_name height
#' @templateVar value_param_desc A number or string.
#'
#' @family table measurements
#'
#' @template getset-example
#' @templateVar attr_val 0.4
NULL
height <- function (ht) .prop_get(ht, "height")
`height<-` <- function (ht, value) {
  .prop_replace(ht, value, "height", check_fun = is_numeric_or_character)
}
set_height <- function (ht, value) {
  .prop_set_table(ht, value, "height", check_fun = is_numeric_or_character)
}
map_height <- function (ht, fn) {
  .prop_map_table(ht, fn, "height", check_fun = is_numeric_or_character)
}


#' Set the table caption
#'
#' By default, captions are displayed above the table. You can change this
#' with [caption_pos()].
#' @template getset-table
#' @templateVar attr_name caption
#' @templateVar value_param_desc A string.
#'
#' @details
#' Captions are not escaped. See the example for a workaround.
#' @family caption properties
#'
#' @examples
#'
#' set_caption(jams, "Pots of jam for sale")
#' # escape caption characters:
#' caption(jams) <- sanitize(
#'       "Make $$$ with jam",
#'       type = "latex")
#'
NULL
caption <- function (ht) .prop_get(ht, "caption")
`caption<-` <- function (ht, value) {
  .prop_replace(ht, value, "caption", check_fun = is.string)
}
set_caption <- function (ht, value) {
  .prop_set_table(ht, value, "caption", check_fun = is.string)
}
map_caption <- function (ht, fn) {
  .prop_map_table(ht, fn, "caption", check_fun = is.string)
}


#' Set the table's tabular environment in LaTeX
#'
#' By default this is either `"tabular"` or `"tabularx"`.
#' @template getset-table
#' @templateVar attr_name tabular_environment
#' @templateVar value_param_desc A string.
#'
#' @details
#' No features are guaranteed to work if you set this to a non-default
#' value. Use at your own risk!
#'
#' @template getset-example
#' @templateVar attr_val "longtable"
NULL
tabular_environment <- function (ht) .prop_get(ht, "tabular_environment")
`tabular_environment<-` <- function (ht, value) {
  .prop_replace(ht, value, "tabular_environment", check_fun = is.string)
}
set_tabular_environment <- function (ht, value) {
  .prop_set_table(ht, value, "tabular_environment", check_fun = is.string)
}
map_tabular_environment <- function (ht, fn) {
  .prop_map_table(ht, fn, "tabular_environment", check_fun = is.string)
}


#' Set the "table" environment in LaTeX
#'
#' By default this is `"table"`.
#' @template getset-table
#' @templateVar attr_name table_environment
#' @templateVar value_param_desc A string.
#'
#' @details
#' No features are guaranteed to work if you set this to a non-default
#' value. Use at your own risk! In particular, you may need to set
#' [latex_float()] to a non-default value.
#'
#' If [position()] is set to `"wrapleft"` or `"wrapright"`, this
#' value is overridden.
#'
#' @template getset-example
#' @templateVar attr_val "table*"
NULL
table_environment <- function (ht) .prop_get(ht, "table_environment")
`table_environment<-` <- function (ht, value) {
  .prop_replace(ht, value, "table_environment", check_fun = is.string)
}
set_table_environment <- function (ht, value) {
  .prop_set_table(ht, value, "table_environment", check_fun = is.string)
}
map_table_environment <- function (ht, fn) {
  .prop_map_table(ht, fn, "table_environment", check_fun = is.string)
}


#' Set a table label for external referencing
#'
#' The label is used as the table's label in LaTeX, and as the "id" property
#' of the table element in HTML.
#'
#' @template getset-table
#' @templateVar attr_name label
#' @templateVar value_param_desc A string.
#'
#' @template getset-example
#' @templateVar attr_val "tab:mytable"
#' @seealso huxtable-options
#' @details
#' LaTeX table labels typically start with `"tab:"`.
#'
#' Within knitr, huxtable labels will default to the same as the knitr chunk label.
#' To turn off this behaviour, set `options(huxtable.autolabel = FALSE)`.
#'
#' If you use \href{https://bookdown.org}{bookdown}, and set a label on your
#' table, the table [caption()] will automatically be prefixed with `(#label)`.
#' You can then refer to the table using `@ref(label)`. `label` needs to start
#' with `"tab:"`; if it doesn't, the `"tab:"` prefix will be added
#' automatically. To turn off this behaviour, set
#' `options(huxtable.bookdown = FALSE)`.
#'
NULL
label <- function (ht) .prop_get(ht, "label")
`label<-` <- function (ht, value) {
  .prop_replace(ht, value, "label", check_fun = is.string)
}
set_label <- function (ht, value) {
  .prop_set_table(ht, value, "label", check_fun = is.string)
}
map_label <- function (ht, fn) {
  .prop_map_table(ht, fn, "label", check_fun = is.string)
}


#' Set the position of the table float in LaTeX
#'
#' Possible values include:
#' * "h": here
#' * "h!" definitely here
#' * "t" top of page
#' * "ht" here or at top of page
#' * "b" bottom of page
#' * "p" page of floats
#'
#' See LaTeX documentation for more details.
#'
#' @template getset-table
#' @templateVar attr_name latex_float
#' @templateVar value_param_desc A string.
#'
#' @template getset-example
#' @templateVar attr_val "b"
NULL
latex_float <- function (ht) .prop_get(ht, "latex_float")
`latex_float<-` <- function (ht, value) {
  .prop_replace(ht, value, "latex_float", check_fun = is.string)
}
set_latex_float <- function (ht, value) {
  .prop_set_table(ht, value, "latex_float", check_fun = is.string)
}
map_latex_float <- function (ht, fn) {
  .prop_map_table(ht, fn, "latex_float", check_fun = is.string)
}
