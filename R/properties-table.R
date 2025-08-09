#' Set the table's position with respect to surrounding content
#'
#' Table position may be "left", "right" or "center". If you want text to
#' wrap around the table, use "wrapleft" or "wrapright".
#'
#' @inherit hux_prop_params params return
#' @param value String. "left", "center", "right", "wrapleft" or "wrapright". `r rd_default("position")`
#' @details
#' `"wrapleft"` and `"wrapright"` position the table to the left or right, and allow text to
#' wrap around the table.
#'
#' @examples
#'
#' set_position(jams, "left")
#' set_position(jams, "right")
#' set_position(jams, "center")
#'
#' @name position
NULL

#' @rdname position
#' @export
position <- function(ht) prop_get(ht, "position")

#' @rdname position
#' @export
`position<-` <- function(ht, value) {
  if (!all(is.na(value))) {
    assert_that(all(na.omit(value) %in% c("left", "center", "centre", "right", "wrapleft", "wrapright")))
  }
  value[value == "centre"] <- "center"
  prop_replace(ht, value, "position")
}

#' @rdname position
#' @export
set_position <- function(ht, value) {
  if (!all(is.na(value))) {
    assert_that(all(na.omit(value) %in% c("left", "center", "centre", "right", "wrapleft", "wrapright")))
  }
  value[value == "centre"] <- "center"
  prop_set_table(ht, value, "position")
}


#' Position the table's caption
#'
#' If `caption_pos` is "top" or "bottom", then the horizontal position ("left",
#' "center" or "right") will be determined by the huxtable"s [position()].
#'
#' @inherit hux_prop_params params return
#' @param value String: "top", "bottom", "topleft", "topcenter", "topright", "bottomleft", "bottomcenter" or "bottomright". `r rd_default("caption_pos")`
#'
#' @family caption properties
#' @examples
#'
#' caption(jams) <- "Jam for sale"
#' jams
#' set_caption_pos(jams, "bottom")
#'
#' @name caption_pos
NULL

#' @rdname caption_pos
#' @export
caption_pos <- function(ht) prop_get(ht, "caption_pos")

#' @rdname caption_pos
#' @export
`caption_pos<-` <- function(ht, value) {
  if (!all(is.na(value))) {
    assert_that(all(na.omit(value) %in% c(
      "top", "bottom", "topleft", "topcenter", "topcentre",
      "topright", "bottomleft", "bottomcenter", "bottomcentre", "bottomright"
    )))
  }
  value[value == "topcentre"] <- "topcenter"
  value[value == "bottomcentre"] <- "bottomcenter"
  prop_replace(ht, value, "caption_pos")
}

#' @rdname caption_pos
#' @export
set_caption_pos <- function(ht, value) {
  if (!all(is.na(value))) {
    assert_that(all(na.omit(value) %in% c(
      "top", "bottom", "topleft", "topcenter", "topcentre",
      "topright", "bottomleft", "bottomcenter", "bottomcentre", "bottomright"
    )))
  }
  value[value == "topcentre"] <- "topcenter"
  value[value == "bottomcentre"] <- "bottomcenter"
  prop_set_table(ht, value, "caption_pos")
}



#' Set the width of the table caption
#'
#' A numeric widths is interpreted as a proportion of text width in LaTeX, or of
#' width of the containing element in HTML. A character width must be a valid
#' LaTeX or CSS dimension. The default, `NA`, makes the caption the same width
#' as the table.
#'
#' @inherit hux_prop_params params return
#' @param value Number or string. `r rd_default("caption_width")`
#'
#' @family caption properties
#'
#' @examples
#' set_caption_width(jams, 0.5)
#'
#' @name caption_width
NULL

#' @rdname caption_width
#' @export
caption_width <- function(ht) prop_get(ht, "caption_width")

#' @rdname caption_width
#' @export
`caption_width<-` <- function(ht, value) {
  if (!all(is.na(value))) {
    assert_that(is_numeric_or_character(value))
  }
  prop_replace(ht, value, "caption_width")
}

#' @rdname caption_width
#' @export
set_caption_width <- function(ht, value) {
  if (!all(is.na(value))) {
    assert_that(is_numeric_or_character(value))
  }
  prop_set_table(ht, value, "caption_width")
}


#' Set the table width
#'
#' `width()` sets the width of the entire table, while [col_width()] sets the
#' width of individual columns. A numeric width is treated as a proportion of
#' f the surrounding block width (HTML) or text width (LaTeX). A character width
#' must be a valid CSS or LaTeX dimension.
#'
#' @inherit hux_prop_params params return
#' @param value A number or string. `r rd_default("width")`
#'
#' @examples
#' set_width(jams, 0.8)
#'
#' @family table measurements
#' @name width
NULL

#' @rdname width
#' @export
width <- function(ht) prop_get(ht, "width")

#' @rdname width
#' @export
`width<-` <- function(ht, value) {
  if (!all(is.na(value))) {
    assert_that(is_numeric_or_character(value))
  }
  prop_replace(ht, value, "width")
}

#' @rdname width
#' @export
set_width <- function(ht, value) {
  if (!all(is.na(value))) {
    assert_that(is_numeric_or_character(value))
  }
  prop_set_table(ht, value, "width")
}


#' Set the table height
#'
#' `height()` sets the height of the entire table, while [row_height()] sets the
#' height of individual rows. A numeric height is treated as a proportion of
#' the containing block (HTML) or `\textheight` (LaTeX). A character height
#' must be a valid CSS or LaTeX dimension.
#'
#' @inherit hux_prop_params params return
#' @param value A number or string. `r rd_default("height")`
#'
#' @family table measurements
#'
#' @examples
#' set_height(jams, 0.4)
#'
#' @name height
NULL

#' @rdname height
#' @export
height <- function(ht) prop_get(ht, "height")

#' @rdname height
#' @export
`height<-` <- function(ht, value) {
  if (!all(is.na(value))) {
    assert_that(is_numeric_or_character(value))
  }
  prop_replace(ht, value, "height")
}

#' @rdname height
#' @export
set_height <- function(ht, value) {
  if (!all(is.na(value))) {
    assert_that(is_numeric_or_character(value))
  }
  prop_set_table(ht, value, "height")
}


#' Set the table caption
#'
#' By default, captions are displayed above the table. You can change this
#' with [caption_pos()].
#'
#' @inherit hux_prop_params params return
#' @param value A string. `r rd_default("caption")`
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
#'   "Make $$$ with jam",
#'   type = "latex"
#' )
#'
#' @name caption
NULL

#' @rdname caption
#' @export
caption <- function(ht) prop_get(ht, "caption")

#' @rdname caption
#' @export
`caption<-` <- function(ht, value) {
  if (!all(is.na(value))) {
    assert_that(is.string(value))
  }
  prop_replace(ht, value, "caption")
}

#' @rdname caption
#' @export
set_caption <- function(ht, value) {
  if (!all(is.na(value))) {
    assert_that(is.string(value))
  }
  prop_set_table(ht, value, "caption")
}


#' Set the table's tabular environment in LaTeX
#'
#' By default this is either `"tabular"` or `"tabularx"`.
#'
#' @inherit hux_prop_params params return
#' @param value A string. `r rd_default("tabular_environment")`
#'
#' @details
#' No features are guaranteed to work if you set this to a non-default
#' value. Use at your own risk!
#'
#' @examples
#' set_tabular_environment(jams, "longtable")
#'
#' @name tabular_environment
NULL

#' @rdname tabular_environment
#' @export
tabular_environment <- function(ht) prop_get(ht, "tabular_environment")

#' @rdname tabular_environment
#' @export
`tabular_environment<-` <- function(ht, value) {
  if (!all(is.na(value))) {
    assert_that(is.string(value))
  }
  prop_replace(ht, value, "tabular_environment")
}

#' @rdname tabular_environment
#' @export
set_tabular_environment <- function(ht, value) {
  if (!all(is.na(value))) {
    assert_that(is.string(value))
  }
  prop_set_table(ht, value, "tabular_environment")
}


#' Set the "table" environment in LaTeX
#'
#' By default this is `"table"`.
#'
#' @inherit hux_prop_params params return
#' @param value A string. `r rd_default("table_environment")`
#'
#' @details
#' No features are guaranteed to work if you set this to a non-default
#' value. Use at your own risk! In particular, you may need to set
#' [latex_float()] to a non-default value.
#'
#' If [position()] is set to `"wrapleft"` or `"wrapright"`, this
#' value is overridden.
#'
#' @examples
#' set_table_environment(jams, "table*")
#'
#' @name table_environment
NULL

#' @rdname table_environment
#' @export
table_environment <- function(ht) prop_get(ht, "table_environment")

#' @rdname table_environment
#' @export
`table_environment<-` <- function(ht, value) {
  if (!all(is.na(value))) {
    assert_that(is.string(value))
  }
  prop_replace(ht, value, "table_environment")
}

#' @rdname table_environment
#' @export
set_table_environment <- function(ht, value) {
  if (!all(is.na(value))) {
    assert_that(is.string(value))
  }
  prop_set_table(ht, value, "table_environment")
}


#' Set a table label for external referencing
#'
#' The label is used as the table's label in LaTeX, and as the "id" property
#' of the table element in HTML.
#'
#' @inherit hux_prop_params params return
#' @param value A string. `r rd_default("label")`
#'
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
#' @examples
#' set_label(jams, "tab:mytable")
#'
#' @name label
NULL

#' @rdname label
#' @export
label <- function(ht) prop_get(ht, "label")

#' @rdname label
#' @export
`label<-` <- function(ht, value) {
  if (!all(is.na(value))) {
    assert_that(is.string(value))
  }
  prop_replace(ht, value, "label")
}

#' @rdname label
#' @export
set_label <- function(ht, value) {
  if (!all(is.na(value))) {
    assert_that(is.string(value))
  }
  prop_set_table(ht, value, "label")
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
#' @inherit hux_prop_params params return
#' @param value A string. `r rd_default("latex_float")`
#'
#' @examples
#' set_latex_float(jams, "b")
#'
#' @name latex_float
NULL

#' @rdname latex_float
#' @export
latex_float <- function(ht) prop_get(ht, "latex_float")

#' @rdname latex_float
#' @export
`latex_float<-` <- function(ht, value) {
  if (!all(is.na(value))) {
    assert_that(is.string(value))
  }
  prop_replace(ht, value, "latex_float")
}

#' @rdname latex_float
#' @export
set_latex_float <- function(ht, value) {
  if (!all(is.na(value))) {
    assert_that(is.string(value))
  }
  prop_set_table(ht, value, "latex_float")
}
