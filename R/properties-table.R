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
  prop_set_table(ht, value, "position",
    check_values = c("left", "center", "centre", "right", "wrapleft", "wrapright"),
    extra = quote({
      value[value == "centre"] <- "center"
    })
  )
}

#' @rdname position
#' @export
set_position <- function(ht, value) {
  prop_set_table(ht, value, "position",
    check_values = c("left", "center", "centre", "right", "wrapleft", "wrapright"),
    extra = quote({
      value[value == "centre"] <- "center"
    })
  )
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
  prop_set_table(ht, value, "caption_pos",
    check_values = c(
      "top", "bottom", "topleft", "topcenter", "topcentre",
      "topright", "bottomleft", "bottomcenter", "bottomcentre", "bottomright"
    ),
    extra = quote({
      value[value == "topcentre"] <- "topcenter"
      value[value == "bottomcentre"] <- "bottomcenter"
    })
  )
}

#' @rdname caption_pos
#' @export
set_caption_pos <- function(ht, value) {
  prop_set_table(ht, value, "caption_pos",
    check_values = c(
      "top", "bottom", "topleft", "topcenter", "topcentre",
      "topright", "bottomleft", "bottomcenter", "bottomcentre", "bottomright"
    ),
    extra = quote({
      value[value == "topcentre"] <- "topcenter"
      value[value == "bottomcentre"] <- "bottomcenter"
    })
  )
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
  prop_set_table(ht, value, "caption_width", check_fun = is_numeric_or_character)
}

#' @rdname caption_width
#' @export
set_caption_width <- function(ht, value) {
  prop_set_table(ht, value, "caption_width", check_fun = is_numeric_or_character)
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
  prop_set_table(ht, value, "width", check_fun = is_numeric_or_character)
}

#' @rdname width
#' @export
set_width <- function(ht, value) {
  prop_set_table(ht, value, "width", check_fun = is_numeric_or_character)
}


#' Set the table background color
#'
#' `table_background_color()` sets a background color for the whole table.
#' Individual [background_color()] values override the table background.
#'
#' Colors can be specified as described in [background_color()]. The default,
#' `NA`, leaves the table background unset.
#'
#' Output formats handle table backgrounds differently:
#'
#' * HTML applies the color to the `<table>` element.
#' * LaTeX wraps non-breakable tables in a zero-padding `\colorbox`.
#'   Breakable LaTeX tables ignore the property because a `longtable` cannot be
#'   placed inside a `\colorbox`.
#' * Typst uses the table's `fill` setting. Individual cell fills override it.
#' * RTF, Excel, Word and PowerPoint output apply the table color to
#'   cells which do not have an individual background color.
#' * On-screen output applies the color to the whole table area, including
#'   borders. Individual cell backgrounds still override it.
#' * Markdown output ignores the property.
#'
#' @inherit hux_prop_params params return
#' @param value A string or `NA`. `r rd_default("table_background_color")`
#'
#' @family formatting functions
#'
#' @examples
#' set_table_background_color(jams, "grey95")
#'
#' @name table_background_color
NULL

#' @rdname table_background_color
#' @export
table_background_color <- function(ht) prop_get(ht, "table_background_color")

#' @rdname table_background_color
#' @export
`table_background_color<-` <- function(ht, value) {
  prop_set_table(ht, value, "table_background_color", check_fun = is.string)
}

#' @rdname table_background_color
#' @export
set_table_background_color <- function(ht, value) {
  prop_set_table(ht, value, "table_background_color", check_fun = is.string)
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
  prop_set_table(ht, value, "height", check_fun = is_numeric_or_character)
}

#' @rdname height
#' @export
set_height <- function(ht, value) {
  prop_set_table(ht, value, "height", check_fun = is_numeric_or_character)
}


#' Allow a table to break across pages
#'
#' `breakable()` controls whether a table may break between rows across pages.
#' Individual rows are kept together. It affects paged HTML, LaTeX, RTF, Typst,
#' and Word output; other output formats ignore it.
#'
#' In LaTeX, breakable tables use the `longtable` environment and therefore
#' require a one-column layout. They cannot have a fixed [height()] or use a
#' wrapping [position()].
#'
#' @inherit hux_prop_params params return
#' @param value Logical. `r rd_default("breakable")`
#'
#' @examples
#' set_breakable(jams, TRUE)
#'
#' @name breakable
NULL

#' @rdname breakable
#' @export
breakable <- function(ht) prop_get(ht, "breakable")

#' @rdname breakable
#' @export
`breakable<-` <- function(ht, value) {
  prop_set_table(ht, value, "breakable", check_fun = is.flag)
}

#' @rdname breakable
#' @export
set_breakable <- function(ht, value) {
  prop_set_table(ht, value, "breakable", check_fun = is.flag)
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
#'
#' Table captions set via the Quarto `tbl-cap` or `tbl-subcap` chunk options
#' override captions set by this mechanism. A warning is issued if both are set.
#'
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
  prop_set_table(ht, value, "caption", check_fun = is.string)
}

#' @rdname caption
#' @export
set_caption <- function(ht, value) {
  prop_set_table(ht, value, "caption", check_fun = is.string)
}


#' Set notes beneath a table
#'
#' Table notes are stored separately from the table's cells. They do not change
#' the number of rows in the huxtable. Each element of `value` is printed as a
#' separate note beneath the table.
#'
#' Use `NA` or `NULL` to remove all table notes.
#'
#' @inherit hux_prop_params params
#' @param value A character vector, or `NA`/`NULL` to remove all notes.
#' @return `table_notes()` returns a character vector. The replacement function,
#'   `set_table_notes()` and `add_table_note()` return the modified huxtable.
#'
#' @details
#' Table notes are plain text. Huxtable escapes them as required for each output
#' format.
#'
#' HTML, LaTeX, Typst and Word output use their native table-footer or table-note
#' structures. RTF, Markdown and screen output print notes immediately after the
#' table. Excel writes each note to a merged row below the table. Breakable LaTeX
#' tables use the `threeparttablex` package.
#'
#' Use [cell_note()] to add notes referenced from individual cells. Referenced
#' cell notes are printed after unreferenced table notes.
#'
#' [add_footnote()] is retained for compatibility only. It adds an ordinary,
#' full-width row to the table and is soft-deprecated in favour of
#' `add_table_note()`.
#'
#' @examples
#' ht <- set_table_notes(jams, "Note: Prices exclude delivery.")
#' ht <- add_table_note(ht, "Source: The jam growers' association.")
#' table_notes(ht)
#' table_notes(ht) <- NULL
#'
#' @name table_notes
NULL

#' @rdname table_notes
#' @export
table_notes <- function(ht) {
  notes <- prop_get(ht, "table_notes") %||% character()
  notes[!is.na(notes)]
}

#' @rdname table_notes
#' @export
`table_notes<-` <- function(ht, value) {
  # Table properties use a scalar NA as their internal empty value. Normalize
  # zero-length inputs because validate_prop() cannot replace them with a default.
  if (is.null(value) || length(value) == 0L || all(is.na(value))) {
    value <- NA_character_
  }
  prop_set_table(ht, value, "table_notes",
    check_fun = function(x) is.character(x) && !anyNA(x)
  )
}

#' @rdname table_notes
#' @export
set_table_notes <- function(ht, value) {
  # See the replacement method above for why empty input is normalized here.
  if (is.null(value) || length(value) == 0L || all(is.na(value))) {
    value <- NA_character_
  }
  prop_set_table(ht, value, "table_notes",
    check_fun = function(x) is.character(x) && !anyNA(x)
  )
}

#' @rdname table_notes
#' @export
add_table_note <- function(ht, value) {
  stopifnot(is.character(value), !anyNA(value))
  table_notes(ht) <- c(table_notes(ht), value)
  ht
}


#' Set the symbols used for cell note references
#'
#' `note_symbol` is a table property controlling marks for [cell_note()]. The
#' marks themselves are assigned when a huxtable is rendered rather than stored
#' in its cells, so subsetting does not modify cell-note metadata.
#'
#' @inherit hux_prop_params params
#' @param value One of `"numeric"`, `"roman"` or `"alphabetic"`, or a
#'   non-empty string whose individual characters form a custom symbol
#'   sequence. `r rd_default("note_symbol")`
#' @return `note_symbol()` returns a character string. The replacement function
#'   and `set_note_symbol()` return the modified huxtable.
#'
#' @details
#' Numeric symbols are `1`, `2`, ...; Roman symbols are `i`, `ii`, ...; and
#' alphabetic symbols are `a`, `b`, ..., `z`, `aa`, .... Custom strings are
#' treated like a custom alphabet. For example, `"+*"` produces `+`, `*`,
#' `++`, `+*`, `*+`, ....
#'
#' Custom Unicode symbols require support from the output format's font and
#' rendering engine.
#'
#' @examples
#' ht <- set_cell_note(jams, 1, 1, "Price estimated")
#' ht <- set_note_symbol(ht, "+*")
#' note_symbol(ht)
#'
#' @name note_symbol
NULL

#' @rdname note_symbol
#' @export
note_symbol <- function(ht) prop_get(ht, "note_symbol")

#' @rdname note_symbol
#' @export
`note_symbol<-` <- function(ht, value) {
  prop_set_table(ht, value, "note_symbol",
    check_fun = function(x) is.string(x) && nzchar(x)
  )
}

#' @rdname note_symbol
#' @export
set_note_symbol <- function(ht, value) {
  prop_set_table(ht, value, "note_symbol",
    check_fun = function(x) is.string(x) && nzchar(x)
  )
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
  prop_set_table(ht, value, "tabular_environment", check_fun = is.string)
}

#' @rdname tabular_environment
#' @export
set_tabular_environment <- function(ht, value) {
  prop_set_table(ht, value, "tabular_environment", check_fun = is.string)
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
  prop_set_table(ht, value, "table_environment", check_fun = is.string)
}

#' @rdname table_environment
#' @export
set_table_environment <- function(ht, value) {
  prop_set_table(ht, value, "table_environment", check_fun = is.string)
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
#' Within knitr, huxtable labels default to the knitr chunk label. If a chunk
#' prints more than one huxtable, later labels have `"-2"`, `"-3"` and so on
#' appended to make them unique. To turn off this behaviour, set
#' `options(huxtable.autolabel = FALSE)`.
#'
#' If you use \href{https://bookdown.org}{bookdown}, and set a label on your
#' table, the table [caption()] will automatically be prefixed with `(#label)`.
#' You can then refer to the table using `@ref(label)`. `label` needs to start
#' with `"tab:"`; if it doesn't, the `"tab:"` prefix will be added
#' automatically. To turn off this behaviour, set
#' `options(huxtable.bookdown = FALSE)`.
#'
#' Quarto table labels override labels set by this mechanism. A warning is
#' issued if both are set.
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
  prop_set_table(ht, value, "label", check_fun = is.string)
}

#' @rdname label
#' @export
set_label <- function(ht, value) {
  prop_set_table(ht, value, "label", check_fun = is.string)
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
  prop_set_table(ht, value, "latex_float", check_fun = is.string)
}

#' @rdname latex_float
#' @export
set_latex_float <- function(ht, value) {
  prop_set_table(ht, value, "latex_float", check_fun = is.string)
}
