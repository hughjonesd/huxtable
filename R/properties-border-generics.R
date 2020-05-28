
for (val in paste0(c("left", "right", "top", "bottom"), "_border")) {
  make_getter_setters(val,
    "cell",
    check_fun = is_borderish,
    default = 0.4,
    only_set_map = TRUE
  )
}


for (val in paste0(c("left", "right", "top", "bottom"), "_border_color")) {
  make_getter_setters(val,
    "cell",
    only_set_map = TRUE
  )
}


for (val in paste0(c("left", "right", "top", "bottom"), "_border_style")) {
  make_getter_setters(val,
    "cell",
    check_values = c("solid", "double", "dashed", "dotted"),
    only_set_map = TRUE
  )
}


# order of these matters
border_props <- c(
  "top_border", "left_border", "right_border", "bottom_border",
  "top_border_color", "left_border_color", "right_border_color", "bottom_border_color",
  "top_border_style", "left_border_style", "right_border_style", "bottom_border_style"
)

border_setters <- paste0(border_props, "<-")
huxtable_border_df <- data.frame(
  name = border_props,
  side = gsub("^([a-z]+)_.*", "\\1", border_props),
  border_attr = gsub("^.*_([a-z]+)$", "\\1", border_props),
  getter = I(lapply(border_props, get, envir = getNamespace("huxtable"))),
  setter = I(lapply(border_setters, get, envir = getNamespace("huxtable"))),
  stringsAsFactors = FALSE
)

#' @evalNamespace make_exports(huxtable_border_df$name, with_map = TRUE)
NULL


#' Set borders
#'
#' These functions set borders between cells.
#'
#' @eval make_border_aliases("")
#'
#' @param ht A huxtable.
#' @param row A row specifier. See [rowspecs] for details.
#' @param col An optional column specifier.
#' @param value A numeric thickness or a [bdr()] object.
#' @param fn A mapping function. See [mapping-functions] for details.
#'
#' @details
#' Borders are always "collapsed": `right_border(ht)[, 1]` is
#' the same as `left_border(ht)[, 2]`, and setting one sets the other.
#'
#' Setting `left_border(ht) <- number` sets the border thickness.
#' You can set multiple properties at once by using [bdr()].
#'
#' Currently in LaTeX, all non-zero border widths on a given line must be the
#' same.
#'
#' @section Limitations:
#'
#' * In HTML, you will need to set a width of at least 3 to get a double border.
#' * Only "solid" and "double" styles are currently implemented in LaTeX, and
#'   all non-zero horizontal border widths on a given line must be the same.
#'
#'
#' @seealso [set-multiple]
#'
#' @family border properties
#'
#' @examples
#'
#' bottom_border(jams)[1, ] <- 0.4
#' jams
#'
#' bottom_border(jams)[1, ] <- bdr(0.4, "solid", "blue")
#' jams
#'
#' set_bottom_border(jams, bdr(0.4, "solid", "green"))
#'
#' @name borders
NULL


#' @name left_border
#' @rdname borders
#' @templateVar attr_name left_border
#' @templateVar default 0.4
#' @template cell-property-usage
NULL


#' @name right_border
#' @rdname borders
#' @templateVar attr_name right_border
#' @templateVar default 0.4
#' @template cell-property-usage
NULL


#' @name top_border
#' @rdname borders
#' @templateVar attr_name top_border
#' @templateVar default 0.4
#' @template cell-property-usage
NULL


#' @name bottom_border
#' @rdname borders
#' @templateVar attr_name bottom_border
#' @templateVar default 0.4
#' @template cell-property-usage
NULL


#' @name `left_border<-`
#' @rdname borders
NULL


#' @name `right_border<-`
#' @rdname borders
NULL


#' @name `top_border<-`
#' @rdname borders
NULL


#' @name `bottom_border<-`
#' @rdname borders
NULL


#' Set border colors
#'
#' These functions set border colors.
#'
#' @eval make_border_aliases("_color")
#'
#' @param ht A huxtable.
#' @param row A row specifier. See [rowspecs] for details.
#' @param col An optional column specifier.
#' @param value A valid R color, e.g. `"red"`, `"#FF0000"`.
#' @param fn A mapping function. See [mapping-functions] for details.
#'
#' @details
#' Borders are always "collapsed": `right_border_color(ht)[, 1]` is
#' the same as `left_border_color(ht)[, 2]`, and setting one sets the other.
#'
#' @section Limitations:
#'
#' * Transparent borders with the alpha channel set are not guaranteed to work.
#'
#' @seealso [set-multiple], [bdr()]
#'
#' @family border properties
#'
#' @examples
#'
#' jams <- set_all_borders(jams)
#' bottom_border_color(jams)[1, ] <- "red"
#' jams
#'
#' set_bottom_border_color(jams, "blue")
#'
#' @name border-colors
NULL


#' @name left_border_color
#' @rdname border-colors
#' @templateVar attr_name left_border_color
#' @template cell-property-usage
NULL


#' @name right_border_color
#' @rdname border-colors
#' @templateVar attr_name right_border_color
#' @template cell-property-usage
NULL


#' @name top_border_color
#' @rdname border-colors
#' @templateVar attr_name top_border_color
#' @template cell-property-usage
NULL


#' @name bottom_border_color
#' @rdname border-colors
#' @templateVar attr_name bottom_border_color
#' @template cell-property-usage
NULL


#' @name `left_border_color<-`
#' @rdname border-colors
NULL


#' @name `right_border_color<-`
#' @rdname border-colors
NULL


#' @name `top_border_color<-`
#' @rdname border-colors
NULL


#' @name `bottom_border_color<-`
#' @rdname border-colors
NULL



#' Set border styles
#'
#' These functions set border styles.
#'
#' @eval make_border_aliases("_style")
#'
#' @param ht A huxtable.
#' @param row A row specifier. See [rowspecs] for details.
#' @param col An optional column specifier.
#' @param value One of `"solid"`, `"double"`, `"dashed"` or `"dotted"`.
#' @param fn A mapping function. See [mapping-functions] for details.
#'
#' @details
#' Borders are always "collapsed": `right_border_style(ht)[, 1]` is
#' the same as `left_border_style(ht)[, 2]`, and setting one sets the other.
#'
#' @section Limitations:
#'
#' * In HTML, you will need to set a width of at least 3 to get a double border.
#' * Only "solid" and "double" styles are currently implemented in LaTeX.
#'
#' @seealso [set-multiple], [bdr()]
#'
#' @family border properties
#'
#' @examples
#'
#' jams <- set_all_borders(jams)
#' bottom_border_style(jams)[1, ] <- "dotted"
#' jams
#'
#' set_bottom_border_style(jams, "double")
#'
#' @name border-styles
NULL


#' @name left_border_style
#' @rdname border-styles
#' @templateVar attr_name left_border_style
#' @template cell-property-usage
NULL


#' @name right_border_style
#' @rdname border-styles
#' @templateVar attr_name right_border_style
#' @template cell-property-usage
NULL


#' @name top_border_style
#' @rdname border-styles
#' @templateVar attr_name top_border_style
#' @template cell-property-usage
NULL


#' @name bottom_border_style
#' @rdname border-styles
#' @templateVar attr_name bottom_border_style
#' @template cell-property-usage
NULL


#' @name `left_border_style<-`
#' @rdname border-styles
NULL


#' @name `right_border_style<-`
#' @rdname border-styles
NULL


#' @name `top_border_style<-`
#' @rdname border-styles
NULL


#' @name `bottom_border_style<-`
#' @rdname border-styles
NULL
