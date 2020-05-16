

# order of these matters
huxtable_border_props <- c(
  "top_border", "left_border", "right_border", "bottom_border",
  "top_border_color", "left_border_color", "right_border_color", "bottom_border_color",
  "top_border_style", "left_border_style", "right_border_style", "bottom_border_style"
)


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
for (val in paste0(c("left", "right", "top", "bottom"), "_border")) {
  make_getter_setters(val, "cell", check_fun = is.numeric, default = 0.4,
        only_set_map = TRUE)
}


# when left_border(ht)[i, j] <- value is called,
# first left_border(ht) is called and assigned to (say) lb
# `[<-`(lb, i, j, value) is then called. The result is (say) lb_new,
# which needs to represent the whole object.
# Finally, `left_border<-`(ht, value = lb_new) is called. The result
# is assigned to ht!
#
# Plan: `left_border` returns an appropriate matrix of thicknesses as now,
# but with a borderMatrix subclass.
# `[<-.borderMatrix` returns an object of class bdr (a list of 3 matrices)
# `left_border<-` copes with both bdr objects, and  matrices of
# thickness, since it is also sometimes called directly.
#
# left_border_style(ht)[1, 3]
# calls left_border_style, calls [<- on the result and then
# calls
# left_border_style<-. So we just make sure
# left_border_style<- and left_border_style talk to the appropriate bit of the
# 3 lists.


xxx_border_huxtable <- function(lr_tb, i, j, border_prop) {
  function (ht) {
    mlist <- attr(ht, lr_tb)

    last_col <- ncol(ht) + 1
    last_row <- nrow(ht) + 1
    m <- mlist$thickness[eval(i), eval(j), drop = FALSE]
    class(m) <- c("borderMatrix", class(m))

    m
  }
}


#' @export
left_border.huxtable <- xxx_border_huxtable("lr_borders",
      i = quote(seq_len(nrow(ht))), j = quote(-last_col))


#' @export
right_border.huxtable <- xxx_border_huxtable("lr_borders",
      i = quote(seq_len(nrow(ht))), j = quote(-1))


#' @export
top_border.huxtable <- xxx_border_huxtable("tb_borders",
      i = quote(-last_row), j = quote(seq_len(ncol(ht))))


#' @export
bottom_border.huxtable <- xxx_border_huxtable("tb_borders",
      i = quote(-1), j = quote(seq_len(ncol(ht))))


#' @export
#' @method `left_border<-` huxtable
#' @export `left_border<-.huxtable`
`left_border<-.huxtable` <- function (ht, value) {
  UseMethod("left_border<-.huxtable", value)
}


#' @export
#' @method `right_border<-` huxtable
#' @export `right_border<-.huxtable`
`right_border<-.huxtable` <- function (ht, value) {
  UseMethod("right_border<-.huxtable", value)
}


#' @export
#' @method `top_border<-` huxtable
#' @export `top_border<-.huxtable`
`top_border<-.huxtable` <- function (ht, value) {
  UseMethod("top_border<-.huxtable", value)
}


#' @export
#' @method `bottom_border<-` huxtable
#' @export `bottom_border<-.huxtable`
`bottom_border<-.huxtable` <- function (ht, value) {
  UseMethod("bottom_border<-.huxtable", value)
}



xxx_border_arrow_yyy_hux <- function(lr_tb, i, j, border_prop) {
  function(ht, value) {
    force(value)
    mlist <- attr(ht, lr_tb)
    last_col <- ncol(ht) + 1
    last_row <- nrow(ht) + 1
    mlist[[border_prop]][eval(i), eval(j)] <- value
    attr(ht, lr_tb) <- mlist

    ht
  }
}


#' @export
#' @method `left_border<-.huxtable` default
`left_border<-.huxtable.default` <- xxx_border_arrow_yyy_hux("lr_borders",
      quote(seq_len(nrow(ht))), quote(- last_col), "thickness")



#' @export
#' @method `right_border<-.huxtable` default
`right_border<-.huxtable.default` <- xxx_border_arrow_yyy_hux("lr_borders",
  quote(seq_len(nrow(ht))), quote(- 1), "thickness")



#' @export
#' @method `top_border<-.huxtable` default
`top_border<-.huxtable.default` <- xxx_border_arrow_yyy_hux("tb_borders",
      quote(- last_row), quote(seq_len(ncol(ht))), "thickness")



#' @export
#' @method `bottom_border<-.huxtable` default
`bottom_border<-.huxtable.default` <- xxx_border_arrow_yyy_hux("tb_borders",
      quote(- 1), quote(seq_len(ncol(ht))), "thickness")



#' @export
`left_border_style<-.huxtable` <- xxx_border_arrow_yyy_hux("lr_borders",
  i = quote(seq_len(nrow(ht))), j = quote(-last_col), "style")


#' @export
`right_border_style<-.huxtable` <- xxx_border_arrow_yyy_hux("lr_borders",
  i = quote(seq_len(nrow(ht))), j = quote(-1), "style")


#' @export
`top_border_style<-.huxtable` <- xxx_border_arrow_yyy_hux("tb_borders",
  i = quote(-last_row), j = quote(seq_len(ncol(ht))), "style")


#' @export
`bottom_border_style<-.huxtable` <- xxx_border_arrow_yyy_hux("tb_borders",
  i = quote(-1), j = quote(seq_len(ncol(ht))), "style")

#' @export
`left_border_color<-.huxtable` <- xxx_border_arrow_yyy_hux("lr_borders",
  i = quote(seq_len(nrow(ht))), j = quote(-last_col), "color")


#' @export
`right_border_color<-.huxtable` <- xxx_border_arrow_yyy_hux("lr_borders",
  i = quote(seq_len(nrow(ht))), j = quote(-1), "color")


#' @export
`top_border_color<-.huxtable` <- xxx_border_arrow_yyy_hux("tb_borders",
  i = quote(-last_row), j = quote(seq_len(ncol(ht))), "color")


#' @export
`bottom_border_color<-.huxtable` <- xxx_border_arrow_yyy_hux("tb_borders",
  i = quote(-1), j = quote(seq_len(ncol(ht))), "color")



xxx_border_arrow_yyy_hux_bdr <- function (lr_tb, i, j) {
  function (ht, value) {
    mlist <- attr(ht, lr_tb)
    last_col <- ncol(ht) + 1
    last_row <- nrow(ht) + 1
    mlist$thickness[eval(i), eval(j)] <- value$thickness
    mlist$color[eval(i), eval(j)]     <- value$color
    mlist$style[eval(i), eval(j)]     <- value$style
    attr(ht, lr_tb) <- mlist

    ht
  }
}


#' @export
#' @method `left_border<-.huxtable` bdr
`left_border<-.huxtable.bdr` <- xxx_border_arrow_yyy_hux_bdr("lr_borders",
        i = quote(seq_len(nrow(ht))), j = quote(-last_col))



#' @export
#' @method `right_border<-.huxtable` bdr
`right_border<-.huxtable.bdr` <- xxx_border_arrow_yyy_hux_bdr("lr_borders",
      i = quote(seq_len(nrow(ht))), j = quote(-1))


#' @export
#' @method `top_border<-.huxtable` bdr
`top_border<-.huxtable.bdr` <- xxx_border_arrow_yyy_hux_bdr("tb_borders",
      i = quote(-last_row), j = quote(seq_len(ncol(ht))))


#' @export
#' @method `bottom_border<-.huxtable` bdr
`bottom_border<-.huxtable.bdr` <- xxx_border_arrow_yyy_hux_bdr("tb_borders",
      i = quote(-1), j = quote(seq_len(ncol(ht))))



xxx_border_yyy_huxtable <- function(lr_tb, i, j, border_prop) {
  function (ht) {
    mlist <- attr(ht, lr_tb)
    last_col <- ncol(ht) + 1
    last_row <- nrow(ht) + 1
    mlist[[border_prop]][eval(i), eval(j), drop = FALSE]
  }
}



#' @export
left_border_style.huxtable <- xxx_border_yyy_huxtable("lr_borders",
  i = quote(seq_len(nrow(ht))), j = quote(-last_col), "style")


#' @export
right_border_style.huxtable <- xxx_border_yyy_huxtable("lr_borders",
  i = quote(seq_len(nrow(ht))), j = quote(-1), "style")


#' @export
top_border_style.huxtable <- xxx_border_yyy_huxtable("tb_borders",
  i = quote(-last_row), j = quote(seq_len(ncol(ht))), "style")


#' @export
bottom_border_style.huxtable <- xxx_border_yyy_huxtable("tb_borders",
  i = quote(-1), j = quote(seq_len(ncol(ht))), "style")

#' @export
left_border_color.huxtable <- xxx_border_yyy_huxtable("lr_borders",
  i = quote(seq_len(nrow(ht))), j = quote(-last_col), "color")


#' @export
right_border_color.huxtable <- xxx_border_yyy_huxtable("lr_borders",
  i = quote(seq_len(nrow(ht))), j = quote(-1), "color")


#' @export
top_border_color.huxtable <- xxx_border_yyy_huxtable("tb_borders",
  i = quote(-last_row), j = quote(seq_len(ncol(ht))), "color")


#' @export
bottom_border_color.huxtable <- xxx_border_yyy_huxtable("tb_borders",
  i = quote(-1), j = quote(seq_len(ncol(ht))), "color")


#' @export
#' @method `[<-` borderMatrix
#' @export `[<-.borderMatrix`
`[<-.borderMatrix` <- function (x, i, j, ..., value) {
  # x is the result of left_border() - a matrix of thicknesses,
  # nr x nc.
  # value is whatever the user is passing in like `left_border(ht) <- value`
  # The result of this call will be passed to `left_border<-`.
  # So it could be a `bdr()` object or a normal matrix.
  #
  # but this may also be called in the course of normal huxtable manipulation,
  # e.g. if a user does
  # borders <- left_border(ht) # gets a borderMatrix!
  # borders[1,1] <- 0
  # left_border(ht) <- borders
  UseMethod("[<-.borderMatrix", value)
}

#' @export
#' @method `[<-.borderMatrix` bdr
`[<-.borderMatrix.bdr` <- function (x, i, j, ..., value) {
  thickness <- x[] # subsetting unclasses
  thickness[i, j, ...] <- value$thickness
  style <- matrix("", nrow(x), ncol(x))
  style[i, j, ...] <- value$style
  color <- matrix("", nrow(x), ncol(x))
  color[i, j, ...] <- value$color

  new_bdr(
    thickness = thickness,
    style     = style,
    color     = color
  )
}

#' @export
#' @method `[<-.borderMatrix` default
`[<-.borderMatrix.default` <- function (x, i, j, ..., value) {
  thickness <- x[] # subsetting unclasses
  thickness[i, j, ...] <- value

  thickness
}



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
  "cell", only_set_map = TRUE)

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
  "cell", check_values = c("solid", "double", "dashed", "dotted"), only_set_map = TRUE)


