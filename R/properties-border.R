
#' Set borders
#'
#' These functions set borders between cells.
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
NULL


#' @name right_border
#' @rdname borders
NULL


#' @name top_border
#' @rdname borders
NULL


#' @name bottom_border
#' @rdname borders
NULL


for (val in paste0(c("left", "right", "top", "bottom"), "_border")) {
  make_getter_setters(val, "cell", check_fun = is_borderish, default = 0.4,
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
    dimnames(m) <- dimnames(ht)

    m
  }
}


#' @export
left_border.huxtable <- xxx_border_huxtable("lr_borders",
      i = quote(seq_len(nrow(ht))), j = quote(-last_col))


#' @export
right_border.huxtable <- xxx_border_huxtable("lr_borders",
      i = quote(seq_len(nrow(ht))), j = -1)


#' @export
top_border.huxtable <- xxx_border_huxtable("tb_borders",
      i = quote(-last_row), j = quote(seq_len(ncol(ht))))


#' @export
bottom_border.huxtable <- xxx_border_huxtable("tb_borders",
      i = -1, j = quote(seq_len(ncol(ht))))


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
      quote(seq_len(nrow(ht))), quote(-last_col), "thickness")



#' @export
#' @method `right_border<-.huxtable` default
`right_border<-.huxtable.default` <- xxx_border_arrow_yyy_hux("lr_borders",
      quote(seq_len(nrow(ht))), - 1, "thickness")



#' @export
#' @method `top_border<-.huxtable` default
`top_border<-.huxtable.default` <- xxx_border_arrow_yyy_hux("tb_borders",
      quote(-last_row), quote(seq_len(ncol(ht))), "thickness")



#' @export
#' @method `bottom_border<-.huxtable` default
`bottom_border<-.huxtable.default` <- xxx_border_arrow_yyy_hux("tb_borders",
      i = -1, j = quote(seq_len(ncol(ht))), "thickness")



#' @export
`left_border_style<-.huxtable` <- xxx_border_arrow_yyy_hux("lr_borders",
      i = quote(seq_len(nrow(ht))), j = quote(-last_col), "style")


#' @export
`right_border_style<-.huxtable` <- xxx_border_arrow_yyy_hux("lr_borders",
      i = quote(seq_len(nrow(ht))), j = -1, "style")


#' @export
`top_border_style<-.huxtable` <- xxx_border_arrow_yyy_hux("tb_borders",
      i = quote(-last_row), j = quote(seq_len(ncol(ht))), "style")


#' @export
`bottom_border_style<-.huxtable` <- xxx_border_arrow_yyy_hux("tb_borders",
      i = -1, j = quote(seq_len(ncol(ht))), "style")

#' @export
`left_border_color<-.huxtable` <- xxx_border_arrow_yyy_hux("lr_borders",
      i = quote(seq_len(nrow(ht))), j = quote(-last_col), "color")


#' @export
`right_border_color<-.huxtable` <- xxx_border_arrow_yyy_hux("lr_borders",
      i = quote(seq_len(nrow(ht))), j = -1, "color")


#' @export
`top_border_color<-.huxtable` <- xxx_border_arrow_yyy_hux("tb_borders",
      i = quote(-last_row), j = quote(seq_len(ncol(ht))), "color")


#' @export
`bottom_border_color<-.huxtable` <- xxx_border_arrow_yyy_hux("tb_borders",
      i = -1, j = quote(seq_len(ncol(ht))), "color")



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
      i = quote(seq_len(nrow(ht))), j = -1)


#' @export
#' @method `top_border<-.huxtable` bdr
`top_border<-.huxtable.bdr` <- xxx_border_arrow_yyy_hux_bdr("tb_borders",
      i = quote(-last_row), j = quote(seq_len(ncol(ht))))


#' @export
#' @method `bottom_border<-.huxtable` bdr
`bottom_border<-.huxtable.bdr` <- xxx_border_arrow_yyy_hux_bdr("tb_borders",
      i = -1, j = quote(seq_len(ncol(ht))))



xxx_border_yyy_huxtable <- function(lr_tb, i, j, border_prop) {
  function (ht) {
    mlist <- attr(ht, lr_tb)
    last_col <- ncol(ht) + 1
    last_row <- nrow(ht) + 1

    mx <- mlist[[border_prop]][eval(i), eval(j), drop = FALSE]
    dimnames(mx) <- dimnames(ht)

    mx
  }
}



#' @export
left_border_style.huxtable <- xxx_border_yyy_huxtable("lr_borders",
  i = quote(seq_len(nrow(ht))), j = quote(-last_col), "style")


#' @export
right_border_style.huxtable <- xxx_border_yyy_huxtable("lr_borders",
  i = quote(seq_len(nrow(ht))), j = -1, "style")


#' @export
top_border_style.huxtable <- xxx_border_yyy_huxtable("tb_borders",
  i = quote(-last_row), j = quote(seq_len(ncol(ht))), "style")


#' @export
bottom_border_style.huxtable <- xxx_border_yyy_huxtable("tb_borders",
  i = -1, j = quote(seq_len(ncol(ht))), "style")

#' @export
left_border_color.huxtable <- xxx_border_yyy_huxtable("lr_borders",
  i = quote(seq_len(nrow(ht))), j = quote(-last_col), "color")


#' @export
right_border_color.huxtable <- xxx_border_yyy_huxtable("lr_borders",
  i = quote(seq_len(nrow(ht))), j = -1, "color")


#' @export
top_border_color.huxtable <- xxx_border_yyy_huxtable("tb_borders",
  i = quote(-last_row), j = quote(seq_len(ncol(ht))), "color")


#' @export
bottom_border_color.huxtable <- xxx_border_yyy_huxtable("tb_borders",
  i = -1, j = quote(seq_len(ncol(ht))), "color")


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
  style <- matrix(huxtable_env$huxtable_default_attrs$border_style, nrow(x), ncol(x))
  style[i, j, ...] <- value$style
  color <- matrix(huxtable_env$huxtable_default_attrs$border_color, nrow(x), ncol(x))
  color[i, j, ...] <- value$color

  new_bdr(
    thickness = thickness,
    style     = style,
    color     = color
  )
}


#' @export
#' @method `[<-.borderMatrix` list
`[<-.borderMatrix.list` <- function (x, i, j, ..., value) {
  values_are_bdr <- sapply(value, is_bdr)
  if (! all(values_are_bdr)) stop("Unrecognized object ", value,
        " passed into border function.\nPass a number or a `bdr` object.")
  thickness <- x[] # subsetting unclasses
  new_dims <- dim(thickness[i, j, drop = FALSE])
  new_thickness <- sapply(value, getElement, name = "thickness")
  dim(new_thickness) <- new_dims
  new_style <- sapply(value, getElement, name = "style")
  dim(new_style) <- new_dims
  new_color <- sapply(value, getElement, name = "color")
  dim(new_color) <- new_dims

  value <- new_bdr(
          thickness = new_thickness,
          style     = new_style,
          color     = new_color
        )
  # NextMethod uses the class vector of the object supplied to the generic,
  # so we just invoke this manually:
  `[<-.borderMatrix`(x, i, j, ..., value = value)
}


#' @export
#' @method `[<-.borderMatrix` default
`[<-.borderMatrix.default` <- function (x, i, j, ..., value) {
  thickness <- x[] # subsetting unclasses
  thickness[i, j, ...] <- value

  thickness
}


for (val in paste0(c("left", "right", "top", "bottom"), "_border_color")) make_getter_setters(val,
  "cell", only_set_map = TRUE)


for (val in paste0(c("left", "right", "top", "bottom"), "_border_style")) make_getter_setters(val,
  "cell", check_values = c("solid", "double", "dashed", "dotted"), only_set_map = TRUE)


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
