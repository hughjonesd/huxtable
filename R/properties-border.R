

# when left_border(ht)[i, j] <- value is called,
# first left_border(ht) is called and assigned to (say) lb.
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
#' @method left_border<- huxtable
#' @export `left_border<-.huxtable`
#' @rdname borders
`left_border<-.huxtable` <- function (ht, value) {
  UseMethod("left_border<-.huxtable", value)
}


#' @export
#' @method right_border<- huxtable
#' @export `right_border<-.huxtable`
#' @rdname borders
`right_border<-.huxtable` <- function (ht, value) {
  UseMethod("right_border<-.huxtable", value)
}


#' @export
#' @method top_border<- huxtable
#' @export `top_border<-.huxtable`
#' @rdname borders
`top_border<-.huxtable` <- function (ht, value) {
  UseMethod("top_border<-.huxtable", value)
}


#' @export
#' @method bottom_border<- huxtable
#' @export `bottom_border<-.huxtable`
#' @rdname borders
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


#' Internal function
#'
#' This is used to manage border properties. Do not call it directly.
#' @usage
#' \method{[}{borderMatrix}(x, ...) <- value
#'
#' @param x A `borderMatrix` object.
#' @param ... Indices.
#' @param value A [bdr()] object, number, matrix, or list.
#'
#' @return A [bdr()] object.
#'
#' @export
#' @method `[<-` borderMatrix
#' @export `[<-.borderMatrix`
`[<-.borderMatrix` <- function (x, ..., value) {
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
`[<-.borderMatrix.bdr` <- function (x, ..., value) {
  thickness <- x[] # subsetting unclasses
  thickness[...] <- value$thickness
  style <- matrix(huxtable_env$huxtable_default_attrs$border_style, nrow(x), ncol(x))
  style[...] <- value$style
  color <- matrix(huxtable_env$huxtable_default_attrs$border_color, nrow(x), ncol(x))
  color[...] <- value$color

  new_bdr(
    thickness = thickness,
    style     = style,
    color     = color
  )
}


#' @export
#' @method `[<-.borderMatrix` list
`[<-.borderMatrix.list` <- function (x, ..., value) {
  values_are_bdr <- sapply(value, is_bdr)
  if (! all(values_are_bdr)) stop("Unrecognized object ", value,
        " passed into border function.\nPass a number or a `bdr` object.")
  thickness <- x[] # subsetting unclasses
  new_dims <- dim(thickness[..., drop = FALSE])
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
  `[<-.borderMatrix`(x, ..., value = value)
}


#' @export
#' @method `[<-.borderMatrix` default
`[<-.borderMatrix.default` <- function (x, ..., value) {
  thickness <- x[] # subsetting unclasses
  thickness[...] <- value

  thickness
}


