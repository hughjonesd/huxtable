

# when left_border(ht)[i, j] <- value is called,
# first left_border(ht) is called and assigned to (say) lb.
# `[<-`(lb, i, j, value) is then called. The result is (say) lb_new,
# which needs to represent the whole object.
# Finally, `left_border<-`(ht, value = lb_new) is called. The result
# is assigned to ht!
#
# Plan: `left_border` returns a brdr() object.
# `[<-.brder` returns another brdr.
# `left_border<-` copes with both brdr objects, and matrices of
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
    thickness <- mlist$thickness[eval(i), eval(j), drop = FALSE]
    style <- mlist$style[eval(i), eval(j), drop = FALSE]
    color <- mlist$color[eval(i), eval(j), drop = FALSE]

    res <- new_brdr(
      thickness = thickness,
      style     = style,
      color     = color
    )
    dimnames(res) <- dimnames(ht)

    res
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
  prop_name_default <- switch(border_prop,
          "thickness" = "border",
          "color"     = "border_color",
          "style"     = "border_style"
        )

  function(ht, value) {
    force(value)
    value[is.na(value)] <- huxtable_env$huxtable_default_attrs[[prop_name_default]]

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



xxx_border_arrow_yyy_hux_brdr <- function (lr_tb, i, j) {
  function (ht, value) {
    mlist <- attr(ht, lr_tb)
    last_col <- ncol(ht) + 1
    last_row <- nrow(ht) + 1

    value$thickness[is.na(value$thickness)] <-
          huxtable_env$huxtable_default_attrs[["border"]]
    value$color[is.na(value$color)] <-
          huxtable_env$huxtable_default_attrs[["border_color"]]
    value$style[is.na(value$style)] <-
          huxtable_env$huxtable_default_attrs[["border_style"]]

    mlist$thickness[eval(i), eval(j)] <- value$thickness
    mlist$color[eval(i), eval(j)]     <- value$color
    mlist$style[eval(i), eval(j)]     <- value$style
    attr(ht, lr_tb) <- mlist

    ht
  }
}


#' @export
#' @method `left_border<-.huxtable` brdr
`left_border<-.huxtable.brdr` <- xxx_border_arrow_yyy_hux_brdr("lr_borders",
        i = quote(seq_len(nrow(ht))), j = quote(-last_col))


#' @export
#' @method `right_border<-.huxtable` brdr
`right_border<-.huxtable.brdr` <- xxx_border_arrow_yyy_hux_brdr("lr_borders",
      i = quote(seq_len(nrow(ht))), j = -1)


#' @export
#' @method `top_border<-.huxtable` brdr
`top_border<-.huxtable.brdr` <- xxx_border_arrow_yyy_hux_brdr("tb_borders",
      i = quote(-last_row), j = quote(seq_len(ncol(ht))))


#' @export
#' @method `bottom_border<-.huxtable` brdr
`bottom_border<-.huxtable.brdr` <- xxx_border_arrow_yyy_hux_brdr("tb_borders",
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
