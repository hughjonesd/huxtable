# when left_border(ht)[i, j] <- value is called,
# first left_border(ht) is called and assigned to (say) lb.
# `[<-`(lb, i, j, value) is then called. The result is (say) lb_new,
# which needs to represent the whole object.
# Finally, `left_border<-`(ht, value = lb_new) is called. The result
# is assigned to ht!
#
# Plan: `left_border` returns a brdr() object.
# `[<-.brdr` returns another brdr.
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
    style     <- mlist$style[eval(i), eval(j), drop = FALSE]
    color     <- mlist$color[eval(i), eval(j), drop = FALSE]

    res <- new_brdr(
      thickness = thickness,
      style     = style,
      color     = color
    )
    dimnames(res) <- dimnames(ht)

    res
  }
}

xxx_border_arrow_yyy_hux <- function(lr_tb, i, j, border_prop) {
  prop_name_default <- switch(border_prop,
          thickness = "border",
          color     = "border_color",
          style     = "border_style"
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

## getter helpers -----------------------------------------------------------

#' @export
left_border <- xxx_border_huxtable("lr_borders",
      i = quote(seq_len(nrow(ht))), j = quote(-last_col))

#' @export
right_border <- xxx_border_huxtable("lr_borders",
      i = quote(seq_len(nrow(ht))), j = -1)

#' @export
top_border <- xxx_border_huxtable("tb_borders",
      i = quote(-last_row), j = quote(seq_len(ncol(ht))))

#' @export
bottom_border <- xxx_border_huxtable("tb_borders",
      i = -1, j = quote(seq_len(ncol(ht))))

## setter helpers -----------------------------------------------------------

left_border_set_default <- xxx_border_arrow_yyy_hux("lr_borders",
      quote(seq_len(nrow(ht))), quote(-last_col), "thickness")
left_border_set_brdr <- xxx_border_arrow_yyy_hux_brdr("lr_borders",
      quote(seq_len(nrow(ht))), quote(-last_col))

right_border_set_default <- xxx_border_arrow_yyy_hux("lr_borders",
      quote(seq_len(nrow(ht))), -1, "thickness")
right_border_set_brdr <- xxx_border_arrow_yyy_hux_brdr("lr_borders",
      quote(seq_len(nrow(ht))), -1)

top_border_set_default <- xxx_border_arrow_yyy_hux("tb_borders",
      quote(-last_row), quote(seq_len(ncol(ht))), "thickness")
top_border_set_brdr <- xxx_border_arrow_yyy_hux_brdr("tb_borders",
      quote(-last_row), quote(seq_len(ncol(ht))))

bottom_border_set_default <- xxx_border_arrow_yyy_hux("tb_borders",
      -1, quote(seq_len(ncol(ht))), "thickness")
bottom_border_set_brdr <- xxx_border_arrow_yyy_hux_brdr("tb_borders",
      -1, quote(seq_len(ncol(ht))))

#' @export
#' @rdname borders
`left_border<-` <- function (ht, value) {
  if (is_brdr(value)) {
    left_border_set_brdr(ht, value)
  } else {
    left_border_set_default(ht, value)
  }
}

#' @export
#' @rdname borders
`right_border<-` <- function (ht, value) {
  if (is_brdr(value)) {
    right_border_set_brdr(ht, value)
  } else {
    right_border_set_default(ht, value)
  }
}

#' @export
#' @rdname borders
`top_border<-` <- function (ht, value) {
  if (is_brdr(value)) {
    top_border_set_brdr(ht, value)
  } else {
    top_border_set_default(ht, value)
  }
}

#' @export
#' @rdname borders
`bottom_border<-` <- function (ht, value) {
  if (is_brdr(value)) {
    bottom_border_set_brdr(ht, value)
  } else {
    bottom_border_set_default(ht, value)
  }
}

## style setters ------------------------------------------------------------

#' @export
`left_border_style<-`   <- xxx_border_arrow_yyy_hux("lr_borders",
      i = quote(seq_len(nrow(ht))), j = quote(-last_col), "style")
#' @export
`right_border_style<-`  <- xxx_border_arrow_yyy_hux("lr_borders",
      i = quote(seq_len(nrow(ht))), j = -1, "style")
#' @export
`top_border_style<-`    <- xxx_border_arrow_yyy_hux("tb_borders",
      i = quote(-last_row), j = quote(seq_len(ncol(ht))), "style")
#' @export
`bottom_border_style<-` <- xxx_border_arrow_yyy_hux("tb_borders",
      i = -1, j = quote(seq_len(ncol(ht))), "style")

## color setters ------------------------------------------------------------

#' @export
`left_border_color<-`   <- xxx_border_arrow_yyy_hux("lr_borders",
      i = quote(seq_len(nrow(ht))), j = quote(-last_col), "color")
#' @export
`right_border_color<-`  <- xxx_border_arrow_yyy_hux("lr_borders",
      i = quote(seq_len(nrow(ht))), j = -1, "color")
#' @export
`top_border_color<-`    <- xxx_border_arrow_yyy_hux("tb_borders",
      i = quote(-last_row), j = quote(seq_len(ncol(ht))), "color")
#' @export
`bottom_border_color<-` <- xxx_border_arrow_yyy_hux("tb_borders",
      i = -1, j = quote(seq_len(ncol(ht))), "color")

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

## style getters ------------------------------------------------------------

#' @export
left_border_style <- xxx_border_yyy_huxtable("lr_borders",
  i = quote(seq_len(nrow(ht))), j = quote(-last_col), "style")
#' @export
right_border_style <- xxx_border_yyy_huxtable("lr_borders",
  i = quote(seq_len(nrow(ht))), j = -1, "style")
#' @export
top_border_style <- xxx_border_yyy_huxtable("tb_borders",
  i = quote(-last_row), j = quote(seq_len(ncol(ht))), "style")
#' @export
bottom_border_style <- xxx_border_yyy_huxtable("tb_borders",
  i = -1, j = quote(seq_len(ncol(ht))), "style")

## color getters ------------------------------------------------------------

#' @export
left_border_color <- xxx_border_yyy_huxtable("lr_borders",
  i = quote(seq_len(nrow(ht))), j = quote(-last_col), "color")
#' @export
right_border_color <- xxx_border_yyy_huxtable("lr_borders",
  i = quote(seq_len(nrow(ht))), j = -1, "color")
#' @export
top_border_color <- xxx_border_yyy_huxtable("tb_borders",
  i = quote(-last_row), j = quote(seq_len(ncol(ht))), "color")
#' @export
bottom_border_color <- xxx_border_yyy_huxtable("tb_borders",
  i = -1, j = quote(seq_len(ncol(ht))), "color")

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

