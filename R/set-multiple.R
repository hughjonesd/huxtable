
#' @import assertthat
NULL


#' Set left, right, top and bottom properties
#'
#' These functions set left, right,
#' top and/or bottom properties
#' simultaneously for the specified cells.
#'
#' * `set_all_*` functions set top, bottom, left and right properties.
#' * `set_tb_*` functions set top and bottom properties.
#' * `set_lr_*` functions set left and right properties.
#'
#'
#' @template property-params
#' @param value Value(s) to set. Set to `NA` to reset to the default.
#'
#' @return The modified huxtable.
#'
#' @seealso [borders], [border-colors], [border-styles], [padding].
#' @name set-multiple
NULL


#' @rdname set-multiple
#' @export
#' @examples
#' ht <- as_hux(jams)
#' ht <- set_all_borders(ht)
#' ht
set_all_borders <- function(ht, row, col, value = 0.4) {
  recall_ltrb(ht, "set_%s_border")
}

#' @rdname set-multiple
#' @export
map_all_borders <- function (ht, row, col, fn) {
  recall_ltrb(ht, "map_%s_border")
}


#' @rdname set-multiple
#' @export
#' @examples
#' ht <- set_all_border_colors(ht, "red")
#' ht
set_all_border_colors <- function(ht, row, col, value) {
  recall_ltrb(ht, "set_%s_border_color")
}

#' @rdname set-multiple
#' @export
map_all_border_colors <- function (ht, row, col, fn) {
  recall_ltrb(ht, "map_%s_border_color")
}


#' @rdname set-multiple
#' @export
#' @examples
#' ht <- set_all_border_styles(ht, "double")
set_all_border_styles <- function(ht, row, col, value) {
  recall_ltrb(ht, "set_%s_border_style")
}

#' @rdname set-multiple
#' @export
map_all_border_styles <- function (ht, row, col, fn) {
  recall_ltrb(ht, "map_%s_border_style")
}


#' @rdname set-multiple
#' @export
#' @examples
#' ht <- set_all_padding(ht, 1:3, 1:2, "20px")
set_all_padding <- function(ht, row, col, value) {
  recall_ltrb(ht, "set_%s_padding")
}


#' @rdname set-multiple
#' @export
map_all_padding <- function (ht, row, col, fn) {
  recall_ltrb(ht, "map_%s_padding")
}


#' @rdname set-multiple
#' @export
#' @examples
#' ht <- set_tb_padding(ht, 10)
set_tb_padding <- function (ht, row, col, value) {
  recall_ltrb(ht, "set_%s_padding", sides = c("top", "bottom"))
}


#' @rdname set-multiple
#' @export
map_tb_padding <- function (ht, row, col, fn) {
  recall_ltrb(ht, "map_%s_padding", sides = c("top", "bottom"))
}


#' @rdname set-multiple
#' @export
set_lr_padding <- function (ht, row, col, value) {
  recall_ltrb(ht, "set_%s_padding", sides = c("left", "right"))
}


#' @rdname set-multiple
#' @export
map_lr_padding <- function (ht, row, col, fn) {
  recall_ltrb(ht, "map_%s_padding", sides = c("left", "right"))
}


#' @rdname set-multiple
#' @export
#' @examples
#' ht <- set_tb_borders(ht)
set_tb_borders <- function (ht, row, col, value) {
  recall_ltrb(ht, "set_%s_border", sides = c("top", "bottom"))
}


#' @rdname set-multiple
#' @export
map_tb_borders <- function (ht, row, col, fn) {
  recall_ltrb(ht, "map_%s_border", sides = c("top", "bottom"))
}


#' @rdname set-multiple
#' @export
set_lr_borders <- function (ht, row, col, value) {
  recall_ltrb(ht, "set_%s_border", sides = c("left", "right"))
}


#' @rdname set-multiple
#' @export
map_lr_borders <- function (ht, row, col, fn) {
  recall_ltrb(ht, "map_%s_border", sides = c("left", "right"))
}


#' @rdname set-multiple
#' @export
#' @examples
#' set_tb_border_colors(ht, "red")
set_tb_border_colors <- function (ht, row, col, value) {
  recall_ltrb(ht, "set_%s_border_color", sides = c("top", "bottom"))
}


#' @rdname set-multiple
#' @export
map_tb_border_colors <- function (ht, row, col, fn) {
  recall_ltrb(ht, "map_%s_border_color", sides = c("top", "bottom"))
}


#' @rdname set-multiple
#' @export
set_lr_border_colors <- function (ht, row, col, value) {
  recall_ltrb(ht, "set_%s_border_color", sides = c("left", "right"))
}


#' @rdname set-multiple
#' @export
map_lr_border_colors <- function (ht, row, col, fn) {
  recall_ltrb(ht, "map_%s_border_color", sides = c("left", "right"))
}


#' @rdname set-multiple
#' @export
#' @examples
#' set_tb_border_styles(ht, "double")
set_tb_border_styles <- function (ht, row, col, value) {
  recall_ltrb(ht, "set_%s_border_style", sides = c("top", "bottom"))
}


#' @rdname set-multiple
#' @export
map_tb_border_styles <- function (ht, row, col, fn) {
  recall_ltrb(ht, "map_%s_border_style", sides = c("top", "bottom"))
}


#' @rdname set-multiple
#' @export
set_lr_border_styles <- function (ht, row, col, value) {
  recall_ltrb(ht, "set_%s_border_style", sides = c("left", "right"))
}


#' @rdname set-multiple
#' @export
map_lr_border_styles <- function (ht, row, col, fn) {
  recall_ltrb(ht, "map_%s_border_style", sides = c("left", "right"))
}


recall_ltrb <- function(ht, template,
      sides = c("left", "top", "right", "bottom")) {
  call <- sys.call(sys.parent(1L))
  call_names <- parse(text = paste0("huxtable::",
    sprintf(template, sides)))
  for (cn in call_names) {
    call[[1]] <- cn
    call[[2]] <- quote(ht)
    ht <- eval(call, list(ht = ht), parent.frame(2L)) # = sys.frame(sys.parent(1)) i.e. caller of orig
  }

  ht
}
