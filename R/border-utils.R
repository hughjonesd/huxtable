#' Compute visible borders accounting for hidden spans
#'
#' @noRd
get_visible_borders <- function(ht) {
  dc <- display_cells(ht)

  # a vertical border is hidden, if it is shadowed by a cell to its left
  vert_borders <- attr(ht, "lr_borders")$thickness
  left_shadowed <- dc[dc$display_col < dc$col, ]
  left_shadowed <- as.matrix(left_shadowed[c("row", "col")])
  vert_borders[left_shadowed] <- 0

  # a horizontal border is hidden, if it is shadowed by a cell above it
  horiz_borders <- attr(ht, "tb_borders")$thickness
  top_shadowed <- dc[dc$display_row < dc$row, ]
  top_shadowed <- as.matrix(top_shadowed[c("row", "col")])
  horiz_borders[top_shadowed] <- 0

  res <- list(vert = vert_borders, horiz = horiz_borders)
  return(res)
}


#' Collapsed border colors for vertical and horizontal borders
#'
#' @noRd
collapsed_border_colors <- function(ht) {
  list(
    vert  = attr(ht, "lr_borders")$color,
    horiz = attr(ht, "tb_borders")$color
  )
}


#' Collapsed border styles for vertical and horizontal borders
#'
#' @noRd
collapsed_border_styles <- function(ht) {
  list(
    vert  = attr(ht, "lr_borders")$style,
    horiz = attr(ht, "tb_borders")$style
  )
}
