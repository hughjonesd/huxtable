#' @import assertthat
#' @importFrom stats na.omit
NULL

#' @export
#' @rdname to_screen
print_screen <- function(ht, ...) cat(to_screen(ht, ...))


#' Print a huxtable on screen
#'
#' @param ht A huxtable.
#' @param ... Passed on to `to_screen`.
#' @param min_width Minimum width in on-screen characters of the result.
#' @param max_width Maximum width in on-screen characters of the result. Overrides `min_width`.
#' @param compact Logical. To save space, don't print lines for empty horizontal borders.
#' @param colnames Logical. Whether or not to print colum names.
#' @param color Logical. Whether to print the huxtable in color (requires the `crayon` package).
#'
#' @return `to_screen` returns a string. `print_screen` prints the string and returns `NULL`.
#'
#' @details
#' Screen display shows the following features:
#'
#' * Table and caption positioning
#' * Merged cells
#' * Cell alignment
#' * Borders
#' * Cell background and border color (if the "crayon" package is installed)
#' * Text color, bold and italic (if the "crayon" package is installed)
#'
#' Cell padding, widths and heights are not shown, nor are border styles.
#'
#' @export
#' @family printing functions
#' @examples
#' bottom_border(jams)[1, 1:2] <- 1
#' bold(jams)[1, 1:2] <- TRUE
#' jams <- map_text_color(
#'   jams,
#'   by_regex("berry" = "red")
#' )
#'
#' print_screen(jams)
to_screen <- NULL

#' Apply borders and colours to a character matrix for screen output
#'
#' Internal helper for [to_screen()].
#'
#' @param ht A huxtable object with the correct number of columns.
#' @param char_data List returned by [character_matrix()].
#' @param color Logical: whether to colour borders.
#' @return Modified `char_data` list.
#' @noRd
apply_screen_borders <- function(ht, char_data, color) {
  charmat <- char_data$charmat
  border_rows <- char_data$border_rows
  border_cols <- char_data$border_cols

  border_cols[-1] <- border_cols[-1] + 1

  borders <- get_visible_borders(ht)
  border_mat <- matrix(0L, nrow(charmat), ncol(charmat))
  index_rows <- lapply(seq_len(nrow(ht)), function(x) seq(border_rows[x], border_rows[x + 1] - 1))
  index_cols <- lapply(seq_len(ncol(ht)), function(x) seq(border_cols[x], border_cols[x + 1] - 1))

  for (i in seq_len(nrow(ht) + 1)) {
    for (j in seq_len(ncol(ht) + 1)) {
      if (i <= nrow(ht) && borders$vert[i, j] > 0) {
        ir <- index_rows[[i]]
        border_mat[ir, border_cols[j]] <- border_mat[ir, border_cols[j]] + 1L
        border_mat[ir + 1, border_cols[j]] <- border_mat[ir + 1, border_cols[j]] + 2L
      }
      if (j <= ncol(ht) && borders$horiz[i, j] > 0) {
        ic <- index_cols[[j]]
        border_mat[border_rows[i], ic] <- border_mat[border_rows[i], ic] + 4L
        border_mat[border_rows[i], ic + 1] <- border_mat[border_rows[i], ic + 1] + 8L
      }
    }
  }

  pipe_chars <- c(
    NA,
    "\u2502", "\u2502", "\u2502", "\u2500",
    "\u250c", "\u2514", "\u251c", "\u2500",
    "\u2510", "\u2518", "\u2524", "\u2500",
    "\u252c", "\u2534", "\u253c"
  )
  border_chars <- pipe_chars[border_mat + 1L]
  charmat[!is.na(border_chars)] <- border_chars[!is.na(border_chars)]

  if (color) {
    bcolors <- collapsed_border_colors(ht)
    uniq <- unique(na.omit(unlist(bcolors)))
    col_funs <- lapply(uniq, crayon::make_style)
    names(col_funs) <- uniq
    for (i in seq_len(nrow(ht) + 1)) {
      for (j in seq_len(ncol(ht) + 1)) {
        if (i <= nrow(ht)) {
          ir <- index_rows[[i]]
          col_fun <- col_funs[[bcolors$vert[i, j]]]
          if (!is.na(bcolors$vert[i, j])) charmat[ir, border_cols[j]] <- col_fun(charmat[ir, border_cols[j]])
        }
        if (j <= ncol(ht)) {
          ic <- c(index_cols[[j]], max(index_cols[[j]]) + 1)
          col_fun <- col_funs[[bcolors$horiz[i, j]]]
          if (!is.na(bcolors$horiz[i, j])) charmat[border_rows[i], ic] <- col_fun(charmat[border_rows[i], ic])
        }
      }
    }
  }

  char_data$charmat <- charmat
  char_data$border_cols <- border_cols
  char_data$border_rows <- border_rows
  char_data
}


# calculate text column widths, wrap huxtable text accordingly, and return a
# matrix of characters, without borders
 #' Build a matrix of characters representing a huxtable
 #'
 #' This is an internal workhorse for screen and markdown output. It
 #' calculates column widths, wraps and aligns cell contents and returns
 #' matrices of characters and their screen widths together with border
 #' locations.
 #'
 #' @param ht A huxtable.
 #' @param inner_border_h,inner_border_v Widths of borders between cells.
 #' @param outer_border_h,outer_border_v Widths of the table's outer borders.
 #' @param min_width Minimum width of the rendered table.
 #' @param max_width Maximum width of the rendered table.
 #' @param color Logical. If `TRUE` then text is coloured using crayon.
 #' @param markdown Logical. If `TRUE` then strings are formatted for markdown output.
 #' @return A list with `charmat`, `width_mat`, `border_rows`, `border_cols`
 #'   and `last_ht_col`.
 #' @noRd
character_matrix <- NULL


col_aware_strsplit <- function(...) {
  if (requireNamespace("crayon", quietly = TRUE)) {
    crayon::col_strsplit(...)
  } else {
    strsplit(...)
  }
}


col_aware_strpad <- function(string, width, side) {
  if (requireNamespace("crayon", quietly = TRUE)) {
    clean <- crayon::strip_style(string)
    padded <- stringi::stri_pad(clean, width, side = side, use_length = TRUE)
    # returns a matrix. First column is whole match. Next columns are captures:
    pads <- stringr::str_match(padded, "^( *).*?( *)$")
    paste0(pads[, 2], string, pads[, 3])
  } else {
    stringi::stri_pad(string, width, side = side, use_length = TRUE)
  }
}


pad_position <- function(string, position, max_width) {
  width <- min(max_width, getOption("width", 80))
  assert_that(position %in% c("left", "center", "right"))
  if (position == "left") {
    return(string)
  }
  side <- if (position == "center") "both" else "left"
  stringr::str_pad(string, width, side)
}


make_cell_style <- function(ht, row, col) {
  tc <- text_color(ht)[row, col]
  bgc <- background_color(ht)[row, col]
  bold <- bold(ht)[row, col]
  italic <- italic(ht)[row, col]

  maybe_combine_style <- function(style, style2) {
    if (is.null(style)) style2 else crayon::combine_styles(style, style2)
  }
  style <- NULL
  if (bold) style <- crayon::bold
  if (italic) style <- maybe_combine_style(style, crayon::italic)
  if (!is.na(tc)) style <- maybe_combine_style(style, crayon::make_style(tc))
  if (!is.na(bgc)) style <- maybe_combine_style(style, crayon::make_style(bgc, bg = TRUE))
  style <- style %||% identity

  style
}
