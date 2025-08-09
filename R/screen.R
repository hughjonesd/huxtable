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
#' This implementation draws a simple text representation using
#' Unicode box-drawing characters. It focuses on width handling and
#' basic borders and does not attempt to reproduce every styling
#' feature of other output formats. The `color` argument is accepted
#' for compatibility but currently ignored.
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
to_screen <- function(ht, min_width = 0, max_width = Inf, compact = FALSE,
                      colnames = TRUE, color = TRUE) {
  assert_that(is_huxtable(ht))
  if (color && !requireNamespace("crayon", quietly = TRUE)) {
    warning("crayon package is required for color output", call. = FALSE)
  }

  char_data <- character_matrix(ht, colnames = colnames)
  mat <- char_data$strings
  widths <- char_data$widths
  ncols <- length(widths)

  if (colnames && nrow(mat) > 0 && all(colnames(ht) == "")) {
    # don't include empty column names
    mat <- mat[-1, , drop = FALSE]
    colnames <- FALSE
  }

  borders <- get_visible_borders(ht)
  if (all(borders$vert == 0) && all(borders$horiz == 0)) {
    rows <- apply(mat, 1, function(r) paste(r, collapse = " "))
    out <- rows
  } else {
    make_hline <- function() {
      paste0("+", paste0(stringi::stri_dup("-", widths + 2), collapse = "+"), "+")
    }

    make_row <- function(row) {
      cells <- vapply(seq_len(ncols), function(i) {
        stringi::stri_pad(row[i], widths[i], side = "right", use_length = TRUE)
      }, character(1))
      paste0("| ", paste(cells, collapse = " | "), " |")
    }

    rows <- apply(mat, 1, make_row)
    hline <- make_hline()

    if (colnames && nrow(mat) > 0) {
      body <- c(rows[1], hline, rows[-1])
    } else {
      body <- rows
    }
    out <- c(hline, body, hline)
  }

  # enforce max_width by truncating
  if (is.finite(max_width)) {
    out <- stringi::stri_sub(out, 1, max_width)
  }

  # ensure min_width
  if (min_width > 0) {
    out <- stringi::stri_pad(out, min_width, side = "right", use_length = TRUE)
  }

  paste(c(out, ""), collapse = "\n")
}

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
  styles  <- collapsed_border_styles(ht)

  border_mat <- matrix(0L, nrow(charmat), ncol(charmat))
  style_mat  <- matrix(1L, nrow(charmat), ncol(charmat))
  style_lookup <- c(solid = 1L, double = 2L, dotted = 3L, dashed = 4L)
  update_style <- function(old, new) {
    if (old == new || old == 1L) new else if (new == 1L) old else 1L
  }

  index_rows <- lapply(seq_len(nrow(ht)), function(x) seq(border_rows[x], border_rows[x + 1] - 1))
  index_cols <- lapply(seq_len(ncol(ht)), function(x) seq(border_cols[x], border_cols[x + 1] - 1))

  for (i in seq_len(nrow(ht) + 1)) {
    for (j in seq_len(ncol(ht) + 1)) {
      # Each (i, j) pair represents the corner above row i and left of column j.
      if (i <= nrow(ht) && borders$vert[i, j] > 0) {
        ir <- index_rows[[i]]
        sidx <- style_lookup[styles$vert[i, j]] %||% 1L
        # Flag vertical borders with bit 1 for the top half and bit 2 for the bottom half.
        border_mat[ir, border_cols[j]] <- border_mat[ir, border_cols[j]] + 1L
        border_mat[ir + 1, border_cols[j]] <- border_mat[ir + 1, border_cols[j]] + 2L
        style_mat[ir, border_cols[j]]     <- update_style(style_mat[ir, border_cols[j]], sidx)
        style_mat[ir + 1, border_cols[j]] <- update_style(style_mat[ir + 1, border_cols[j]], sidx)
      }
      if (j <= ncol(ht) && borders$horiz[i, j] > 0) {
        ic <- index_cols[[j]]
        sidx <- style_lookup[styles$horiz[i, j]] %||% 1L
        # Horizontal borders use bit 4 for the left segment and bit 8 for the right segment.
        border_mat[border_rows[i], ic] <- border_mat[border_rows[i], ic] + 4L
        border_mat[border_rows[i], ic + 1] <- border_mat[border_rows[i], ic + 1] + 8L
        style_mat[border_rows[i], ic]     <- update_style(style_mat[border_rows[i], ic], sidx)
        style_mat[border_rows[i], ic + 1] <- update_style(style_mat[border_rows[i], ic + 1], sidx)
      }
    }
  }

  pipe_sets <- list(
    c(NA, "\u2502", "\u2502", "\u2502", "\u2500", "\u250c", "\u2514", "\u251c", "\u2500", "\u2510", "\u2518", "\u2524", "\u2500", "\u252c", "\u2534", "\u253c"),
    c(NA, "\u2551", "\u2551", "\u2551", "\u2550", "\u2554", "\u255a", "\u2560", "\u2550", "\u2557", "\u255d", "\u2563", "\u2550", "\u2566", "\u2569", "\u256c"),
    c(NA, "\u250a", "\u250a", "\u250a", "\u2508", "\u250c", "\u2514", "\u251c", "\u2508", "\u2510", "\u2518", "\u2524", "\u2508", "\u252c", "\u2534", "\u253c"),
    c(NA, "\u2506", "\u2506", "\u2506", "\u2504", "\u250c", "\u2514", "\u251c", "\u2504", "\u2510", "\u2518", "\u2524", "\u2504", "\u252c", "\u2534", "\u253c")
  )

  border_chars <- matrix(NA_character_, nrow(charmat), ncol(charmat))
  for (s in seq_along(pipe_sets)) {
    mask <- style_mat == s & border_mat > 0
    border_chars[mask] <- pipe_sets[[s]][border_mat[mask] + 1L]
  }
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
#' This minimal internal helper extracts cell contents as strings and
#' calculates simple column widths for use by [to_screen()].
#'
#' @param ht A huxtable.
#' @param colnames Logical. Include column names as the first row?
#' @return A list with `strings`, a character matrix, and `widths`, the
#'   display width of each column.
#' @noRd
character_matrix <- function(ht, colnames = TRUE, ...) {
  assert_that(is_huxtable(ht))
  mat <- clean_contents(ht, output_type = "screen")
  if (colnames) {
    mat <- rbind(colnames(ht), mat)
  }
  widths <- apply(mat, 2, function(x) max(stringi::stri_width(x), na.rm = TRUE))
  list(strings = mat, widths = widths)
}


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
