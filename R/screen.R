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
to_screen <- function(ht,
                      min_width = ceiling(getOption("width") / 6),
                      max_width = getOption("width", Inf),
                      compact = TRUE,
                      colnames = TRUE,
                      color = getOption("huxtable.color_screen", default = TRUE),
                      ...) {
  assert_that(is.number(min_width), is.number(max_width), is.flag(compact), is.flag(colnames), is.flag(color))

  if (color && !requireNamespace("crayon", quietly = TRUE)) {
    warning(
      "On-screen color requires the `crayon` package. Run:\n",
      "install.packages(\"crayon\")\n",
      "or set `options(huxtable.color_screen = FALSE)`."
    )
    color <- FALSE
  }

  all_colnames <- colnames(ht)
  last_ht_col <- orig_ncol <- ncol(ht)

  if (ncol(ht) > 0 && nrow(ht) > 0) {
    char_data <- character_matrix(
      ht,
      inner_border_h = 3, outer_border_h = 2,
      inner_border_v = 1, outer_border_v = 1,
      min_width = min_width, max_width = max_width,
      color = color, markdown = FALSE
    )

    last_ht_col <- char_data$last_ht_col
    ht <- ht[, seq_len(last_ht_col)]
    char_data <- apply_screen_borders(ht, char_data, color)

    charmat   <- char_data$charmat
    width_mat <- char_data$width_mat
    border_rows <- char_data$border_rows

    if (compact) {
      empty <- apply(charmat, 1, function(x) {
        all(grepl(" ", x, fixed = TRUE) | grepl("\u2502", x, fixed = TRUE))
      })
      empty <- intersect(border_rows, which(empty))
      if (length(empty) > 0) {
        charmat <- charmat[-empty, , drop = FALSE]
        width_mat <- width_mat[-empty, , drop = FALSE]
      }
    }

    result <- apply(charmat, 1, paste0, collapse = "")
    width_mat <- pmax(width_mat, 1)
    width_mat[charmat == ""] <- 0
    row_char_widths <- rowSums(width_mat)
    pad_width <- min(max_width, getOption("width", 80)) - max(row_char_widths)
    pad_width <- switch(position_no_wrap(ht),
      "left" = 0,
      "right" = pad_width,
      "center" = floor(pad_width / 2)
    )
    result <- paste0(strrep(" ", max(pad_width, 0)), result)
    result <- paste(result, collapse = "\n")
    result <- paste0(result, "\n")
  } else {
    result <- glue::glue("<huxtable with {nrow(ht)} rows and {ncol(ht)} columns>\n")
  }

  if (!is.na(cap <- caption(ht))) {
    poss_pos <- c("left", "center", "right")
    hpos <- if (any(found <- sapply(poss_pos, grepl, x = caption_pos(ht)))) {
      poss_pos[found]
    } else {
      position_no_wrap(ht)
    }
    if (ncharw(cap) > max_width) cap <- strwrap(cap, max_width)
    stringr_side <- switch(hpos,
      left = "right",
      right = "left",
      center = "both"
    )
    cap <- stringr::str_pad(cap, ncol(charmat) - 4, stringr_side)
    cap <- stringr::str_pad(cap, ncol(charmat), "both")
    cap <- paste0(pad_position(cap, position_no_wrap(ht), max_width), collapse = "\n")
    cap <- paste0(cap, "\n")
    result <- if (grepl("top", caption_pos(ht))) paste0(cap, result) else paste0(result, cap)
  }

  if (colnames && any(nzchar(all_colnames))) {
    cn <- paste0("Column names: ", paste(all_colnames, collapse = ", "))
    cn <- strwrap(cn, max_width)
    cn <- paste0(cn, collapse = "\n")
    result <- paste0(result, "\n", cn, "\n")
  }

  if (last_ht_col < orig_ncol) {
    result <- glue::glue("{result}\n{last_ht_col}/{orig_ncol} columns shown.\n")
  }

  result
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
character_matrix <- function(ht,
                             inner_border_h,
                             inner_border_v,
                             outer_border_h,
                             outer_border_v,
                             min_width,
                             max_width = Inf,
                             color = FALSE,
                             markdown) {
  if (ncol(ht) == 0) stop("Couldn't display any columns in less than max_width characters.")

  dc <- display_cells(ht, all = FALSE)
  dc <- dc[order(dc$colspan), ]
  contents <- clean_contents(ht, output_type = if (markdown) "markdown" else "screen")
  drow_mat <- as.matrix(dc[, c("display_row", "display_col")])
  dc$contents <- contents[drow_mat]

  cw <- col_width(ht)
  if (!is.numeric(cw) || anyNA(cw)) cw <- rep(1, ncol(ht))
  cw <- cw / sum(cw)
  widths <- pmax(1, ceiling(min_width * cw))

  # compute width requirements from cell contents ---------------------------------
  content_widths <- ncharw(dc$contents)
  max_word <- sapply(lapply(strsplit(dc$contents, "(\t|\n|\r|\v )"), ncharw), function(x) max(c(0, x)))
  for (i in seq_len(nrow(dc))) {
    dcell <- dc[i, ]
    need <- if (wrap(ht)[dcell$display_row, dcell$display_col]) max_word[i] else content_widths[i]
    if (markdown && bold(ht)[dcell$display_row, dcell$display_col]) need <- need + 4
    if (markdown && italic(ht)[dcell$display_row, dcell$display_col]) need <- need + 2
    cols <- seq(dcell$display_col, dcell$end_col)
    avail <- sum(widths[cols])
    if (need > avail) {
      extra <- ceiling((need - avail) / dcell$colspan)
      widths[cols] <- widths[cols] + extra
    }
  }

  # shrink to max width -----------------------------------------------------------
  available <- max_width - 2 * outer_border_h - (ncol(ht) - 1) * inner_border_h
  max_widths <- floor(available * cw)
  if (any(max_widths < 1)) {
    return(character_matrix(ht[, -ncol(ht)], inner_border_h, inner_border_v, outer_border_h,
      outer_border_v, min_width, max_width, color = color, markdown = markdown))
  }
  widths <- pmin(widths, max_widths)

  # wrap, pad and align -----------------------------------------------------------
  dc$strings <- vector("list", nrow(dc))
  for (i in seq_len(nrow(dc))) {
    dcell <- dc[i, ]
    cols <- seq(dcell$display_col, dcell$end_col)
    width <- sum(widths[cols])
    md_bold <- markdown && bold(ht)[dcell$display_row, dcell$display_col]
    md_italic <- markdown && italic(ht)[dcell$display_row, dcell$display_col]
    eff_width <- width
    if (md_bold) eff_width <- eff_width - 4
    if (md_italic) eff_width <- eff_width - 2
    text <- dcell$contents
    while (!identical(new <- gsub("( |\u00a0)$", "\u00a0", text), text)) text <- new
    text <- gsub("\n", "\n\n", text, fixed = TRUE)
    strings <- fansi::strwrap2_ctl(text, width = eff_width + 1, wrap.always = TRUE)
    strings <- strings[strings != ""]
    if (length(strings) == 0) strings <- ""
    if (md_bold) strings[ncharw(strings) > 0] <- paste0("**", strings[ncharw(strings) > 0], "**")
    if (md_italic) strings[ncharw(strings) > 0] <- paste0("*", strings[ncharw(strings) > 0], "*")
    align <- real_align(ht)[dcell$display_row, dcell$display_col]
    side <- switch(align, left = "right", right = "left", center = "both")
    strings <- col_aware_strpad(strings, width, side)
    dc$strings[[i]] <- strings
  }

  dc$text_height <- vapply(dc$strings, length, integer(1))
  dc$text_width <- vapply(dc$strings, function(x) max(ncharw(x, type = "chars")), integer(1))

  # determine row heights ---------------------------------------------------------
  dc <- dc[order(dc$rowspan), ]
  heights <- rep(1, nrow(ht))
  for (i in seq_len(nrow(dc))) {
    dcell <- dc[i, ]
    rows <- seq(dcell$display_row, dcell$end_row)
    need <- dcell$text_height
    if (sum(heights[rows]) + inner_border_v * (dcell$rowspan - 1) < need) {
      extra <- ceiling((need - sum(heights[rows])) / dcell$rowspan)
      heights[rows] <- heights[rows] + extra
    }
  }

  border_widths <- c(outer_border_h, rep(inner_border_h, ncol(ht) - 1), outer_border_h)
  all_widths <- border_widths + c(0, widths)
  start_cols <- cumsum(all_widths[seq_len(ncol(ht))]) + 1
  border_cols <- c(start_cols, sum(all_widths) + 1) - border_widths

  border_heights <- c(outer_border_v, rep(inner_border_v, nrow(ht) - 1), outer_border_v)
  all_heights <- border_heights + c(0, heights)
  start_rows <- cumsum(all_heights[seq_len(nrow(ht))]) + 1
  border_rows <- c(start_rows, sum(all_heights) + 1) - border_heights

  charmat <- matrix(" ", sum(all_heights), sum(all_widths))
  width_mat <- matrix(0, sum(all_heights), sum(all_widths))
  for (i in seq_len(nrow(dc))) {
    dcell <- dc[i, ]
    letters <- unlist(col_aware_strsplit(dcell$strings[[1]], ""))
    style <- if (color) make_cell_style(ht, dcell$display_row, dcell$display_col) else identity
    rows <- seq(start_rows[dcell$display_row], start_rows[dcell$display_row] + dcell$text_height - 1)
    cols <- seq(start_cols[dcell$display_col], start_cols[dcell$display_col] + dcell$text_width - 1)
    charmat[rows, cols] <- matrix(style(letters), length(rows), length(cols), byrow = TRUE)
    width_mat[rows, cols] <- matrix(ncharw(letters), length(rows), length(cols), byrow = TRUE)
  }

  list(
    charmat = charmat,
    width_mat = width_mat,
    border_rows = border_rows,
    border_cols = border_cols,
    last_ht_col = ncol(ht)
  )
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
