# Typst printing ------------------------------------------------------------------

#' @export
#'
#' @rdname to_typst
print_typst <- function(ht, ...) {
  cat(to_typst(ht, ...))
}


#' Create Typst markup representing a huxtable
#'
#' These functions print or return a Typst table.
#'
#' @param ht A huxtable.
#' @param ... Arguments passed to methods. Not currently used.
#'
#' @return `to_typst` returns a Typst string. `print_typst` prints the string and returns `NULL`.
#' @export
#'
#' @family printing functions
#'
#' @examples
#' ht <- huxtable(a = 1:3, b = letters[1:3])
#' to_typst(ht)
to_typst <- function(ht, ...) {
  if (!check_positive_dims(ht)) {
    return("")
  }

  contents <- clean_contents(ht, output_type = "latex")
  dc <- display_cells(ht)
  shadow <- matrix(dc$shadowed, nrow(ht), ncol(ht))

  rs <- rowspan(ht)
  cs <- colspan(ht)
  align <- real_align(ht)

  col_w <- col_width(ht)
  if (is.numeric(col_w)) {
    col_w_str <- ifelse(is.na(col_w), "auto", paste0(col_w, "fr"))
  } else {
    col_w_str <- ifelse(is.na(col_w), "auto", col_w)
  }

  table_opts <- typst_table_options(ht, col_w_str)
  table_start <- paste0("#table(\n  ", paste(table_opts, collapse = ",\n  "), "\n)[\n")

  cells <- matrix("", nrow(ht), ncol(ht))
  row_h <- row_height(ht)

  for (i in seq_len(nrow(ht))) {
    for (j in seq_len(ncol(ht))) {
      if (!shadow[i, j]) {
        cells[i, j] <- typst_cell(
          ht, i, j, contents[i, j], rs[i, j], cs[i, j], align[i, j], row_h[i]
        )
      }
    }
  }

  row_strings <- apply(cells, 1, function(x) paste(x[x != ""], collapse = " "))
  result <- paste0(table_start, paste0("  ", row_strings, collapse = "\n"), "\n]\n")
  result
}

# helpers -----------------------------------------------------------------------

#' @noRd
typst_table_options <- function(ht, col_w_str) {
  table_opts <- c(paste0("columns: (", paste(col_w_str, collapse = ", "), ")"))

  w <- width(ht)
  if (!is.na(w)) {
    if (is.numeric(w)) w <- sprintf("%.3f%%", w * 100)
    table_opts <- c(table_opts, sprintf("width: %s", w))
  }

  h <- height(ht)
  if (!is.na(h)) {
    if (is.numeric(h)) h <- sprintf("%.3f%%", h * 100)
    table_opts <- c(table_opts, sprintf("height: %s", h))
  }

  pos <- position(ht)
  if (!is.na(pos) && pos %in% c("left", "right")) {
    align <- c(left = "left", right = "right")[pos]
    table_opts <- c(table_opts, sprintf("align: %s", align))
  }

  lab <- make_label(ht)
  cap_raw <- caption(ht)
  if (!is.na(cap_raw)) {
    cap <- make_caption(ht, lab, "latex")

    fig_opts <- c(sprintf("caption: [%s]", cap))

    cp <- caption_pos(ht)
    if (!is.na(cp)) {
      vpos <- if (grepl("top", cp)) "top" else "bottom"
      fig_opts <- c(fig_opts, sprintf("position: %s", vpos))
    }

    cw <- caption_width(ht)
    if (!is.na(cw)) {
      if (is.numeric(cw)) cw <- sprintf("%.3f%%", cw * 100)
      table_opts <- c(table_opts, sprintf("caption-width: %s", cw))
    }

    table_opts <- c(table_opts, sprintf("figure: (%s)", paste(fig_opts, collapse = ", ")))
  }

  table_opts
}

#' @noRd
typst_cell <- function(ht, i, j, contents, rs, cs, al, row_h) {
  opts <- typst_cell_options(ht, i, j, rs, cs, al, row_h)
  text <- typst_cell_text(ht, i, j, contents)
  cell_opts <- if (length(opts) > 0) sprintf("(%s)", paste(opts, collapse = ", ")) else ""
  sprintf("cell%s[%s]", cell_opts, text)
}

#' @noRd
typst_cell_options <- function(ht, i, j, rs, cs, al, row_h) {
  opts <- c()
  if (rs > 1) opts <- c(opts, sprintf("rowspan: %d", rs))
  if (cs > 1) opts <- c(opts, sprintf("colspan: %d", cs))

  va <- c(top = "top", middle = "center", bottom = "bottom")[valign(ht)[i, j]]
  if (!is.na(al) || !is.na(va)) {
    opts <- c(opts, sprintf("align: (%s, %s)", al, va))
  }

  if (!is.na(row_h)) {
    rh <- row_h
    if (is.numeric(rh)) rh <- sprintf("%.3f%%", rh * 100)
    opts <- c(opts, sprintf("height: %s", rh))
  }

  bg <- background_color(ht)[i, j]
  if (!is.na(bg)) {
    opts <- c(opts, sprintf("fill: rgb(%s)", format_color(bg)))
  }

  stroke <- typst_stroke(ht, i, j)
  if (length(stroke)) opts <- c(opts, stroke)

  opts
}

#' @noRd
typst_stroke <- function(ht, i, j) {
  tb <- brdr_thickness(top_border(ht))[i, j]
  rb <- brdr_thickness(right_border(ht))[i, j]
  bb <- brdr_thickness(bottom_border(ht))[i, j]
  lb <- brdr_thickness(left_border(ht))[i, j]
  tbs <- top_border_style(ht)[i, j]
  rbs <- right_border_style(ht)[i, j]
  bbs <- bottom_border_style(ht)[i, j]
  lbs <- left_border_style(ht)[i, j]
  tbc <- format_color(top_border_color(ht)[i, j], default = "black")
  rbc <- format_color(right_border_color(ht)[i, j], default = "black")
  bbc <- format_color(bottom_border_color(ht)[i, j], default = "black")
  lbc <- format_color(left_border_color(ht)[i, j], default = "black")

  stroke_parts <- c()
  if (tb > 0) stroke_parts <- c(stroke_parts, sprintf("top: %.4gpt + %s + rgb(%s)", tb, tbs, tbc))
  if (rb > 0) stroke_parts <- c(stroke_parts, sprintf("right: %.4gpt + %s + rgb(%s)", rb, rbs, rbc))
  if (bb > 0) stroke_parts <- c(stroke_parts, sprintf("bottom: %.4gpt + %s + rgb(%s)", bb, bbs, bbc))
  if (lb > 0) stroke_parts <- c(stroke_parts, sprintf("left: %.4gpt + %s + rgb(%s)", lb, lbs, lbc))

  if (length(stroke_parts) > 0) {
    sprintf("stroke: (%s)", paste(stroke_parts, collapse = ", "))
  } else {
    NULL
  }
}

#' @noRd
typst_cell_text <- function(ht, i, j, cell_text) {
  text_opts <- c()
  if (bold(ht)[i, j]) text_opts <- c(text_opts, "weight: \"bold\"")
  if (italic(ht)[i, j]) text_opts <- c(text_opts, "style: \"italic\"")
  if (!is.na(fs <- font_size(ht)[i, j])) text_opts <- c(text_opts, sprintf("size: %.4gpt", fs))
  if (!is.na(f <- font(ht)[i, j])) text_opts <- c(text_opts, sprintf("family: \"%s\"", f))

  if (length(text_opts) > 0) {
    sprintf("#text(%s)[%s]", paste(text_opts, collapse = ", "), cell_text)
  } else {
    cell_text
  }
}
