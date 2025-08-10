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
  shadow <- matrix(display_cells(ht)$shadowed, nrow(ht), ncol(ht))

  col_w <- col_width(ht)
  if (is.numeric(col_w)) {
    col_w_str <- ifelse(is.na(col_w), "auto", paste0(col_w, "fr"))
  } else {
    col_w_str <- ifelse(is.na(col_w), "auto", col_w)
  }

  table_opts <- typst_table_options(ht, col_w_str)
  table_start <- paste0("table(\n  ", paste(table_opts, collapse = ",\n  "), ",\n")

  cells <- matrix("", nrow(ht), ncol(ht))

  for (row in seq_len(nrow(ht))) {
    for (col in seq_len(ncol(ht))) {
      if (!shadow[row, col]) {
        cells[row, col] <- typst_cell(
          ht = ht,
          row = row,
          col = col,
          content = contents[row, col]
        )
      }
    }
  }

  row_strings <- apply(cells, 1, function(x) paste(x[x != ""], collapse = ", "))

  hr <- header_rows(ht)
  hc <- header_cols(ht)

  header_block <- ""
  if (any(hr)) {
    header_rows_strings <- row_strings[hr]
    header_rows_strings <- header_rows_strings[nzchar(header_rows_strings)]
    if (length(header_rows_strings) > 0) {
      header_block <- paste0(
        "  table.header(\n",
        paste0("    ", header_rows_strings, collapse = "\n"),
        "\n  ),\n"
      )
    }
    row_strings <- row_strings[!hr]
  }
  row_strings <- row_strings[nzchar(row_strings)]

  header_cols_block <- ""
  if (any(hc)) {
    col_strings <- apply(cells[, hc, drop = FALSE], 1, function(x) paste(x[x != ""], collapse = " "))
    col_strings <- col_strings[nzchar(col_strings)]
    if (length(col_strings) > 0) {
      header_cols_block <- paste0(
        "  table.header(columns: (", paste(hc, collapse = ", "), "))[\n",
        paste0("    ", col_strings, collapse = "\n"),
        "\n  ]\n"
      )
    }
  }

  result <- paste0(
    table_start,
    header_block,
    header_cols_block,
    paste0("  ", row_strings, collapse = ",\n"),
    "\n)"
  )
  result <- typst_figure(ht, result)

  if (using_quarto()) {
    result <- paste("\n\n```{=typst}\n", result, "\n```\n\n")
  }

  result
}

# helpers -----------------------------------------------------------------------

#' Escape special characters for Typst markup
#'
#' @param x A character string.
#'
#' @return Escaped string safe for Typst.
#' @noRd
typst_escape <- function(x) {
  x <- gsub("\\\\", "\\\\\\\\", x)
  x <- gsub("#", "\\#", x, fixed = TRUE)
  x <- gsub("[", "\\[", x, fixed = TRUE)
  x <- gsub("]", "\\]", x, fixed = TRUE)
  x
}

#' Build options for a Typst table
#'
#' @param ht A huxtable.
#' @param col_w_str Character vector of column widths formatted for Typst.
#'
#' @return Character vector of table options to be passed to `#table`.
#' @noRd
typst_table_options <- function(ht, col_w_str) {
  table_opts <- c(paste0("columns: (", paste(col_w_str, collapse = ", "), ")"))

  pos <- position(ht)
  if (!is.na(pos) && pos %in% c("left", "right")) {
    align <- pos
    table_opts <- c(table_opts, sprintf("align: %s", align))
  }

  table_opts <- c(table_opts, "stroke: none")

  table_opts
}


#' Surround text by a typst figure
#'
#' @noRd
typst_figure <- function(ht, text) {
  cap <-  if (is.na(caption(ht))) {
            "none"
          } else {
            lab <- make_label(ht)
            cap <- typst_escape(caption(ht)) # TODO what about labels?
            paste0("[", cap, "]")
          }

  cap <- sprintf("caption: %s", cap)

  # TODO: caption_pos, caption_width, position, numbering, label

  fig <- paste0(
    "#figure(\n",
    text,
    ",\n",
    cap,
    "\n",
    ")"
  )

  return(fig)
}


#' Create a Typst table cell
#'
#' @param ht A huxtable.
#' @param row Row index of the cell.
#' @param col Column index of the cell.
#' @param content Cell contents as a string.
#'
#' @return A single Typst cell string, e.g. `cell()[...]`.
#' @noRd
typst_cell <- function(ht, row, col, content) {
  opts <- typst_cell_options(ht = ht, row = row, col = col)
  text <- typst_cell_text(ht, row, col, content)
  cell_opts <- if (length(opts) > 0) {
    sprintf("(%s)", paste(opts, collapse = ", "))
  } else {
    ""
  }
  sprintf("table.cell%s[%s]", cell_opts, text)
}

#' Derive Typst cell options
#'
#' @param ht A huxtable.
#' @param row Row index.
#' @param col Column index.
#'
#' @return Character vector of cell options (possibly empty).
#' @noRd
typst_cell_options <- function(ht, row, col) {
  opts <- c()

  rowspan <- rowspan(ht)[row, col]
  colspan <- colspan(ht)[row, col]
  if (rowspan > 1) opts <- c(opts, sprintf("rowspan: %d", rowspan))
  if (colspan > 1) opts <- c(opts, sprintf("colspan: %d", colspan))

  horizontal_align <- real_align(ht)[row, col]
  vertical_align <- valign(ht)[row, col]
  vertical_align <- c(top = "top", middle = "horizon", bottom = "bottom")[vertical_align]
  if (!is.na(vertical_align)) {
    opts <- c(opts, sprintf("align: (%s + %s)", horizontal_align, vertical_align))
  } else if (!is.na(horizontal_align)) {
    opts <- c(opts, sprintf("align: %s", horizontal_align))
  }

  rh <- row_height(ht)[row]
  if (!is.na(rh)) {
    if (is.numeric(rh)) rh <- sprintf("%.3f%%", rh * 100)
    opts <- c(opts, sprintf("height: %s", rh))
  }

  bg <- background_color(ht)[row, col]
  if (!is.na(bg)) {
    opts <- c(opts, sprintf("fill: rgb(%s)", format_color(bg)))
  }

  pads <- c(
    top    = top_padding(ht)[row, col],
    right  = right_padding(ht)[row, col],
    bottom = bottom_padding(ht)[row, col],
    left   = left_padding(ht)[row, col]
  )
  default_pad <- 6
  if (!all(is.na(pads)) && any(pads != default_pad)) {
    if (length(unique(pads)) == 1) {
      opts <- c(opts, sprintf("inset: %.4gpt", pads[[1]]))
    } else {
      pad_parts <- sprintf("%s: %.4gpt", names(pads), pads)
      pad_parts <- pad_parts[!is.na(pads)]
      opts <- c(opts, sprintf("inset: (%s)", paste(pad_parts, collapse = ", ")))
    }
  }

  stroke <- typst_stroke(ht, row, col)
  if (length(stroke)) opts <- c(opts, stroke)

  opts
}

#' Construct a Typst stroke declaration
#'
#' @param ht A huxtable.
#' @param row Row index.
#' @param col Column index.
#'
#' @return A single `stroke` option or `NULL` if no borders are set.
#' @noRd
typst_stroke <- function(ht, row, col) {
  tb <- brdr_thickness(top_border(ht))[row, col]
  rb <- brdr_thickness(right_border(ht))[row, col]
  bb <- brdr_thickness(bottom_border(ht))[row, col]
  lb <- brdr_thickness(left_border(ht))[row, col]
  tbs <- top_border_style(ht)[row, col]
  rbs <- right_border_style(ht)[row, col]
  bbs <- bottom_border_style(ht)[row, col]
  lbs <- left_border_style(ht)[row, col]
  tbc <- format_color(top_border_color(ht)[row, col], default = "black")
  rbc <- format_color(right_border_color(ht)[row, col], default = "black")
  bbc <- format_color(bottom_border_color(ht)[row, col], default = "black")
  lbc <- format_color(left_border_color(ht)[row, col], default = "black")

  stroke_side <- function(thickness, style, color) {
    if (is.na(style) || style == "solid") {
      sprintf("stroke(thickness: %.4gpt, paint: rgb(%s))", thickness, color)
    } else {
      dash_styles <- c(dashed = "dashed", dotted = "dotted")
      dash <- dash_styles[style]
      if (is.na(dash)) {
        sprintf("stroke(thickness: %.4gpt, paint: rgb(%s))", thickness, color)
      } else {
        sprintf("stroke(thickness: %.4gpt, paint: rgb(%s), dash: \"%s\")", thickness, color, dash)
      }
    }
  }

  stroke_parts <- c()
  if (tb > 0) stroke_parts <- c(stroke_parts, sprintf("top: %s", stroke_side(tb, tbs, tbc)))
  if (rb > 0) stroke_parts <- c(stroke_parts, sprintf("right: %s", stroke_side(rb, rbs, rbc)))
  if (bb > 0) stroke_parts <- c(stroke_parts, sprintf("bottom: %s", stroke_side(bb, bbs, bbc)))
  if (lb > 0) stroke_parts <- c(stroke_parts, sprintf("left: %s", stroke_side(lb, lbs, lbc)))

  if (length(stroke_parts) > 0) {
    sprintf("stroke: (%s)", paste(stroke_parts, collapse = ", "))
  } else {
    NULL
  }
}

#' Apply text styling for a Typst cell
#'
#' @param ht A huxtable.
#' @param row Row index.
#' @param col Column index.
#' @param cell_text The cell's content string.
#'
#' @return A string containing Typst markup for the styled text.
#' @noRd
typst_cell_text <- function(ht, row, col, cell_text) {
  text_opts <- c()
  if (bold(ht)[row, col]) text_opts <- c(text_opts, "weight: \"bold\"")
  if (italic(ht)[row, col]) text_opts <- c(text_opts, "style: \"italic\"")
  if (!is.na(fs <- font_size(ht)[row, col])) text_opts <- c(text_opts, sprintf("size: %.4gpt", fs))
  if (!is.na(f <- font(ht)[row, col])) text_opts <- c(text_opts, sprintf("family: \"%s\"", f))
  if (!is.na(tc <- text_color(ht)[row, col])) text_opts <- c(text_opts, sprintf("fill: rgb(%s)", format_color(tc)))

  if (length(text_opts) > 0) {
    text <- sprintf("#text(%s)[%s]", paste(text_opts, collapse = ", "), cell_text)
  } else {
    text <- cell_text
  }

  if (!wrap(ht)[row, col]) {
    text <- sprintf("#box(breakable: false)[%s]", text)
  }

  text
}
