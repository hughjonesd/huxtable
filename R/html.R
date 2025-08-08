#' @export
#'
#' @rdname to_html
#'
print_html <- function(ht, ...) cat(huxtable_html_css(), to_html(ht, ...))


#' Create HTML representing a huxtable
#'
#' These functions print or return an HTML table. `print_html` also prepends a
#' `<style>` block defining basic CSS classes.
#'
#' @param ht A huxtable.
#' @param ... Arguments passed to methods. Not currently used.
#'
#' @return `to_html` returns an HTML string. `as_html` wraps `to_html` and returns an
#'   `htmltools::HTML` object. `print_html` prints the string and returns `NULL`.
#' @export
#'
#' @family printing functions
#'
#' @examples
#' ht <- hux(a = 1:3, b = letters[1:3])
#' to_html(ht)
#' as_html(ht)

#' @export
#'
#' @rdname to_html
#'
#' @return `print_notebook` prints HTML output suitable for use in an
#' RStudio interactive notebook.
print_notebook <- function(ht, ...) {
  html <- paste0(huxtable_html_css(), to_html(ht))
  print(rmarkdown::html_notebook_output_html(html))
}


huxtable_html_css <- function() {
  "<style>\n.huxtable {\n  border-collapse: collapse;\n  border: 0px;\n  margin-bottom: 2em;\n  margin-top: 2em;\n}\n.huxtable-cell {\n  vertical-align: top;\n  text-align: left;\n  white-space: normal;\n  border-style: solid;\n  border-width: 0pt;\n  padding: 6pt;\n  font-weight: normal;\n}\n.huxtable-header {\n  font-weight: bold;\n}\n</style>\n"
}

#' @export
#' @rdname to_html
to_html <- function(ht, ...) {
  check_positive_dims(ht)

  table_start <- build_table_style(ht)
  cols_html <- build_colgroup(ht)
  cell_html <- build_cell_html(ht)
  row_html <- build_row_html(ht, cell_html)

  paste0(table_start, cols_html, row_html, "</table>\n")
}

#' @export
#' @rdname to_html
as_html <- function(ht, ...) {
  htmltools::HTML(to_html(ht, ...))
}

#' Build opening table tag and caption for HTML output
#'
#' @param ht A huxtable.
#'
#' @return A string containing the opening `<table>` tag and optional caption.
#' @noRd
build_table_style <- function(ht) {
  width <- width(ht)
  width_string <- if (is.na(width)) {
    ""
  } else {
    if (is.numeric(width)) width <- paste0(width * 100, "%")
    sprintf("width: %s;", width)
  }

  margin_string <- switch(position(ht),
    "wrapleft"  = "margin-left: 0%; margin-right: 2em;",
    "wrapright" = "margin-left: 2em; margin-right: 0%;",
    "left"      = "margin-left: 0%; margin-right: auto;",
    "right"     = "margin-left: auto; margin-right: 0%;",
    "center"    = "margin-left: auto; margin-right: auto;"
  )

  height <- height(ht)
  height_string <- blank_where(
    {
      h <- if (is.numeric(height)) paste0(height * 100, "%") else height
      sprintf("height: %s;", h)
    },
    is.na(height)
  )

  float_string <- switch(position(ht),
    "wrapleft"  = "float: left;",
    "wrapright" = "float: right;",
    ""
  )

  lab <- make_label(ht)
  id_string <- if (is.na(lab)) "" else sprintf(" id=\"%s\"", lab)

  quarto_attribute <- if (getOption("huxtable.quarto_process", FALSE)) {
    ""
  } else {
    "data-quarto-disable-processing=\"true\" "
  }
  style <- paste(width_string, margin_string, height_string, float_string)
  style <- trimws(style)
  style_attr <- if (nzchar(style)) sprintf(' style="%s"', style) else ""
  table_start <- sprintf(
    '<table class="huxtable" %s%s%s>\n',
    quarto_attribute,
    style_attr,
    id_string
  )

  if (!is.na(cap <- make_caption(ht, lab, "html"))) {
    vpos <- if (grepl("top", caption_pos(ht))) "top" else "bottom"
    hpos <- get_caption_hpos(ht)

    if (!is.na(cap_width <- caption_width(ht))) {
      if (!is.na(as.numeric(cap_width))) {
        cap_width <- paste0(as.numeric(cap_width) * 100, "%")
      }
      cap_width <- sprintf("width: %s;", cap_width)
    } else {
      cap_width <- ""
    }
    cap <- sprintf(
      '<caption style="caption-side: %s; text-align: %s;%s">%s</caption>',
      vpos, hpos, cap_width, cap
    )
    table_start <- paste0(table_start, cap)
  }

  table_start
}

#' Build `<col>` tags for column widths
#'
#' @param ht A huxtable.
#'
#' @return A string of `<col>` tags.
#' @noRd
build_colgroup <- function(ht) {
  col_widths <- col_width(ht)
  empty_cw <- is.na(col_widths)
  if (is.numeric(col_widths)) col_widths <- sprintf("%s%%", col_widths * 100)
  cols_html <- sprintf(' style="width: %s"', col_widths)
  cols_html <- blank_where(cols_html, empty_cw)
  cols_html <- sprintf("<col%s>", cols_html)
  paste0(cols_html, collapse = "")
}

#' Build HTML for each table cell
#'
#' @param ht A huxtable.
#'
#' @return A character matrix containing HTML for each cell.
#' @noRd
build_cell_html <- function(ht) {
  display_cells <- display_cells(ht)

  rowspan <- rowspan(ht)
  rowspan <- blank_where(sprintf(' rowspan="%s"', rowspan), rowspan == 1)
  colspan <- colspan(ht)
  colspan <- blank_where(sprintf(' colspan="%s"', colspan), colspan == 1)

  valign <- sprintf("vertical-align: %s;", valign(ht))
  valign <- blank_where(valign, valign(ht) == "top")
  align <- sprintf("text-align: %s;", real_align(ht))
  align <- blank_where(align, real_align(ht) == "left")
  wrap <- ifelse(wrap(ht), "", "white-space: nowrap;")

  border_css <- compute_border_css(ht)
  border_css <- sub("^ ", "", border_css)
  border_css <- blank_where(
    border_css,
    border_css ==
      "border-style: solid solid solid solid; border-width: 0pt 0pt 0pt 0pt;"
  )

  add_pts <- function(x) if (is.numeric(x)) sprintf("%.4gpt", x) else x
  padding <- sprintf(
    "padding: %s %s %s %s;",
    add_pts(top_padding(ht)),
    add_pts(right_padding(ht)),
    add_pts(bottom_padding(ht)),
    add_pts(left_padding(ht))
  )
  padding <- blank_where(padding, padding == "padding: 6pt 6pt 6pt 6pt;")

  bg_color <- background_color(ht)
  bg_color <- format_color(bg_color)
  bg_color <- sprintf("background-color: rgb(%s);", bg_color)
  bg_color <- blank_where(bg_color, is.na(background_color(ht)))

  italic <- ifelse(italic(ht), "font-style: italic;", "")

  font <- sprintf("font-family: %s;", font(ht))
  font <- blank_where(font, is.na(font(ht)))
  font_size <- sprintf("font-size: %.4gpt;", font_size(ht))
  font_size <- blank_where(font_size, is.na(font_size(ht)))

  is_header <- matrix(FALSE, nrow(ht), ncol(ht))
  is_header[header_rows(ht), ] <- TRUE
  is_header[, header_cols(ht)] <- TRUE
  bold_inline <- ifelse(
    bold(ht) != is_header,
    ifelse(bold(ht), "font-weight: bold;", "font-weight: normal;"),
    ""
  )

  style <- trimws(paste(
    valign, align, wrap, border_css,
    padding, bg_color, bold_inline, italic, font, font_size
  ))
  style <- ifelse(style == "", "", paste0(' style="', style, '"'))

  th_td <- matrix("td", nrow(ht), ncol(ht))
  th_td[header_rows(ht), ] <- "th"
  th_td[, header_cols(ht)] <- "th"
  cell_class <- matrix('class="huxtable-cell"', nrow(ht), ncol(ht))
  cell_class[is_header] <- 'class="huxtable-cell huxtable-header"'
  cell_start <- sprintf("<%s %s%s%s%s>", th_td, cell_class, rowspan, colspan, style)
  cell_end <- sprintf("</%s>", th_td)
  contents <- clean_contents(ht, output_type = "html")

  rot <- rotation(ht)
  rot <- (rot %% 360) * -1 # HTML goes anticlockwise
  rot_div <- sprintf('<div style="transform: rotate(%.4gdeg); white-space: nowrap;">', rot)
  # special-case straight up/down to be handled by writing-mode.
  # this will probably break on non-LTR text, but before it was hard to use anyway.
  rot_div[rot == -270] <- sprintf('<div style="writing-mode: vertical-rl;">')
  rot_div[rot == -90] <- sprintf(
    '<div style="writing-mode: vertical-rl; transform: rotate(180deg);">'
  )

  rot_div_end <- rep("</div>", length(rot_div))
  rot_div <- blank_where(rot_div, rot == 0)
  rot_div_end <- blank_where(rot_div_end, rot == 0)

  color <- text_color(ht)
  color <- format_color(color)
  color_span <- sprintf('<span style="color: rgb(%s);">', color)
  color_span <- blank_where(color_span, is.na(text_color(ht)))
  color_span_end <- rep("</span>", length(color))
  color_span_end <- blank_where(color_span_end, is.na(text_color(ht)))

  cells_html <- paste0(
    cell_start, rot_div, color_span, contents,
    color_span_end, rot_div_end, cell_end
  )
  cells_html <- blank_where(cells_html, display_cells$shadowed)
  dim(cells_html) <- dim(ht)
  cells_html
}

#' Wrap cell HTML in table rows
#'
#' @param ht A huxtable.
#' @param cells_html A matrix of cell HTML as returned by `build_cell_html()`.
#'
#' @return A string containing the HTML for all rows.
#' @noRd
build_row_html <- function(ht, cells_html) {
  cells_html <- apply(cells_html, 1, paste0, collapse = "")
  row_heights <- row_height(ht)
  if (is.numeric(row_heights)) {
    row_heights <- 100 * row_heights / sum(row_heights)
    row_heights <- sprintf("%.3g%%", row_heights) # %3g prints max 1 decimal place
  }
  row_heights <- sprintf(' style="height: %s;"', row_heights)
  row_heights <- blank_where(row_heights, is.na(row_height(ht)))
  tr <- sprintf("<tr%s>\n", row_heights)
  row_html <- paste0(tr, cells_html, rep("</tr>\n", length(tr)))

  header_idx <- unname(which(header_rows(ht)))
  body_idx <- setdiff(seq_len(nrow(ht)), header_idx)

  if (!length(header_idx)) {
    cells_html <- paste0("<tbody>\n", paste0(row_html, collapse = ""), "</tbody>\n")
  } else if (identical(header_idx, seq_len(max(header_idx)))) {
    cells_html <- c(
      paste0("<thead>\n", paste0(row_html[header_idx], collapse = ""), "</thead>\n"),
      if (length(body_idx)) {
        paste0("<tbody>\n", paste0(row_html[body_idx], collapse = ""), "</tbody>\n")
      }
    )
    cells_html <- paste0(cells_html, collapse = "")
  } else {
    cells_html <- paste0(row_html, collapse = "")
  }

  cells_html <- paste0(tr, cells_html, rep("</tr>\n", length(tr)))
  paste0(cells_html, collapse = "")
}

#' Create border css for each cell
#'
#' This returns a matrix of border CSS. Cells with spans > 1 get the
#' borders from the correct position.
#'
#' @param ht A huxtable.
#'
#' @return A character array of border CSS, ending in a semicolon
#' @noRd
compute_border_css <- function(ht) {
  top_row <- c(row(ht))
  bottom_row <- top_row + c(rowspan(ht)) - 1
  left_col <- c(col(ht))
  right_col <- left_col + c(colspan(ht)) - 1

  dc <- display_cells(ht)
  dc <- as.matrix(dc[, c("row", "col", "end_row", "end_col")])
  # we don't use display_row because shadowed cells will be blanked anyway.
  top_matrix <- dc[, c("row", "col"), drop = FALSE]
  left_matrix <- top_matrix
  bottom_matrix <- dc[, c("end_row", "col"), drop = FALSE]
  right_matrix <- dc[, c("row", "end_col"), drop = FALSE]

  # We don't use get_visible_borders, because borders in the middle of a
  # span won't be used anyway.
  tb <- brdr_thickness(top_border(ht))[top_matrix]
  rb <- brdr_thickness(right_border(ht))[right_matrix]
  bb <- brdr_thickness(bottom_border(ht))[bottom_matrix]
  lb <- brdr_thickness(left_border(ht))[left_matrix]

  tbs <- top_border_style(ht)[top_matrix]
  rbs <- right_border_style(ht)[right_matrix]
  bbs <- bottom_border_style(ht)[bottom_matrix]
  lbs <- left_border_style(ht)[left_matrix]

  tbc <- top_border_color(ht)[top_matrix]
  rbc <- right_border_color(ht)[right_matrix]
  bbc <- bottom_border_color(ht)[bottom_matrix]
  lbc <- left_border_color(ht)[left_matrix]

  format_border_color_css <- function(col, pos) {
    x <- sprintf(" border-%s-color: rgb(%s);", pos, format_color(col))
    blank_where(x, is.na(col))
  }
  tbc <- format_border_color_css(tbc, "top")
  rbc <- format_border_color_css(rbc, "right")
  bbc <- format_border_color_css(bbc, "bottom")
  lbc <- format_border_color_css(lbc, "left")

  if (any(tbs == "double" & tb > 0 & tb < 3) ||
    any(rbs == "double" & rb > 0 & rb < 3) ||
    any(bbs == "double" & bb > 0 & bb < 3) ||
    any(lbs == "double" & lb > 0 & lb < 3)
  ) {
    warning("border_style set to \"double\" but border less than 3 points")
  }

  border_css <- sprintf(
    " border-style: %s %s %s %s; border-width: %.4gpt %.4gpt %.4gpt %.4gpt;",
    tbs, rbs, bbs, lbs, tb, rb, bb, lb
  )
  border_css <- paste0(border_css, sprintf("%s %s %s %s", tbc, rbc, bbc, lbc))

  return(border_css)
}
