
#' Convert a huxtable for Word/Powerpoint
#'
#' Huxtables can be converted to [flextable::flextable()] objects, for use in Word and Powerpoint documents.
#'
#' @param x A huxtable.
#' @param colnames_to_header Use huxtable column names as the header. If \code{FALSE}, the flextable
#'    will contain only a body and no header.
#' @param ... Not used.
#'
#' @return an object of class flextable.
#'
#' @details
#' With recent versions of "flextable" and Pandoc, huxtables can be automatically outputted
#' from rmarkdown `word_document` and/or `powerpoint_presentation` documents. (Powerpoint
#' presentations require pandoc version >= 2.4.0.)
#'
#' Properties are supported, with the following exceptions:
#' * Rotation of 0, 90 or 270 is supported.
#' * Non-numeric widths and heights are not supported. Table heights are treated
#'   as a proportion of 9 inches; table widths are treated as a proportion of 6
#'   inches. So e.g. `height(ht) <- 0.5` will give a height of 4.5 inches.
#' * Table wrap and table position are not supported.
#' * Border style "double" is not supported and becomes "solid".
#' * Captions are supported with recent versions of flextable, but not
#'   [caption_pos()] or [caption_width()].
#'
#' @section Challenge:
#'
#' Try to say `as_flextable.huxtable` ten times without pausing.
#'
#' @examplesIf rlang::is_installed("flextable")
#' ht <- hux(a = 1:3, b = 1:3)
#' ft <- as_flextable(ht)
#' \dontrun{
#'   my_doc <- officer::read_docx()
#'   my_doc <- flextable::body_add_flextable(
#'         my_doc, ft)
#'   print(my_doc, target =
#'         "path/to/my_doc.docx")
#' }
#' @aliases as_flextable.huxtable
#' @export
#'
as_flextable <- function(x, ...) UseMethod("as_flextable")


#' @rdname as_flextable
#' @export
as_flextable.huxtable <- function(x, colnames_to_header = FALSE, ...) {
  assert_package("as_flextable","flextable")
  flextable_version <- utils::packageVersion("flextable")

  cc <- clean_contents(x, output_type = "word")
  cc <- as.data.frame(cc, stringsAsFactors = FALSE)
  names(cc) <- make.names(names(cc)) # flextable does not like invalid names
  ft <- flextable::flextable(cc)
  if (! colnames_to_header) ft <- flextable::delete_part(ft, "header")
  if (any(markdown(x))) {
    assert_package("as_flextable", "ftExtra", version = "0.0.2")
    md_cell_refs <- which(markdown(x), arr.ind = TRUE)
    for (r in seq_len(nrow(md_cell_refs))) {
      hr <- md_cell_refs[r, 1]
      hc <- md_cell_refs[r, 2]
      cell <- cc[[hr, hc]]
      ft <- flextable::compose(ft, i = hr, j = hc,
            value = ftExtra::as_paragraph_md(cell, .from = "commonmark+strikeout"))
    }
  }

  rots <- list("0" = "lrtb", "90" = "btlr", "270" = "tbrl")
  dcells <- display_cells(x, all = FALSE)
  for (r in seq_len(nrow(dcells))) {
    dcell <- dcells[r, ]
    drow <- dcell$display_row
    dcol <- dcell$display_col
    if (dcell$colspan > 1 || dcell$rowspan > 1) {
      ft <- flextable::merge_at(ft, i = drow:dcell$end_row, j = dcol:dcell$end_col)
    }
    if (bold(x)[drow, dcol]) ft <- flextable::bold(ft, i = drow, j = dcol)
    if (italic(x)[drow, dcol]) ft <- flextable::italic(ft, i = drow, j = dcol)
    if (! is.na(fs <- font_size(x)[drow, dcol])) ft <- flextable::fontsize(ft, i = drow, j = dcol, size = fs)
    if (! is.na(tc <- text_color(x)[drow, dcol])) ft <- flextable::color(ft, i = drow, j = dcol, color = tc)
    if (! is.na(bgc <- background_color(x)[drow, dcol])) ft <- flextable::bg(ft, i = drow, j = dcol, bg = bgc)
    ft <- flextable::align(ft, i = drow, j = dcol, align = real_align(x)[drow, dcol])

    ft <- flextable::padding(ft, i = drow, j = dcol,
            padding.bottom = bottom_padding(x)[drow, dcol],
            padding.left   = left_padding(x)[drow, dcol],
            padding.right  = right_padding(x)[drow, dcol],
            padding.top    = top_padding(x)[drow, dcol]
          )
    bnames <- c("top", "left", "bottom", "right")
    bdrs  <- get_all_borders(x, drow, dcol)
    bcols <- get_all_border_colors(x, drow, dcol)
    bst   <- get_all_border_styles(x, drow, dcol)
    bst[bst == "double"] <- "solid"
    bcols[is.na(bcols)] <- "black"
    fp_borders <- lapply(bnames, function (x)
      officer::fp_border(color = bcols[[x]], width = bdrs[[x]], style = bst[[x]])
    )
    names(fp_borders) <- bnames
    ft <- flextable::surround(ft, i = drow:dcell$end_row, j = dcol:dcell$end_col,
            border.bottom = fp_borders$bottom,
            border.left   = fp_borders$left,
            border.right  = fp_borders$right,
            border.top    = fp_borders$top
          )
    rot <- as.character(rotation(x)[drow, dcol])
    valign <- valign(x)[drow, dcol]
    if (valign == "middle") valign <- "center"
    if (! (rot %in% names(rots))) {
      warning("flextable can only handle rotation of 0, 90 or 270")
      rot <- "0"
    }
    ft <- flextable::rotate(ft, i = drow, j = dcol, rotation = rots[[rot]], align = valign)
  }

  tw <- width(x)
  cw <- col_width(x)
  # if we only have col_width, set width to 0.5
  # if we only have width, use it and use equal col widths
  # if we have neither, use autofit
  # if we have both, multiply col_widths by width
  if (! is.numeric(tw) && is.numeric(cw) && ! any(is.na(cw))) tw <- 0.5
  if (! is.numeric(cw) || any(is.na(cw))) cw <- rep(1/ncol(x), ncol(x))
  if (is.numeric(tw) && ! is.na(tw)) {
    tw <- tw * 6 # flextable sizes are in inches
    cw <- cw * tw
    ft <- flextable::width(ft, width = cw)
  } else {
    ft <- flextable::autofit(ft)
  }

  th <- height(x)
  rh <- row_height(x)
  # if we have row heights, set height to 0.5
  # if we only have height, set row_heights to equal
  # otherwise, do nothing - don't call autofit again unless you overwrite
  if (! is.numeric(th) && is.numeric(rh) && ! any(is.na(rh))) th <- 0.5
  if (! is.numeric(rh) || any(is.na(rh))) rh <- rep(1/nrow(x), nrow(x))
  if (is.numeric(th) && ! is.na(th)) {
    rh <- rh * 9 # inches again, so this is roughly A4 with 2 1" margins
    rh <- rh * th
    ft <- flextable::height(ft, height = rh)
    if (flextable_version >= "0.5.7") ft <- flextable::hrule(ft, rule = "atleast")
  }

  # set caption
  if (!is.null(caption(x)) & !is.na(caption(x))) {
    if (flextable_version >= "0.5.5"){
      ft <- flextable::set_caption(ft, caption(x))
    } else {
      message("Use of table captions requires \"flextable\" package version >= 0.5.5.",
              "To upgrade, type:\n",
              " install.packages(\"flextable\")")
    }
  }

  ft
}
