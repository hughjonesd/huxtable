


#' @rdname as_flextable
#'
#' @export
as_FlexTable <- function(x, ...) {
  .Deprecated(new = "as_flextable", package = "huxtable")
  as_flextable(x, ...)
}


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
#' `as_FlexTable` is deprecated and calls `as_flextable` with a warning.
#'
#' Properties are supported, with the following exceptions:

#' * Rotation of 0, 90 or 270 is supported.
#' * Non-numeric column widths and row heights are not supported.
#' * Table height, wrap, captions and table position are not supported.
#' * Border style "double" is not supported and becomes "solid".
#
#'
#' @section Challenge:
#'
#' Try to say `as_flextable.huxtable` ten times without pausing.
#'
#' @examples
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
as_flextable <- function(x, ...) flextable::as_flextable(x, ...)


#' @rdname as_flextable
#' @export
as_flextable.huxtable <- function(x, colnames_to_header = FALSE, ...) {
  assert_package("as_flextable","flextable")

  cc <- clean_contents(x, type = "word")
  cc <- as.data.frame(cc, stringsAsFactors = FALSE)
  names(cc) <- make.names(names(cc)) # flextable does not like invalid names
  ft <- flextable::flextable(cc)
  if (! colnames_to_header) ft <- flextable::delete_part(ft, "header")

  if (is.numeric(rh <- row_height(x))) ft <- flextable::height(ft, height = rh)
  if (is.numeric(cw <- col_width(x)))  ft <- flextable::width(ft, width = cw)

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
    ft <- flextable::border(ft, i = drow:dcell$end_row, j = dcol:dcell$end_col,
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

  ft
}
