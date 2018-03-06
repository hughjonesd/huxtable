


#' @rdname as_flextable
#'
#' @export
as_FlexTable <- function(x, ...) {
  warning("as_FlexTable is deprecated; redirecting to as_flextable")
  as_flextable(x, ...)
}


#' Convert a huxtable for Word/Powerpoint
#'
#' Huxtables can be converted to [flextable::flextable()] objects, for use in Word and Powerpoint documents.
#'
#' @param x A huxtable.
#' @param ... Not used.
#'
#' @return an object of class flextable.
#'
#' @details
#'
#' Note: you can't use flextable Word output within rmarkdown. Instead you have to write the Word file
#' yourself. See [officer::read_docx()].
#'
#' `as_FlexTable` is deprecated and calls `as_flextable` with a warning.
#'
#' Properties are supported, with the following exceptions:
#' \itemize{
#'   \item Rotation of 0, 90 or 270 is supported.
#'   \item Non-numeric column widths and row heights are not supported.
#'   \item Table height, wrap, captions and table position are not supported.
#' }
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
#'   my_doc <- flextable::body_add_flextable(my_doc, ft)
#'   print(my_doc, target = "path/to/my_doc.docx")
#' }
#' @aliases as_flextable.huxtable
#' @export
#'
as_flextable <- function(x, ...) UseMethod('as_flextable')


#' @rdname as_flextable
#' @export
as_flextable.huxtable <- function(x, ...) {
  if (! requireNamespace('flextable')) stop('as_flextable requires the flextable package. To install, type:\n',
    'install.packages("flextable")')

  cc <- clean_contents(x, type = 'word')
  cc <- as.data.frame(cc)
  names(cc) <- make.names(names(cc)) # flextable does not like invalid names
  ft <- flextable::flextable(cc)


  if (is.numeric(rh <- row_height(x))) ft <- flextable::height(ft, height = rh)
  if (is.numeric(cw <- col_width(x)))  ft <- flextable::width(ft, width = cw)

  rots <- list('0' = 'lrtb', '90' = 'btlr', '270' = 'tbrl')
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
    ft <- flextable::align(ft, i = drow, j = dcol, align = align(x)[drow, dcol])

    ft <- flextable::padding(ft, i = drow, j = dcol,
            padding.bottom = bottom_padding(x)[drow, dcol],
            padding.left   = left_padding(x)[drow, dcol],
            padding.right  = right_padding(x)[drow, dcol],
            padding.top    = top_padding(x)[drow, dcol]
          )
    bcols <- get_all_border_colors(x, drow, dcol)
    bdrs  <- get_all_borders(x, drow, dcol)
    bcols[is.na(bcols)] <- 'black'
    ft <- flextable::border(ft, i = drow, j = dcol,
            border.bottom = officer::fp_border(color = bcols$bottom, width = bdrs$bottom),
            border.left   = officer::fp_border(color = bcols$left,   width = bdrs$left),
            border.right  = officer::fp_border(color = bcols$right,  width = bdrs$right),
            border.top    = officer::fp_border(color = bcols$top,    width = bdrs$top)
          )
    rot <- as.character(rotation(x)[drow, dcol])
    valign <- valign(x)[drow, dcol]
    if (valign == 'middle') valign <- 'center'
    if (! (rot %in% names(rots))) {
      warning("flextable can only handle rotation of 0, 90 or 270")
      rot <- 0
    }
    ft <- flextable::rotate(ft, i = drow, j = dcol, rotation = rots[[rot]], align = valign)
  }

  ft
}
