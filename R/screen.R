
#' @export
#' @rdname to_screen
print_screen <- function(ht, ...) cat(to_screen(ht, ...))


#' Print a Huxtable on Screen
#'
#' @param ht A huxtable.
#' @param ... Passed on to \code{to_screen}.
#' @param borders Print horizontal borders, vertical borders, both or neither. May be abbreviated.
#'
#' @return \code{to_screen} returns a string. \code{print_screen} prints the string and returns \code{NULL}.
#'
#' @details
#' Only \code{colspan}, \code{rowspan}, \code{align} and \code{caption} properties are shown.
#'
#' @export
#' @family printing functions
#' @examples
#' ht <- huxtable(a = 1:5, b = 1:5)
#' ht <- set_all_borders(ht, 1:5, 1:2, 1)
#' print_screen(ht)
#' print_screen(ht, borders = 'neither')
to_screen  <- function (ht, ...) UseMethod('to_screen')


#' @export
#' @rdname to_screen
to_screen.huxtable <- function(ht, borders = c('both', 'horizontal', 'vertical', 'neither'), ...) {
  borders <- match.arg(borders)

  dc <- display_cells(ht)
  drow_mat <- as.matrix(dc[,c('display_row', 'display_col')])
  dc$contents <- apply(drow_mat, 1, function(rc) clean_contents(ht, rc[1], rc[2]))
  dc <- dc[order(dc$colspan),]
  border_chars   <- 3

  dc$widths <- nchar(dc$contents, type = 'width')
  # each extra row = 2 screen rows including border:
  dc$widths <- ceiling(dc$widths/(2 * dc$rowspan - 1))

  # widths of actual columns, not including borders
  max_widths <- rep(0, ncol(ht))
  for (r in 1:nrow(dc)) {
    width <- dc$width[r]
    cols <- with(dc[r,], display_col:(display_col + colspan - 1))
    if (sum(max_widths[cols]) < width) {
      max_widths[cols] <- pmax(max_widths[cols], ceiling(width/dc$colspan[r]))
    }
  }

  charmat         <- matrix(' ', 2 * nrow(ht) + 1, sum(max_widths) + (ncol(ht) + 1) * border_chars)
  border_cells    <- matrix(0, nrow(charmat), ncol(charmat)) # 0 = not a border; 1 = no border; > 1 = border
  corner_cells    <- matrix(FALSE, nrow(charmat), ncol(charmat))
  for (r in 1:nrow(dc)) {
    dcr <- dc[r,]
    if (dcr$shadowed) next

    drow <- dcr$display_row
    dcol <- dcr$display_col
    end_col <- dcol + dcr$colspan - 1
    end_row <- drow + dcr$rowspan - 1

    # content
    chars <- strsplit(dcr$contents, '')[[1]]
    char_row <- drow * 2
    total_width <- sum(max_widths[dcol:end_col]) + border_chars*(dcr$colspan - 1) # include internal borders
    start_char <- sum(max_widths[seq_len(dcol - 1)]) + border_chars * dcol
    while (length(chars) > 0) {
      idx <- 1:min(total_width, length(chars))
      space <- max(total_width - length(chars), 0)
      char_idx <- switch(align(ht)[drow, dcol],
        left  = start_char + idx,
        right = start_char + space + idx,
        center = start_char + floor(space/2) + idx
      )
      charmat[char_row, char_idx ] <- chars[idx]
      chars <- chars[-idx]
      char_row <- char_row + 1
    }

    bdr_idx_cols <- (start_char - 1):(start_char + total_width + 1)
    border_cells[drow * 2 - 1, bdr_idx_cols] <- pmax(border_cells[drow * 2 - 1, bdr_idx_cols],
      1 + top_border(ht)[drow, dcol])
    border_cells[end_row * 2 + 1, bdr_idx_cols] <- pmax(border_cells[end_row * 2 + 1, bdr_idx_cols],
      1 + bottom_border(ht)[drow, dcol])
    border_cells[(drow * 2):(end_row * 2), start_char - 1] <-
      pmax(border_cells[(drow * 2):(end_row * 2), start_char - 1],
        1 + left_border(ht)[drow, dcol])
    border_cells[(drow * 2):(end_row * 2), start_char + total_width + 2] <-
      pmax(border_cells[(drow * 2):(end_row * 2), start_char + total_width + 2],
        1 + left_border(ht)[drow, dcol])
    corner_cells[drow * 2 + c(-1, 1), c(start_char - 1, start_char + total_width + 2)] <- TRUE
  }

  charmat[border_cells > 0] <- ' '
  if (borders %in% c('both', 'horizontal')) charmat[border_cells > 1 & row(charmat) %% 2]   <- '-'
  if (borders %in% c('both', 'vertical'))   charmat[border_cells > 1 & ! row(charmat) %% 2] <- '|'
  if (borders == 'vertical') charmat[border_cells > 1 & row(charmat) %% 2 & corner_cells] <- '|'

  result <- apply(charmat, 1, paste0, collapse='')
  result <- paste0(result, collapse='\n')
  if (! is.na(cap <- caption(ht))) {
    result <- if (caption_pos(ht) == 'top') paste0(cap, '\n', result) else paste0(result, '\n', cap)
  }

  result
}


#' @export
#' @rdname to_md
print_md <- function(ht, ...) cat(to_md(ht, ...))


#' Create Markdown Representing A Huxtable
#'
#' @param ht A huxtable
#' @param max_width Max width in on-screen characters of the result
#'
#' @return \code{to_md} returns a string. \code{print_md} prints the string and returns
#' \code{NULL}.
#' @export
#'
#' @details
#' Only \code{align} and \code{caption} properties are used. The markdown format is
#' \code{multiline_tables}, see the \href{http://rmarkdown.rstudio.com/authoring_pandoc_markdown.html#tables}{rmarkdown documentation}.
#'
#' @family printing functions
#'
#' @examples
#' ht <- huxtable(a = 1:5, b = 1:5)
#' print_md(ht)
to_md <- function(ht, ...) UseMethod('to_md')

#' @export
#' @rdname to_md
to_md.huxtable <- function(ht, max_width = 80, ...) {
  cw <- col_width(ht)
  if (! is.numeric(cw) || anyNA(cw)) cw <- rep(1, ncol(ht))
  cw <- floor(cw/sum(cw) * (max_width - ncol(ht) + 1))
  width <- sum(cw) + ncol(ht) - 1
  ncharw <- function(x) nchar(x, type = 'width')
  dcells <- display_cells(ht)
  if (any(dcells$shadowed)) warning("Markdown cannot handle cells with colspan/rowspan > 1")
  dcells <- dcells[! dcells$shadowed, ]
  result <- strrep('-', width)
  result <- paste0(result, '\n')
  dcells$contents <- sapply(1:nrow(dcells), function (x) {
    clean_contents(ht, dcells[x, 'display_row'], dcells[x, 'display_col'], 'markdown')
  })

  align <- align(ht)
  if (any(apply(align, 2, function(x) length(unique(x)) > 1)))
        warning("Can't vary column alignment in markdown; using first row")
  align <- align[1,]

  # for every row:
  #   print the first cw characters of display cells in that row
  #   if there are any left over, start a new row
  for (myrow in 1:nrow(ht)) {
    row_chars <- rep('', ncol(ht))
    dcr <- dcells[dcells$display_row == myrow,]
    row_chars[ dcr$display_col ] <- dcr$contents
    while(any(ncharw(row_chars) > 0)) {
      for (i in 1:ncol(ht)) {
        chunk <- substring(row_chars[i], 1, cw[i])
        if ((extra <- cw[i] - ncharw(chunk)) > 0) {
          chunk <- switch(align[i],
            left = paste0(chunk, strrep(' ', extra)),
            center = paste0(strrep(' ', floor(extra/2)), chunk, strrep(' ', ceiling(extra/2))),
            right = paste0(strrep(' ', extra), chunk)
          )
        }
        result <- paste0(result, chunk)
        row_chars[i] <- substring(row_chars[i], cw[i] + 1)
        result <- paste0(result, ' ')
      }
      result <- paste0(result, '\n')
    }
    if (myrow == 1) {
      dash_row <- paste(strrep('-', cw), collapse = " ")
      result <- paste0(result, dash_row, '\n')
    } else {
      result <- paste0(result, '\n') # extra blank line for row
    }
  }
  result <- paste0(result, strrep('-', width), '\n')
  if (! is.na(cap <- caption(ht))) result <- paste0(result, 'Table: ', cap, '\n')
  result <- paste0(result, '\n')
  result
}



