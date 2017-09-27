
#' @export
#' @rdname to_screen
print_screen <- function(ht, ...) cat(to_screen(ht, ...))


#' Print a huxtable on screen
#'
#' @param ht A huxtable.
#' @param ... Passed on to \code{to_screen}.
#' @param blank   Character to print for cell divisions with no border.
#' @param min_width Minimum width in on-screen characters of the result.
#' @param max_width Maximum width in on-screen characters of the result. Overrides \code{min_width}.
#' @param colnames Whether or not to print colum names.
#'
#' @return \code{to_screen} returns a string. \code{print_screen} prints the string and returns \code{NULL}.
#'
#' @details
#' Only \code{colspan}, \code{rowspan}, \code{align} and \code{caption} properties are shown.
#'
#' @export
#' @family printing functions
#' @examples
#' ht <- huxtable(a = 1:5, b = 1:5, add_colnames = TRUE)
#' ht <- set_all_borders(ht, 1:6, 1:2, 1)
#' right_border(ht)[,1] <- left_border(ht)[,2] <- 0
#' align(ht)[1,] <- 'left'
#' print_screen(ht)
#' print_screen(ht, blank = '.')
to_screen  <- function (ht, ...) UseMethod('to_screen')


#' @export
#' @rdname to_screen
to_screen.huxtable <- function (ht, blank = ' ', min_width = ceiling(getOption('width') / 4), max_width = Inf,
      colnames = TRUE, ...) {
  charmat_data <- character_matrix(ht, inner_border_h = 3, outer_border_h = 2, inner_border_v = 1, outer_border_v = 1,
        min_width = min_width, max_width = max_width)
  charmat <- charmat_data$charmat
  border_rows <- charmat_data$border_rows
  border_cols <- charmat_data$border_cols
  border_cols[-1] <- border_cols[-1] + 1 # middle of 3 for interior, last of 2 for last outer

  dc <- display_cells(ht, all = FALSE)
  # the three for loops below ensure that borders are collapsed correctly without regard to order
  for (r in seq_len(nrow(dc))) {
    dcell <- dc[r, ]
    drow <- dcell$display_row
    dcol <- dcell$display_col
    end_row <- dcell$end_row + 1 # carry on to next row/col
    end_col <- dcell$end_col + 1
    bdrs <- get_all_borders(ht, drow, dcol)
    charmat[ border_rows[drow]:border_rows[end_row], border_cols[dcol] ]     <- blank
    charmat[ border_rows[drow]:border_rows[end_row], border_cols[end_col] ]  <- blank
    charmat[ border_rows[drow], border_cols[dcol]:border_cols[end_col] ]     <- blank
    charmat[ border_rows[end_row], border_cols[dcol]:border_cols[end_col] ]  <- blank
  }
  for (r in seq_len(nrow(dc))) {
    dcell <- dc[r, ]
    drow <- dcell$display_row
    dcol <- dcell$display_col
    end_row <- dcell$end_row + 1 # carry on to next row/col
    end_col <- dcell$end_col + 1
    bdrs <- get_all_borders(ht, drow, dcol)
    if (bdrs$left > 0)   charmat[ border_rows[drow]:border_rows[end_row], border_cols[dcol] ]     <- '|'
    if (bdrs$right > 0)  charmat[ border_rows[drow]:border_rows[end_row], border_cols[end_col] ]  <- '|'
  }
  for (r in seq_len(nrow(dc))) {
    dcell <- dc[r, ]
    drow <- dcell$display_row
    dcol <- dcell$display_col
    end_row <- dcell$end_row + 1 # carry on to next row/col
    end_col <- dcell$end_col + 1
    bdrs <- get_all_borders(ht, drow, dcol)
    if (bdrs$top > 0)    charmat[ border_rows[drow], border_cols[dcol]:border_cols[end_col] ]     <- '-'
    if (bdrs$bottom > 0) charmat[ border_rows[end_row], border_cols[dcol]:border_cols[end_col] ]  <- '-'
  }

  result <- paste((apply(charmat, 1, paste0, collapse='')), collapse='\n')
  if (! is.na(cap <- caption(ht))) {
    poss_pos <- c('left', 'center', 'right')
    hpos <- if (any(found <- sapply(poss_pos, grepl, x = caption_pos(ht)))) poss_pos[found] else position(ht)
    cap <- str_pad(cap, hpos, ncol(charmat))
    result <- if (grepl('top', caption_pos(ht))) paste0(cap, '\n', result) else paste0(result, '\n', cap)
  }
  if (colnames) {
    result <- paste0(result, '\n\n', 'Column names: ', paste(colnames(ht), collapse = ', '))
  }

  result
}


#' @export
#' @rdname to_md
print_md <- function(ht, ...) cat(to_md(ht, ...))


#' Create Markdown representing a huxtable
#'
#' @param ht        A huxtable.
#' @param header    Logical. Print the first row as a header?
#' @param min_width Minimum width in on-screen characters of the result.
#' @param max_width Maximum width in on-screen characters of the result. Overrides \code{min_width}.
#' @param ...       Arguments passed to methods.
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
to_md.huxtable <- function(ht, header = TRUE, min_width = getOption('width') / 4, max_width = 80,...) {
  if (any(colspan(ht) > 1 | rowspan(ht) > 1)) warning("Markdown cannot handle cells with colspan/rowspan > 1")
  align <- align(ht)
  if (any(apply(align, 2, function(x) length(unique(x)) > 1)))
    warning("Can't vary column alignment in markdown; using first row")
  ht <- set_align(ht, align[1, ], byrow = TRUE)

  charmat_data <- character_matrix(ht, inner_border_h = 1, outer_border_h = 1, inner_border_v = 1, outer_border_v = 1,
        min_width = min_width, max_width = max_width)
  charmat <- charmat_data$charmat
  border_rows <- charmat_data$border_rows
  border_cols <- charmat_data$border_cols
  # if you have a header, you need a whole line of dashes. Otherwise just to indicate columns
  dash_cols <- if (header) seq_len(ncol(charmat)) else - (border_cols)
  charmat[c(1, nrow(charmat)), dash_cols] <- '-'
  if (header) {
    omit_cols <- border_cols[ - c(1, ncol(ht) + 1)] # skip interior borders
    charmat[border_rows[2], - omit_cols ] <- '-'
  }

  result <- paste((apply(charmat, 1, paste0, collapse='')), collapse='\n')
  result <- paste0(result, '\n\n')
  if (! is.na(cap <- caption(ht))) result <- paste0(result, 'Table: ', cap, '\n')

  result
}


# function to calculate text column widths, wrap huxtable text accordingly, and return a matrix of characters, without
# borders
character_matrix <- function (ht, inner_border_h, inner_border_v, outer_border_h, outer_border_v,
      min_width, max_width = Inf) {
  dc <- display_cells(ht, all = FALSE)
  dc <- dc[order(dc$colspan), ]
  contents <- clean_contents(ht, type = 'screen')
  drow_mat <- as.matrix(dc[, c('display_row', 'display_col')])
  dc$contents <- contents[drow_mat]
  cw <- col_width(ht)
  if (! is.numeric(cw) || anyNA(cw)) cw <- rep(1, ncol(ht))
  cw <- cw / sum(cw)

  min_widths <- ceiling(min_width * cw)
  widths <- min_widths

  content_widths <- ncharw(dc$contents)
  max_word_widths <- sapply(lapply(strsplit(dc$contents, "\\s"), ncharw), function (x) {
    if (length(x) > 0) max(x) else 0 # return 0 for empty cells
  })
  for (r in seq_len(nrow(dc))) {
    width <- if (wrap(ht)[ dc$display_row[r], dc$display_col[r] ]) max_word_widths[r] else content_widths[r]
    cols <- seq(dc$display_col[r], dc$end_col[r])
    # allows for width of interior borders if a cell spans multiple columns
    if (sum(widths[cols]) + inner_border_h * (dc$colspan[r] - 1) < width) {
      widths[cols] <- pmax(widths[cols], ceiling(width / dc$colspan[r]))
    }
  }

  max_widths <- floor(cw * (max_width - 2 * outer_border_h - (ncol(ht) - 1) * inner_border_h))
  widths <- pmin(widths, max_widths)

  dc$strings <- vector(nrow(dc), mode = 'list')
  for (r in seq_len(nrow(dc))) {
    dcell <- dc[r, ]
    col <- dcell$display_col
    end_col <- dcell$end_col
    width <- sum(widths[col:end_col])
    strings <- strwrap(dcell$contents, width = width + 1) # for the + 1 see ?strwrap
    # some strings may still be longer than width,
    strings <- unlist(lapply(strings, function (x) {
      while (any(ncharw(x) > width)) {
        lx <- length(x)
        last <- x[lx]
        last <- c(substring(last, 1, width), substring(last, width + 1))
        x[lx:(lx+1)] <- last
      }
      x
    }))
    strings <- str_pad(strings, align(ht)[ dcell$display_row, dcell$display_col ], width)
    dc$strings[[r]] <- strings
  }
  dc$text_height <- sapply(dc$strings, length)
  dc$text_width <- sapply(dc$strings, function (x) max(ncharw(x)))


  # row heights as widths: start at 0 and increase it if its too little, sharing equally among relevant cols
  dc <- dc[order(dc$rowspan),]
  heights <- rep(1, nrow(ht))
  for (r in seq_len(nrow(dc))) {
    dcell <- dc[r, ]
    rows <- seq(dcell$display_row, dcell$end_row)
    if (sum(heights[rows]) + inner_border_v * (dcell$rowspan - 1) < dcell$text_height) {
      heights[rows] <- pmax(heights[rows], ceiling(dcell$text_height / dcell$rowspan))
    }
  }


  border_widths <- c(outer_border_h, rep(inner_border_h, ncol(ht) - 1), outer_border_h)
  # width of outer border, then cells + following border:
  all_widths    <- border_widths + c(0, widths)
  starting_cols <- cumsum(all_widths[seq_len(ncol(ht))]) + 1
  border_cols   <- c(starting_cols, sum(all_widths) + 1) - border_widths

  border_heights <- c(outer_border_v, rep(inner_border_v, nrow(ht) - 1), outer_border_v)
  all_heights    <- border_heights + c(0, heights)
  starting_rows <- cumsum(all_heights[seq_len(nrow(ht))]) + 1
  border_rows   <- c(starting_rows, sum(all_heights) + 1) - border_heights
  charmat <- matrix(' ', sum(all_heights), sum(all_widths))

  for (r in seq_len(nrow(dc))) {
    dcell <- dc[r, ]
    string_letters <- unlist(strsplit(dcell$strings[[1]], ''))
    drow <- dcell$display_row
    dcol <- dcell$display_col
    rows <- seq(starting_rows[drow], starting_rows[drow] + dcell$text_height - 1)
    cols <- seq(starting_cols[dcol], starting_cols[dcol] + dcell$text_width - 1)
    charmat[rows, cols] <- matrix(string_letters, length(rows), length(cols), byrow = TRUE)
  }

  list(charmat = charmat, border_rows = border_rows, border_cols = border_cols)
}


ncharw <- function (x) nchar(x, type = 'width')


str_pad <- function (strings, align, strlen) {
  stencils <- rep(str_rep(' ', strlen), length(strings))

  if (align == 'left') {
    start <- 1
    stop <- ncharw(strings)
  } else if (align == 'center') {
    start <- floor( (ncharw(stencils) - ncharw(strings)) / 2) + 1
    stop  <- ncharw(stencils) - start + 1
  } else {
    start <- ncharw(stencils) - ncharw(strings) + 1
    stop  <- ncharw(stencils)
  }
  substr(stencils, start, stop) <- strings

  stencils
}


str_rep <- function(x, times) {
  mapply(function (s, t) paste0(rep(s, t), collapse = ''), x, times)
}
