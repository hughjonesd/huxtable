

#' @import assertthat
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
#' `colspan`, `rowspan`, `align` and `caption` properties are shown. If the `crayon`
#' package is installed, output will be colorized (and contents bolded or italicized) by default;
#' this will work in recent daily builds of RStudio as of October 2017.
#'
#' @export
#' @family printing functions
#' @examples
#' ht <- huxtable(a = 1:5, b = 1:5, add_colnames = TRUE)
#' ht <- set_all_borders(ht, 1:6, 1:2, 1)
#' right_border(ht)[,1] <- left_border(ht)[,2] <- 0
#' align(ht)[1,] <- 'left'
#' print_screen(ht)
to_screen  <- function (ht, ...) UseMethod('to_screen')


#' @export
#' @rdname to_screen
to_screen.huxtable <- function (
        ht,
        min_width = ceiling(getOption('width') / 6),
        max_width = getOption('width', Inf),
        compact   = TRUE,
        colnames  = TRUE,
        color     = getOption('huxtable.color_screen', default = TRUE),
        ...
      ) {
  assert_that(is.number(min_width), is.number(max_width), is.flag(compact), is.flag(colnames), is.flag(color))
  if (color && ! requireNamespace('crayon', quietly = TRUE)) {
    warning('Cannot print huxtable in color as `crayon` package is not installed. Try `install.packages("crayon")`.',
          'To avoid seeing this message in future, set `options(huxtable.color_screen = FALSE)`.')
    color <- FALSE
  }

  charmat_data <- character_matrix(ht, inner_border_h = 3, outer_border_h = 2, inner_border_v = 1, outer_border_v = 1,
        min_width = min_width, max_width = max_width, color = color)
  charmat <- charmat_data$charmat
  border_rows <- charmat_data$border_rows
  border_cols <- charmat_data$border_cols
  border_cols[-1] <- border_cols[-1] + 1 # middle of 3 for interior, last of 2 for last outer

  borders <- collapsed_borders(ht)
  border_mat <- matrix(1L, nrow = nrow(charmat), ncol = ncol(charmat))
  # converts a row/col number to a sequence of charmat row/col numbers for the relevant *column/row*
  index_rows <- lapply(seq_len(nrow(ht)), function (x) seq(border_rows[x], border_rows[x + 1] - 1))
  index_cols <- lapply(seq_len(ncol(ht)), function (x) seq(border_cols[x], border_cols[x + 1] - 1))
  # borders$vert is row, col+1; $horiz is row+1, col
  for (i in seq_len(nrow(ht) + 1)) for (j in seq_len(ncol(ht) + 1)) {
    if (i <= nrow(ht)) {
      ir <- index_rows[[i]]
      # has a line above:
      border_mat[ ir, border_cols[j] ]     <- border_mat[ ir, border_cols[j] ]     + 1L * (borders$vert[i, j] > 0)
      # has a line below:
      border_mat[ ir + 1, border_cols[j] ] <- border_mat[ ir + 1, border_cols[j] ] + 2L * (borders$vert[i, j] > 0)
    }
    if (j <= ncol(ht)) {
      ic <- index_cols[[j]]
      # on right:
      border_mat[ border_rows[i], ic ]    <- border_mat[ border_rows[i], ic ]     + 4L * (borders$horiz[i, j] > 0)
      # on left:
      border_mat[ border_rows[i], ic + 1] <- border_mat[ border_rows[i], ic + 1 ] + 8L * (borders$horiz[i, j] > 0)
    }
  }

  pipe_chars <- c(NA,
        '\u2502', '\u2502', '\u2502', '\u2500',
        '\u250c', '\u2514', '\u251c', '\u2500',
        '\u2510', '\u2518', '\u2524', '\u2500',
        '\u252c', '\u2534', '\u253c')
  border_mat[] <- pipe_chars[border_mat]
  charmat[ ! is.na(border_mat) ] <- border_mat[ ! is.na(border_mat) ]

  if (color) {
    bcolors <- collapsed_border_colors(ht)
    unique_cols <- unique(na.omit(unlist(bcolors)))
    col_funs <- lapply(unique_cols, crayon::make_style)
    names(col_funs) <- unique_cols
    for (i in seq_len(nrow(ht) + 1)) for (j in seq_len(ncol(ht) + 1)) {
      if (i <= nrow(ht)) {
        # colour vertical borders:
        ir <- index_rows[[i]]
        color_fun <- col_funs[[ bcolors$vert[i, j] ]]
        if (! is.na(bcolors$vert[i, j])) charmat[ ir, border_cols[j] ] <- color_fun(charmat[ ir, border_cols[j] ])
      }
      if (j <= ncol(ht)) {
        # horizontal borders:
        ic <- c(index_cols[[j]], max(index_cols[[j]]) + 1) # rows extend a little bit to cover ends
        color_fun <- col_funs[[ bcolors$horiz[i, j] ]]
        if (! is.na(bcolors$horiz[i, j])) charmat[ border_rows[i], ic ] <- color_fun(charmat[ border_rows[i], ic ])
      }
    }
  }

  if (compact) {
    empty_borders <- apply(charmat, 1, function (x)
          all(grepl(' ', x, fixed = TRUE) | grepl('\u2502', x, fixed = TRUE)))
    empty_borders <- intersect(border_rows, which(empty_borders))
    # length statement necessary otherwise we end up doing charmat[ - integer(0), ] and getting nothing
    if (length(empty_borders) > 0) charmat <- charmat[ - empty_borders, , drop = FALSE]
  }

  result <- paste(apply(charmat, 1, paste0, collapse = ''), collapse = '\n')
  if (! is.na(cap <- caption(ht))) {
    poss_pos <- c('left', 'center', 'right')
    hpos <- if (any(found <- sapply(poss_pos, grepl, x = caption_pos(ht)))) poss_pos[found] else position(ht)
    if (ncharw(cap) > max_width) cap <- strwrap(cap, max_width)
    cap <- paste0(str_pad(cap, hpos, ncol(charmat)), collapse = '\n')
    result <- if (grepl('top', caption_pos(ht))) paste0(cap, '\n', result) else paste0(result, '\n', cap)
  }
  if (colnames && any(nchar(colnames(ht)) > 0)) {
    colnames_text <- paste0('Column names: ', paste(colnames(ht), collapse = ', '))
    colnames_text <- strwrap(colnames_text, max_width)
    colnames_text <- paste0(colnames_text, collapse = '\n')
    result <- paste0(result, '\n\n', colnames_text, '\n')
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
#' @param max_width Maximum width in on-screen characters of the result. Overrides `min_width`.
#' @param ...       Arguments passed to methods.
#'
#' @return `to_md` returns a string. `print_md` prints the string and returns
#' `NULL`.
#' @export
#'
#' @details
#' Only `align` and `caption` properties are used. The markdown format is
#' `multiline_tables`, see the \href{http://rmarkdown.rstudio.com/authoring_pandoc_markdown.html#tables}{rmarkdown documentation}.
#'
#' @family printing functions
#'
#' @examples
#' ht <- huxtable(a = 1:5, b = 1:5)
#' print_md(ht)
to_md <- function(ht, ...) UseMethod('to_md')


#' @export
#' @rdname to_md
to_md.huxtable <- function(ht, header = TRUE, min_width = getOption('width') / 4, max_width = 80, ...) {
  assert_that(is.flag(header), is.number(min_width), is.number(max_width))
  if (any(colspan(ht) > 1 | rowspan(ht) > 1)) warning("Markdown cannot handle cells with colspan/rowspan > 1")
  align <- real_align(ht)
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

  result <- paste(apply(charmat, 1, paste0, collapse = ''), collapse = '\n')
  result <- paste0(result, '\n\n')
  if (! is.na(cap <- caption(ht))) result <- paste0(result, 'Table: ', cap, '\n')

  result
}


# function to calculate text column widths, wrap huxtable text accordingly, and return a matrix of characters, without
# borders
character_matrix <- function (ht, inner_border_h, inner_border_v, outer_border_h, outer_border_v,
      min_width, max_width = Inf, color = FALSE) {
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
  # return 0 for empty cells. We don't use "\\s"; non-breaking spaces, returned by decimal_pad, are excluded
  # this is risky because we might screw up some locales...
  max_word_widths <- sapply(lapply(strsplit(dc$contents, "(\t|\n|\r|\v )"), ncharw), function (x)  max(c(0, x)))
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
    # strwrap treats non-breaking spaces differently than breaking spaces; this can fuck things up
    # with decimal padding. So all END spaces become non-breaking.
    while (! identical(new <- gsub('( |\u00a0)$', '\u00a0', dcell$contents), dcell$contents)) dcell$contents <- new
    strings <- strwrap(dcell$contents, width = width + 1) # for the + 1 see ?strwrap
    # some strings may still be longer than width,
    strings <- unlist(lapply(strings, function (x) {
      while (any(ncharw(x) > width)) {
        lx <- length(x)
        last <- x[lx]
        last <- c(substring(last, 1, width), substring(last, width + 1))
        x[lx:(lx + 1)] <- last
      }
      x
    }))
    strings <- str_pad(strings, real_align(ht)[ dcell$display_row, dcell$display_col ], width)
    dc$strings[[r]] <- strings
  }
  dc$text_height <- sapply(dc$strings, length)
  dc$text_width <- sapply(dc$strings, function (x) max(ncharw(x)))

  # row heights as widths: start at 0 and increase it if its too little, sharing equally among relevant cols
  dc <- dc[order(dc$rowspan), ]
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
    style <- if (color) make_cell_style(ht, drow, dcol) else identity
    rows <- seq(starting_rows[drow], starting_rows[drow] + dcell$text_height - 1)
    cols <- seq(starting_cols[dcol], starting_cols[dcol] + dcell$text_width - 1)
    charmat[rows, cols] <- matrix(style(string_letters), length(rows), length(cols), byrow = TRUE)
  }

  list(charmat = charmat, border_rows = border_rows, border_cols = border_cols)
}


make_cell_style <- function (ht, row, col) {
  tc         <- text_color(ht)[row, col]
  bgc        <- background_color(ht)[row, col]
  bold       <- bold(ht)[row, col]
  italic     <- italic(ht)[row, col]

  maybe_combine_style <- function (style, style2) if (is.null(style)) style2 else crayon::combine_styles(style, style2)
  style <- NULL
  if (bold) style <- crayon::bold
  if (italic) style <- maybe_combine_style(style, crayon::italic)
  if (! is.na(tc)) style <- maybe_combine_style(style, crayon::make_style(tc))
  if (! is.na(bgc)) style <- maybe_combine_style(style, crayon::make_style(bgc, bg = TRUE))
  if (is.null(style)) style <- identity

  style
}


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
