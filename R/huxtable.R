
#' @import knitr
#' @import rmarkdown
#' @import xtable
NULL


#' Create and Test for Huxtable Objects
#'
#' \code{huxtable}, or \code{hux} for short, creates a huxtable object.
#' \code{as_huxtable} and \code{as_hux} convert an object to a huxtable.
#' \code{is_huxtable} and \code{is_hux} test if their argument is a huxtable.
#'
#' @param ... Named list of values, as for \code{\link{data.frame}}.
#' @param col_names If \code{TRUE}, a first row of column names will be added to the huxtable.
#' @param row_names If \code{TRUE}, a first column of row names, named "rownames", will be added to the huxtable.
#' #' @param ... Named list of values, as for \code{\link{data.frame}}.
#'
#' @return An object of class \code{huxtable}.
#' @export
#'
#' @examples
#' ht <- huxtable(column1 = 1:5, column2 = letters[1:5])
huxtable <- function (..., col_names = FALSE, row_names = FALSE) {
  ht <- data.frame(..., stringsAsFactors = FALSE)

  # order matters here. We want original rownames, not anything else.
  cn <- colnames(ht)
  if (row_names) cn <- c('', cn)
  if (row_names) ht <- cbind(rownames = rownames(ht), ht, stringsAsFactors = FALSE)
  if (col_names) ht <- rbind(cn, ht, stringsAsFactors = FALSE)

  as_huxtable(ht)
}

#' @export
#' @rdname huxtable
hux <- huxtable


#' @param x A suitable object. Conversion methods exist for data frames, tables, matrices and (most) vectors.
#'
#' @export
#'
#' @examples
#' dfr <- data.frame(a = 1:5, b = letters[1:5], stringsAsFactors = FALSE)
#' as_huxtable(dfr)
#'
#' @rdname huxtable
as_huxtable <- function(x, ...) UseMethod('as_huxtable')

#' @export
#' @rdname huxtable
as_hux <- as_huxtable

#' @export
as_huxtable.default <- function (x, ...) {
  x <- as.data.frame(x) # deletes attributes
  for (att in setdiff(huxtable_cell_attrs, 'number_format')) {
    attr(x, att) <- matrix(NA, nrow(x), ncol(x))
  }
  for (att in huxtable_col_attrs) {
    attr(x, att) <- rep(NA, ncol(x))
  }
  for (att in huxtable_row_attrs) {
    attr(x, att) <- rep(NA, nrow(x))
  }
  for (att in huxtable_table_attrs) {
    attr(x, att) <- NA
  }
  attr(x, 'number_format') <- matrix(list(NA), nrow(x), ncol(x))
  for (att in names(huxtable_default_attrs)) {
    attr(x, att)[] <- huxtable_default_attrs[[att]] # [[ indexing matters here
  }

  class(x) <- c('huxtable', class(x))
  x
}

#' @export
as_huxtable.huxtable <- function(x, ...) x

#' @export
as_huxtable.table <- function(x, ...) {
  as_huxtable(as.matrix(x, ...))
}

#' @export
as_huxtable.numeric <- function (x, ...) {
  # use default otherwise matrix has class e.g. c('matrix', 'numeric') so we recurse
  as_huxtable.default(as.matrix(x, ...))
}

#' @export
as_huxtable.character <- as_huxtable.numeric

#' @export
as_huxtable.logical   <- as_huxtable.numeric

#' @export
as_huxtable.complex   <- as_huxtable.numeric

#' @export
#' @rdname huxtable
is_huxtable <- function(x) inherits(x, 'huxtable')

#' @export
#' @rdname huxtable
is_hux <- is_huxtable

#' Subset a huxtable
#'
#' @param x A huxtable.
#' @param i Rows to select.
#' @param j Columns to select.
#' @param drop Not used.
#'
#' @return A huxtable.
#' @export
#' @rdname extract-methods
#' @details
#' \code{[} always returns a new huxtable object, while \code{$} and \code{[[} simply
#' return a vector of data.
#' For the replacement function, if \code{value} is a huxtable, then its cell attributes will be
#' copied into \code{x}. In addition, if \code{nrow(value) == nrow(x)}, then column attributes
#' will be copied into \code{x} as appropriate, and if  \code{ncol(value) == ncol(x)}, then
#' row attributes will be copied.
#' @examples
#' ht <- huxtable(a = 1:3, b = letters[1:3])
#' rowspan(ht)[2,1] <- 2
#' ht[1:2,]
`[.huxtable` <- function (x, i, j, drop = FALSE) {
  ss <- as.data.frame(unclass(x), stringsAsFactors = FALSE)[i, j, drop]
  if (! missing(i) && is.character(i)) i <- which(rownames(ht) %in% i)
  if (! missing(j) && is.character(j)) j <- which(colnames(ht) %in% j)
  for (att in huxtable_cell_attrs) {
    attr(ss, att) <- attr(x, att)[i, j, drop = drop]
  }
  for (att in huxtable_col_attrs) {
    attr(ss, att) <- attr(x, att)[j]
  }
  for (att in huxtable_row_attrs) {
    attr(ss, att) <- attr(x, att)[i]
  }
  for (att in huxtable_table_attrs) {
    attr(ss, att) <- attr(x, att)
  }
  dcells <- display_cells(x)
  # check for dcells where row > nrow(ss) or col > ncol(ss) and display_row, display_col are within ss
  cut <- (dcells$row > nrow(ss) | dcells$col > ncol(ss)) & dcells$display_row <= nrow(ss) &
        dcells$display_col <= ncol(ss)
  if (any(cut)) warning('Some cells will be cut by subset')
  class(ss) <- class(x)
  for (r in which(cut)) {
    drow <- dcells$display_row[r]
    dcol <- dcells$display_col[r]
    colspan(ss)[drow, dcol] <- min(colspan(ss)[drow, dcol], ncol(ss) - dcol + 1)
    rowspan(ss)[drow, dcol] <- min(rowspan(ss)[drow, dcol], nrow(ss) - drow + 1)
  }
  ss
}



#' @param value A matrix, data frame, huxtable or similar object.
#'
#' @rdname extract-methods
#' @export
#'
#' @examples
#' ht <- huxtable(a = 1:3, b = 1:3)
#' ht2 <- huxtable(10:11, 12:13)
#' bold(ht2) <- TRUE
#' ht[2:3,] <- ht2
#' bold(ht)
#'
`[<-.huxtable` <- function(x, i, j, value) {
  if (! is_huxtable(value)) return(NextMethod())

  if (! missing(i) && is.character(i)) i <- which(rownames(ht) %in% i)
  if (! missing(j) && is.character(j)) j <- which(colnames(ht) %in% j)
  for (att in huxtable_cell_attrs) {
    attr(x, att)[i, j] <- attr(value, att)
  }
  if (nrow(value) == nrow(x)) {
    for (att in huxtable_col_attrs) {
      attr(x, att)[j] <- attr(value, att)
    }
  }
  if (ncol(value) == ncol(x)) {
    for (att in huxtable_row_attrs) {
      attr(x, att)[i] <- attr(value, att)
    }
  }

  NextMethod() # returns the object to be reassigned to x
}


#' Add Rows/Columns
#'
#' @param ... Vectors, matrices, data frames or huxtables.
#' @param deparse.level Passed to \code{\link{cbind.data.frame}}
#'
#' @return A huxtable.
#'
#' @details
#' Table-level attributes will be taken from the first argument. Row-level
#' attributes will be taken from the first argument to \code{cbind}, and
#' column-level attributes from the first argument to \code{rbind}.
#'
#' If the first argument to \code{cbind} is not a \code{huxtable}, then
#' \code{cbind.huxtable} will not be called. To avoid this, do e.g.
#' \code{cbind(hux(1:5), ht)}.
#' @examples
#' ht1 <- hux(a = 1:3, b = 1:3)
#' bold(ht1) <- TRUE
#' ht2 <- hux(d = letters[1:3])
#' vec <- LETTERS[1:3]
#' ht <- cbind(ht1, ht2, vec)
#' ht
#' bold(ht)
#'
#' wrong <- cbind(vec, ht)
#' bold(wrong) # uh-oh
#' right <- cbind(as_hux(vec), ht)
#' bold(right)
#' @export
cbind.huxtable <- function(..., deparse.level = 1) {
  Reduce(cbind2_hux, list(...))
}

#' @export
#' @rdname cbind.huxtable
rbind.huxtable <- function(..., deparse.level = 1) {
  Reduce(rbind2_hux, list(...))
}

cbind2_hux <- function(ht, x) bind2_hux(ht, x, 'cbind')
rbind2_hux <- function(ht, x) bind2_hux(ht, x, 'rbind')

bind2_hux <- function(ht, x, type) {
  if (type=='rbind') {
    if (is.vector(x) || is.factor(x)) x <- t(x)
    if (is.vector(ht) || is.factor(ht)) ht <- t(ht)
  }
  ht <- as_hux(ht)
  x <- as_hux(x)
  bind_df <- switch(type, 'cbind' = cbind.data.frame, 'rbind' = function(x,y){
    rbind.data.frame(x, setNames(y, names(x)), stringsAsFactors = FALSE)
  })
  bind_cells <- switch(type, 'cbind' = cbind, 'rbind' = rbind)

  res <- as_hux(bind_df(ht, x))
  for (att in huxtable_cell_attrs) {
    attr(res, att) <- bind_cells(attr(ht, att), attr(x, att))
  }
  join_attrs <- switch(type, 'cbind' = huxtable_col_attrs, 'rbind' = huxtable_row_attrs)
  first_attrs <- switch(type, 'cbind' = huxtable_row_attrs, 'rbind' = huxtable_col_attrs)
  for (att in join_attrs) {
    attr(res, att) <- c(attr(ht, att), attr(x, att))
  }
  for (att in first_attrs) {
    attr(res, att) <- attr(ht, att)
  }
  for (att in huxtable_table_attrs) {
    attr(res, att) <- attr(ht, att)
  }
  res
}

#' Transpose a Huxtable
#'
#' @param x A huxtable.
#'
#' @return The transposed object.
#' @export
#'
#' @details
#' Row and column spans of \code{x} will be swapped, as will column widths and row heights,
#' table width and height, and cell borders (bottom becomes right, etc.).
#' Other attributes - in particular, alignment, vertical alignment and rotation - will be
#' preserved.
#' @examples
#' ht <- huxtable(a = 1:3, b = 1:3)
#' bottom_border(ht)[3,] <- 1
#' ht_trans <- t(ht)
#' ht_trans
t.huxtable <- function (x) {
  res <- as_hux(NextMethod())
  for (att in setdiff(huxtable_cell_attrs, c('colspan', 'rowspan', 'height', 'width',
        'bottom_border', 'left_border', 'top_border', 'right_border'))) {
    attr(res, att) <- t(attr(x, att))
  }
  attr(res, 'colspan') <- t(attr(x, 'rowspan'))
  attr(res, 'rowspan') <- t(attr(x, 'colspan'))
  attr(res, 'width')   <- attr(x, 'height')
  attr(res, 'height')  <- attr(x, 'width')
  attr(res, 'bottom_border') <- t(attr(x, 'right_border'))
  attr(res, 'right_border') <- t(attr(x, 'bottom_border'))
  attr(res, 'left_border') <- t(attr(x, 'top_border'))
  attr(res, 'top_border') <- t(attr(x, 'left_border'))
  row_height(res)      <- col_width(x)
  col_width(res)       <- row_height(x)
  for (att in huxtable_table_attrs) {
    attr(res, att) <- attr(x, att)
  }
  res
}

#' @export
knit_print.huxtable <- function (x, options, ...) {
  of <- rmarkdown::default_output_format(knitr::current_input())
  of <- of$name
  # not sure if 'print' is the right default here...
  call_name <- switch(of, pdf_document = 'to_latex', html_document = 'to_html', 'print')
  res <- do.call(call_name, list(ht=x))
  if (of == 'pdf_document') {
    latex_deps <- list(
            rmarkdown::latex_dependency('array'),
            rmarkdown::latex_dependency('graphicx'),
            rmarkdown::latex_dependency('siunitx'),
            rmarkdown::latex_dependency('xcolor', options = 'table'),
            rmarkdown::latex_dependency('multirow'),
            rmarkdown::latex_dependency('hhline'),
            rmarkdown::latex_dependency('calc')
          )
    tenv <- tabular_environment(x)
    if (tenv %in% c('tabularx', 'tabulary', 'longtable')) latex_deps <- c(latex_deps, list(rmarkdown::latex_dependency(tenv)))
    return(knitr::asis_output(res, meta = latex_deps))
  } else {
    return(knitr::asis_output(res))
  }
}

#' @export
to_md <- function(ht, ...) UseMethod('to_md')

#' Create Markdown Representing a Huxtable
#'
#' @param ht
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
to_md.huxtable <- function(ht, ...) {

}


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
to_screen  <- function (ht, ...) UseMethod('to_screen')


#' @export
#' @rdname to_screen
to_screen.huxtable <- function(ht, borders = c('both', 'horizontal', 'vertical', 'neither')) {
  borders <- match.arg(borders)

  dc <- display_cells(ht)
  drow_mat <- as.matrix(dc[,c('display_row', 'display_col')])
  dc$colspan <- colspan(ht)[drow_mat]
  dc$rowspan <- rowspan(ht)[drow_mat]
  dc$contents <- apply(drow_mat, 1, function(rc) clean_contents(ht, rc[1], rc[2]))
  dc <- dc[order(dc$colspan),]
  border_chars   <- 3

  dc$widths <- nchar(dc$contents, type = 'width')
  # each extra row = 2 screen rows including border:
  dc$widths <- ceiling(dc$widths/(2*dc$rowspan-1))

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
  charmat_borders <- matrix(0, nrow(charmat), ncol(charmat))
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

clean_contents <- function(ht, row, col, type = c('latex', 'html', 'screen'), ...) {
  mytype <- match.arg(type)
  # stopifnot(length(row) == 1 & length(col) == 1)
  contents <- ht[[row, col]] # just the data and just one element.
  # But we might want to allow more than one element; if so just use `[.data.frame`
  if (! is.na(cnum <- suppressWarnings(as.numeric(contents)))) {
    nf <- number_format(ht)[[row, col]] # a list element
    if (is.function(nf)) contents <- nf(cnum)
    if (is.character(nf)) contents <- sprintf(nf, cnum)
    if (is.numeric(nf)) contents <- formatC(round(cnum, nf), format = 'f', digits = nf)
  }

  if (is.na(contents)) contents <- na_string(ht)[row, col]
  if (escape_contents(ht)[row, col] && type != 'screen') {
    # xtable::sanitize.numbers would do very little and is buggy
    contents <-  xtable::sanitize(contents, type = mytype)
  }

  contents
}

# return matrix of cells displayed in a real 'cell position'
display_cells <- function(ht) {
  spans <- data.frame(row = rep(1:nrow(ht), ncol(ht)), col = rep(1:ncol(ht), each = nrow(ht)),
        rowspan = as.vector(rowspan(ht)), colspan = as.vector(colspan(ht)))
  spans$display_row <- spans$row
  spans$display_col <- spans$col
  spans$shadowed <- FALSE
  for (i in 1:nrow(spans)) {
    if (spans$rowspan[i] == 1 & spans$colspan[i] == 1) next
    dr <- spans$row[i]
    dc <- spans$col[i]
    spanned <- spans$row %in% dr:(dr + spans$rowspan[i] - 1) & spans$col %in% dc:(dc + spans$colspan[i] - 1)
    spans[spanned, c('display_row', 'display_col')] <- matrix(c(dr, dc), sum(spanned), 2, byrow = TRUE)
    shadowed <- spanned & (1:nrow(spans)) != i
    spans$shadowed[shadowed] <- TRUE
  }
  spans[, c('row', 'col', 'display_row', 'display_col', 'shadowed')]
}




