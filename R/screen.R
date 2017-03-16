
#' @export
#' @rdname to_screen
print_screen <- function(ht, ...) cat(to_screen(ht, ...))


#' Print a Huxtable on Screen
#'
#' @param ht A huxtable.
#' @param ... Passed on to \code{to_screen}.
#' @param borders Print 'horizontal' borders, 'vertical' borders, 'both' or 'neither'. May be abbreviated.
#' @param blank   Character to print for cell divisions with no border. If \code{NULL}, print a space ' ' but don't print
#'   empty horizontal borders.
#' @param colnames Whether or not to print colum names.
#' @param colnames_color Color to print column names. Note: this won't work in RStudio.
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
#' print_screen(ht, borders = 'horiz')
to_screen  <- function (ht, ...) UseMethod('to_screen')

#
# to_screen2 <- function (ht, borders = c('both', 'horizontal', 'vertical', 'neither'), blank = NULL, colnames = TRUE, colnames_color = 'blue', max_width = 80, ...) {
#   borders <- match.arg(borders)
#   collapse_horiz <- is.null(blank)
#   if (is.null(blank)) blank <- ' '
#   if (colnames) ht <- add_colnames(ht)
#   my_colour <- if (is.null(colnames_color)) identity else crayon::make_style(colnames_color)
#
#   dc               <- display_cells(ht, all = FALSE)
#   drow_mat         <- as.matrix(dc[,c('display_row', 'display_col')])
#   contents         <- clean_contents(ht, type = 'screen')
#   dc$contents      <- contents[drow_mat]
#   dc$wrap          <- wrap(ht)[drow_mat]
#   dc$top_border    <- top_border(ht)[drow_mat]
#   dc$bottom_border <- bottom_border(ht)[drow_mat]
#   dc$left_border   <- left_border(ht)[drow_mat]
#   dc$right_border  <- right_border(ht)[drow_mat]
#   dc <- dc[order(dc$colspan),]
#
#   # this represents character widths on screen for each column
#   # -3 because have to account for borders
#   # pattern is:
#   # BSCCCCCSBSCCCCSBSCCCSB
#   # where B is border S is space C is cell content
#   scr_w <- rep(floor(max_width/ncol(ht)) - 3, ncol(ht))
#   scr_w <- pmax(0, scr_w)
#   # allocate screen colwidths as follows:
#   # for each column, find cells overlapping that column with colspan n
#   # take the max of nowrap cells
#   # if the sum of widths of the cols, + 3 per internal border, is less, increase it
#   # if we hit max_width, exit the loop
#   dcnw <- dc[! dc$wrap,]
#   for (j in seq_along(nrow(dcnw))) {
#     dcr <- dcnw[j,]
#     cell_width <- nchar(dcr$contents, type = 'width')
#     col_width <- sum(scr_w[dcr$display_col:dcr$end_col]) + 3 * (dcr$colspan - 1)
#     if ((excess <- cell_width - colwidth) > 0) {
#       scr_w[dcr$display_col:dcr$end_col] <- scr_w[dcr$display_col:dcr$end_col] +
#             ceiling(excess/dcr$colspan)
#       # 2 border cols take 2 chars, internal ones take 3 chars
#       total_width <- sum(scr_w) + 4 + 3 * (ncol(dcr) - 1)
#       if ((excess2 <- total_width - max_width) > 0) {
#         scr_w[dcr$display_col:dcr$end_col] <- scr_w[dcr$display_col:dcr$end_col] -
#               ceiling(excess2/dcr$colspan)
#       }
#     }
#   }
#
#   char_widths <- interleave(scr_w, c(2, rep(3, length(scr_w) - 1), 2))
#   for (row in 0:nrow(ht)) {
#     dcr  <- dc[dc$display_row == row, ]
#     dcr2 <- dc[dc$display_row == row + 1, ]
#     border_segs <- rep(FALSE, ncol(ht))
#     corner_segs <- rep(FALSE, ncol(ht) + 1)
#     side_segs   <- rep(FALSE, ncol(ht) + 1)
#     for (j in seq_along(nrow(dcr))) {
#       if (dcr$bottom_border[j] > 0) border_segs[dcr$display_col[j]:dcr$end_col[j]] <- TRUE
#       if (dcr$left_border[j] > 0) corner_segs[dcr$display_col[j]] <- TRUE
#       if (dcr$right_border[j] > 0) corner_segs[dcr$end_col[j]] <- TRUE
#     }
#     for (j in seq_along(nrow(dcr2))){
#       if (dcr2$top_border[j] > 0) border_segs[dcr2$display_col[j]:dcr2$end_col[j]] <- TRUE
#       if (dcr2$left_border[j] > 0) {
#         corner_segs[dcr2$display_col[j]] <- TRUE
#         side_segs[dcr2$display_col[j]] <- TRUE
#       }
#       if (dcr2$right_border[j] > 0) {
#         corner_segs[dcr2$end_col[j]] <- TRUE
#         side_segs[dcr2$end_col[j]] <- TRUE
#       }
#     }
#
#
#   }
#
#
#
#   # now, calculate each screen row:
#   # border rows are easy
#   # cell rows: nowrap cells are truncated to sum of widths + 3 per int border
#   #            wrap cells are wrapped, and this creates a new screen row
#   #            horiz borders are superimposed - we know where they are
#
#
# }


#' @export
#' @rdname to_screen
to_screen.huxtable <- function (ht, borders = c('both', 'horizontal', 'vertical', 'neither'), blank = NULL, colnames = TRUE, colnames_color = 'blue', ...) {
  borders <- match.arg(borders)
  collapse_horiz <- is.null(blank)
  if (is.null(blank)) blank <- ' '
  if (colnames) {
    ht <- add_colnames(ht)
  }
  my_colour <- if (is.null(colnames_color)) identity else crayon::make_style(colnames_color)

  dc <- display_cells(ht, all = FALSE)
  drow_mat <- as.matrix(dc[,c('display_row', 'display_col')])
  contents <- clean_contents(ht, type = 'screen')
  dc$contents <- contents[drow_mat]

  dc <- dc[order(dc$colspan),]
  border_chars   <- 3

  dc$widths <- nchar(dc$contents, type = 'width')

  # widths of actual columns, not including borders
  max_widths <- rep(0, ncol(ht))
  for (r in 1:nrow(dc)) {
    width <- dc$widths[r]
    cols <- with(dc[r,], display_col:end_col)
    if (sum(max_widths[cols]) + border_chars * (dc$colspan[r] - 1) < width) {
      max_widths[cols] <- pmax(max_widths[cols], ceiling(width/dc$colspan[r]))
    }
  }

  charmat         <- matrix(' ', 2 * nrow(ht) + 1, sum(max_widths) + (ncol(ht) + 1) * border_chars)
  border_cells    <- matrix(0, nrow(charmat), ncol(charmat)) # 0 = not a border; 1 = no border; > 1 = border
  corner_cells    <- matrix(FALSE, nrow(charmat), ncol(charmat))
  for (r in 1:nrow(dc)) {
    dcr     <- dc[r,]
    drow    <- dcr$display_row
    dcol    <- dcr$display_col
    end_col <- dcol + dcr$colspan - 1
    end_row <- drow + dcr$rowspan - 1

    # content
    chars       <- strsplit(dcr$contents, '')[[1]]
    char_row    <- drow * 2
    total_width <- sum(max_widths[dcol:end_col]) + border_chars*(dcr$colspan - 1) # include internal borders
    start_char  <- sum(max_widths[seq_len(dcol - 1)]) + border_chars * dcol
    while (length(chars) > 0) {
      idx      <- 1:min(total_width, length(chars))
      space    <- max(total_width - length(chars), 0)
      char_idx <- switch(align(ht)[drow, dcol],
              left  = start_char + idx,
              right = start_char + space + idx,
              center = start_char + floor(space/2) + idx
            )
      charmat[char_row, char_idx ] <- chars[idx]
      chars <- chars[-idx]
      char_row <- char_row + 1
    }

    bdr_idx_cols <- (start_char - 1):(start_char + total_width + 2)
    bord <- get_all_borders(ht, drow, dcol)
    border_cells[drow * 2 - 1, bdr_idx_cols] <- pmax(border_cells[drow * 2 - 1, bdr_idx_cols], 1 + bord$top)
    border_cells[end_row * 2 + 1, bdr_idx_cols] <- pmax(border_cells[end_row * 2 + 1, bdr_idx_cols],
            1 + bord$bottom)
    border_cells[(drow * 2):(end_row * 2), start_char - 1] <-
            pmax(border_cells[(drow * 2):(end_row * 2), start_char - 1], 1 + bord$left)
    border_cells[(drow * 2):(end_row * 2), start_char + total_width + 2] <-
          pmax(border_cells[(drow * 2):(end_row * 2), start_char + total_width + 2],1 + bord$right)
    corner_cells[drow * 2 + c(-1, 1), c(start_char - 1, start_char + total_width + 2)] <- TRUE
  }

  charmat[border_cells > 0] <- blank
  if (borders %in% c('both', 'horizontal')) charmat[border_cells > 1 & row(charmat) %% 2]   <- '-'
  if (borders %in% c('both', 'vertical'))   charmat[border_cells > 1 & ! row(charmat) %% 2] <- '|'
  if (borders == 'vertical') charmat[border_cells > 1 & row(charmat) %% 2 & corner_cells] <- '|'
  if (collapse_horiz) {
    empty_borders <- apply(border_cells, 1, function (x) all(x[ -c(1, ncol(charmat)) ] == 1))
    charmat <- charmat[!empty_borders,]
  }
  result <- apply(charmat, 1, paste0, collapse='')

  if (colnames) result[2 - collapse_horiz] <- my_colour(result[2 - collapse_horiz])
  result <- paste0(result, collapse='\n')
  if (! is.na(cap <- caption(ht))) {
    result <- if (caption_pos(ht) == 'top') paste0(cap, '\n', result) else paste0(result, '\n', cap)
  }
  result <- paste0(result, '\n')

  result
}


#' @export
#' @rdname to_md
print_md <- function(ht, ...) cat(to_md(ht, ...))


#' Create Markdown Representing A Huxtable
#'
#' @param ht        A huxtable.
#' @param max_width Max width in on-screen characters of the result.
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
to_md.huxtable <- function(ht, max_width = 80, ...) {
  cw <- col_width(ht)
  if (! is.numeric(cw) || anyNA(cw)) cw <- rep(1, ncol(ht))
  cw <- floor(cw/sum(cw) * (max_width - ncol(ht) + 1))
  width <- sum(cw) + ncol(ht) - 1
  ncharw <- function(x) nchar(x, type = 'width')
  dcells <- display_cells(ht, all = FALSE)
  if (any(dcells$shadowed)) warning("Markdown cannot handle cells with colspan/rowspan > 1")
  dcells <- dcells[! dcells$shadowed, ]
  result <- str_rep('-', width)
  result <- paste0(result, '\n')
  contents <- clean_contents(ht, type = 'markdown')
  dcells$contents <- contents[as.matrix(dcells[, c('display_row', 'display_col')])]

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
            left = paste0(chunk, str_rep(' ', extra)),
            center = paste0(str_rep(' ', floor(extra/2)), chunk, str_rep(' ', ceiling(extra/2))),
            right = paste0(str_rep(' ', extra), chunk)
          )
        }
        result <- paste0(result, chunk)
        row_chars[i] <- substring(row_chars[i], cw[i] + 1)
        result <- paste0(result, ' ')
      }
      result <- paste0(result, '\n')
    }
    if (myrow == 1) {
      dash_row <- paste(str_rep('-', cw), collapse = " ")
      result <- paste0(result, dash_row, '\n')
    } else {
      result <- paste0(result, '\n') # extra blank line for row
    }
  }
  result <- paste0(result, str_rep('-', width), '\n')
  if (! is.na(cap <- caption(ht))) result <- paste0(result, 'Table: ', cap, '\n')
  result <- paste0(result, '\n')
  result
}

str_rep <- function(x, times) {
  mapply(function (s, t) paste0(rep(s, t), collapse=''), x, times)
}

