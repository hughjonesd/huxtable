
#' @import assertthat
NULL


# what to do about losing merged cells? (at the moment, the hidden
# cells reappear)

#' Split a huxtable into multiple huxtables.
#'
#' These functions split a huxtable horizontally or vertically, and
#' return the new sub-tables in a list.
#'
#' @param ht A huxtable.
#' @param after Rows/columns after which to split.
#' @param size Maximum height/width for the result.
#'
#' @return A list of huxtables.
#'
#' @details
#' Only one of `after` and `size` must be given. If `size` is given,
#' the huxtable will be split by [row_height()] or [col_width()],
#' which must be numeric with no `NA` values.
#'
#' @examples
#' ht <- as_hux(matrix(LETTERS[1:16], 4, 4))
#' ht <- set_all_borders(ht)
#' split_across(ht, after = 2)
#' split_down(ht, after = c(1, 3))
#'
#' col_width(ht) <- c(0.15, 0.1, 0.25, 0.3)
#' split_down(ht, size = 0.3)
#' @name split-across-down
NULL


#' @export
#' @rdname split-across-down
split_across <- function (ht, after, size) {
  check_split_args(ht, after, size, max_after = nrow(ht))

  if (! missing(size)) after <- calc_after_by_size(size, row_height(ht))
  row_list <- get_pos_list(after, nrow(ht))

  lapply(row_list, function (rows) ht[rows,])
}


#' @export
#' @rdname split-across-down
split_down <- function (ht, after, size) {
  check_split_args(ht, after, size, max_after = ncol(ht))

  if (! missing(size)) after <- calc_after_by_size(size, col_width(ht))
  col_list <- get_pos_list(after, ncol(ht))

  lapply(col_list, function (cols) ht[, cols])
}


check_split_args <- function (ht, after, size, max_after) {
  assert_that(is_huxtable(ht))
  if (missing(after) + missing(size) != 1) {
    stop("Exactly one of `after` and `size` must be given")
  }
  if (! missing(after)) assert_that(
        is.numeric(after), length(after) > 0, all(after < max_after),
        all(after >= 1), anyDuplicated(after) == 0)
  if (! missing(size)) assert_that(is.number(size), size > 0)
}


calc_after_by_size <- function (size, lengths) {
  assert_that(is.numeric(lengths), ! anyNA(lengths))
  # find the biggest total length
  total_lengths <- cumsum(lengths)
  poss <- total_lengths <= size
  if (! any(poss)) stop("Can't guarantee huxtables to all be less than ", size)
  biggest_poss <- max(which(poss))
  if (biggest_poss == length(lengths)) return(biggest_poss)
  c(biggest_poss,
    biggest_poss + calc_after_by_size(size, lengths[-seq(1L, biggest_poss)])
  )
}

get_pos_list <- function (after, endpoint) {
  after <- sort(after)
  after <- c(0, after)
  if (max(after) < endpoint) after <- c(after, endpoint)
  pos_list <- lapply(seq_len(length(after) - 1), function (x) {
    seq(after[x] + 1, after[x+1])
  })

  pos_list
}
