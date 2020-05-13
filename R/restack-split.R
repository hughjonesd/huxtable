
#' @import assertthat
NULL

#' @section Note:
#' Splitting and restacking may interact surprisingly with
#' [borders][left_border()] and [merged cells][merge_cells()].
#' @name split-gotchas
NULL


#' Restack huxtables across/down the page.
#'
#' * `restack_across()` splits a huxtable horizontally, then joins the parts
#'   up side by side.
#' * `restack_down()` splits a huxtable vertically, then joins the parts up
#'   top to bottom.
#'
#' @param ht A huxtable
#' @param rows,cols How many rows/columns the new result should have.
#' @inherit split-across-down params
#' @inheritSection split-gotchas Note
#'
#' @details
#' If the huxtable could not be evenly split and restacked, a warning will be
#' given and the remainder will be discarded.
#'
#' @return A new huxtable.
#'
#' @seealso [split-across-down]
#'
#' @examples
#'
#' ht <- as_hux(matrix(LETTERS[1:4], 2, 2))
#' ht <- set_all_borders(ht)
#'
#' ht
#' restack_down(ht, 1)
#' restack_across(ht, 1)
#'
#' # headers:
#' restack_across(jams, 2)
#' restack_across(jams, 2,
#'       with_headers = FALSE)
#'
#' @name restack-across-down
NULL


#' @export
#' @rdname restack-across-down
restack_across <- function (
        ht,
        rows,
        with_headers = TRUE
      ) {
  assert_that(is_huxtable(ht), is.count(rows))

  ht_list <- split_across(ht, after = rows, with_headers = with_headers)
  new_ht <- ht_list[[1]]
  while (length(ht_list) > 1 && nrow(ht_list[[2]]) >= rows) {
    ht_list <- split_across(ht_list[[2]], after = rows, with_headers = with_headers)
    new_ht <- cbind(new_ht, ht_list[[1]])
  }

  check_remainder(ht_list, rows)

  new_ht
}


#' @export
#' @rdname restack-across-down
restack_down <- function (
        ht,
        cols,
        with_headers = TRUE
      ) {
  assert_that(is_huxtable(ht), is.count(cols))

  ht_list <- split_down(ht, after = cols, with_headers = with_headers)
  new_ht <- ht_list[[1]]
  while (length(ht_list) > 1 && ncol(ht_list[[2]]) >= cols) {
    ht_list <- split_down(ht_list[[2]], after = cols, with_headers = with_headers)
    new_ht <- rbind(new_ht, ht_list[[1]])
  }

  check_remainder(ht_list, cols)

  new_ht
}


check_remainder <- function (ht_list, divisor){
  if (length(ht_list) > 1) warning(divisor, " didn't split huxtable evenly")
}


#' Split a huxtable into multiple huxtables.
#'
#' These functions split a huxtable horizontally or vertically, and
#' return the new sub-tables in a list.
#'
#' @param ht A huxtable.
#' @param after Rows/columns after which to split. See [rowspecs] for details.
#'   Note that [tidyselect][tidyselect::select_helpers] semantics are allowed
#'   in `split_down()` but not `split_across()`.
#' @param height,width Maximum height/width for the result.
#' @param with_headers Logical. Should header rows/columns be added to
#'   all of the new tables?
#'
#' @return A list of huxtables.
#'
#' @details
#' Only one of `after` and `width` or `height` must be given. If `width` or
#' `height` is given, the huxtable will be split by [col_width()] or
#' [row_height()], which must be numeric with no `NA` values.
#'
#' If `with_headers` is `TRUE`, all previous headers will be added to each
#' new table.
#'
#' @inheritSection split-gotchas Note
#'
#' @seealso [restack-across-down]
#'
#' @examples
#' ht <- as_hux(matrix(LETTERS[1:16], 4, 4))
#' ht <- set_all_borders(ht)
#' split_across(ht, after = 2)
#' split_down(ht, after = c(1, 3))
#'
#' col_width(ht) <- c(0.15, 0.1, 0.25, 0.3)
#' split_down(ht, size = 0.3)
#'
#' # headers are repeated:
#' split_across(jams, 3)
#'
#' @name split-across-down
NULL


#' @export
#' @rdname split-across-down
split_across <- function (
        ht,
        after,
        height,
        with_headers = TRUE
      ) {
  after <- get_rc_spec(ht, after, 1)
  if (is.logical(after)) after <- which(after)
  check_split_args(ht, after, height, max_after = nrow(ht))

  if (! missing(height)) after <- calc_after_by_size(height, row_height(ht))
  row_list <- get_pos_list(after, nrow(ht))

  ht_list <- lapply(row_list, function (rows) ht[rows,])
  if (with_headers && any(headers <- header_rows(ht))) {
    # for each first row/col, copy ALL previous headers.
    for (i in seq_along(after)) {
      if (i == length(ht_list)) next
      prev_headers <- headers[seq(1, after[i])]
      if (any(prev_headers)) {
        prev_headers <- which(prev_headers) # necessary!
        ht_list[[i + 1]] <- rbind(ht[prev_headers, ], ht_list[[i + 1]])
      }
    }
  }

  return(ht_list)
}


#' @export
#' @rdname split-across-down
split_down <- function (
        ht,
        after,
        width,
        with_headers = TRUE
      ) {
  after <- get_rc_spec(ht, after, 2)
  if (is.character(after)) after <- colnames(ht) %in% after
  if (is.logical(after)) after <- which(after)
  check_split_args(ht, after, width, max_after = ncol(ht))

  if (! missing(width)) after <- calc_after_by_size(width, col_width(ht))
  col_list <- get_pos_list(after, ncol(ht))

  ht_list <- lapply(col_list, function (cols) ht[, cols])

  if (with_headers && any(headers <- header_cols(ht))) {
    # for each first row/col, copy ALL previous headers.
    for (i in seq_along(after)) {
      if (i == length(ht_list)) next
      prev_headers <- headers[seq(1, after[i])]
      if (any(prev_headers)) {
        prev_headers <- which(prev_headers) # necessary!
        ht_list[[i + 1]] <- cbind(ht[, prev_headers], ht_list[[i + 1]])
      }
    }
  }

  return(ht_list)
}


check_split_args <- function (ht, after, size, max_after) {
  assert_that(is_huxtable(ht))
  if (missing(after) + missing(size) != 1) {
    stop('Exactly one of "after" and "width"/"height" must be given')
  }
  if (! missing(after)) assert_that(
        is.numeric(after), length(after) > 0, all(after <= max_after),
        all(after >= 1), anyDuplicated(after) == 0)
  if (! missing(size) && ! (is.number(size) && size > 0)) {
    stop('"width"/"height" must be numeric with no NAs')
  }
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
