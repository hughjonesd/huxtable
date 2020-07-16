
#' @import assertthat
NULL


#' Restack huxtables across/down the page
#'
#' * `restack_across()` splits a huxtable horizontally, then joins the parts
#'   up side by side.
#' * `restack_down()` splits a huxtable vertically, then joins the parts up
#'   top to bottom.
#'
#' @param ht A huxtable
#' @param rows,cols How many rows/columns the new result should have.
#' @param on_remainder String. "warn", "stop" or "fill". See below.
#' @inherit split-across-down params
#'
#' @details
#' If `headers` is `TRUE`, header rows/columns will be repeated across/down
#' the restacked huxtable as necessary.
#'
#' `on_remainder` determines what happens if the huxtable could not be evenly
#' divided for restacking:
#'
#' * `"stop"`: stop with an error.
#' * `"fill"`: fill the remainder with empty cells.
#' * `"warn"` (the default): issue a warning, then fill the remainder with empty
#'   cells.
#'
#' @return A new huxtable.
#'
#' @seealso [split-across-down]
#'
#' @examples
#'
#' ht <- as_hux(matrix(LETTERS[1:4], 2, 2))
#' ht <- set_all_borders(ht)
#' ht
#'
#' restack_down(ht, 1)
#' restack_across(ht, 1)
#'
#' # headers:
#' restack_across(jams, 2)
#' restack_across(jams, 2,
#'       headers = FALSE)
#'
#' # on_remainder:
#' restack_across(jams, 3,
#'       on_remainder = "fill")
#'
#' @name restack-across-down
NULL


#' @export
#' @rdname restack-across-down
restack_across <- function (
        ht,
        rows,
        headers = TRUE,
        on_remainder = c("warn", "stop", "fill")
      ) {
  assert_that(is_huxtable(ht), is.count(rows), is.flag(headers))
  on_remainder <- match.arg(on_remainder)
  on_remainder <- switch(on_remainder,
          "warn" = warning,
          "stop" = stop,
          identity # i.e. do nothing
        )

  ht_list <- split_across(ht, after = rows, headers = headers)
  new_ht <- ht_list[[1]]
  while (length(ht_list) > 1 && nrow(ht_list[[2]]) >= rows) {
    ht_list <- split_across(ht_list[[2]], after = rows, headers = headers)
    new_ht <- cbind(new_ht, ht_list[[1]])
  }

  if (length(ht_list) > 1) {
    on_remainder(sprintf("Table was not split equally into %s rows", rows))
    remainder <- ht_list[[2]]
    remainder <- rbind(remainder,
      matrix("", nrow(new_ht) - nrow(remainder), ncol(remainder)))
    new_ht <- cbind(new_ht, remainder)
  }

  new_ht
}


#' @export
#' @rdname restack-across-down
restack_down <- function (
        ht,
        cols,
        headers = TRUE,
        on_remainder = c("warn", "stop", "fill")
      ) {
  assert_that(is_huxtable(ht), is.count(cols), is.flag(headers))
  on_remainder <- match.arg(on_remainder)
  on_remainder <- switch(on_remainder,
          "warn" = warning,
          "stop" = stop,
          identity # i.e. do nothing
        )

  ht_list <- split_down(ht, after = cols, headers = headers)
  new_ht <- ht_list[[1]]
  while (length(ht_list) > 1 && ncol(ht_list[[2]]) >= cols) {
    ht_list <- split_down(ht_list[[2]], after = cols, headers = headers)
    new_ht <- rbind(new_ht, ht_list[[1]])
  }

  if (length(ht_list) > 1) {
    on_remainder(sprintf("Table was not split equally into %s cols", cols))
    remainder <- ht_list[[2]]
    remainder <- cbind(remainder,
          matrix("", nrow(remainder), ncol(new_ht) - ncol(remainder)))
    new_ht <- rbind(new_ht, remainder)
  }

  new_ht
}


#' Split a huxtable into multiple huxtables
#'
#' These functions split a huxtable horizontally or vertically, and
#' return the new sub-tables in a list.
#'
#' @param ht A huxtable.
#' @param after Rows/columns after which to split. See [rowspecs] for details.
#'   Note that [tidyselect][tidyselect::language] semantics are allowed
#'   in `split_down()` but not `split_across()`.
#' @param height,width Maximum height/width for the result.
#' @param headers Logical. Take account of header rows/columns?
#'
#' @return A list of huxtables.
#'
#' @details
#' Only one of `after` and `width` or `height` must be given. If `width` or
#' `height` is given, the huxtable will be split by [col_width()] or
#' [row_height()], which must be numeric with no `NA` values.
#'
#' If `headers` is `TRUE`, all previous headers will be added to each
#' new table.
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
#' split_down(ht, width = 0.3)
#'
#' # split by column name:
#' split_down(jams, "Type")
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
        headers = TRUE
      ) {
  assert_that(is_hux(ht), is.flag(headers))
  if (missing(after) + missing(height) != 1) {
    stop("Exactly one of `after` and `height` must be specified")
  }

  if (! missing(height)) {
    assert_that(is.number(height), height > 0)
    after <- calc_after_by_size(height, row_height(ht))
  } else {
    assert_that(noNA(after))
    after <- get_rc_spec(ht, after, 1)
    if (is.logical(after)) after <- which(after)
  }
  check_after(after, nrow(ht))

  row_list <- get_pos_list(after, nrow(ht))

  ht_list <- lapply(row_list, function (rows) ht[rows,])
  if (headers && any(headers <- header_rows(ht))) {
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
        headers = TRUE
      ) {
  assert_that(is_hux(ht), is.flag(headers))
  if (missing(after) + missing(width) != 1) {
    stop("Exactly one of `after` and `width` must be specified")
  }

  if (! missing(width)) {
    assert_that(is.number(width), width > 0)
    after <- calc_after_by_size(width, col_width(ht))
  } else {
    after <- get_rc_spec(ht, after, 2)
    if (is.character(after)) after <- match(after, colnames(ht))
    if (is.logical(after)) after <- which(after)
  }
  check_after(after, ncol(ht))

  col_list <- get_pos_list(after, ncol(ht))

  ht_list <- lapply(col_list, function (cols) ht[, cols])

  if (headers && any(headers <- header_cols(ht))) {
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


check_after <- function (after, last_row_col) {
  assert_that(noNA(after), length(after) > 0, all(after >= 0),
    all(after <= last_row_col))
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
