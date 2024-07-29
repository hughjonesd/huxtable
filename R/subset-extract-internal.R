


#' Delete columns by numeric indices
#'
#' @param ht A huxtable
#' @param idx Column indices to delete
#'
#' @return The modified huxtable
#' @noRd
delete_cols <- function (ht, idx) {
  if (any(is.na(idx))) stop("Tried to delete a non-existent column")
  subset_idx <- seq_len(ncol(ht))[-idx]

  subset_cols(ht, subset_idx)
}


#' Subset columns from a huxtable
#'
#' @param ht A huxtable
#' @param idx Integer vector of column indices
#'
#' @return The huxtable with columns removed/reordered
#' @noRd
subset_cols <- function(ht, idx) {
  assert_that(is_huxtable(ht), is.numeric(idx), all(idx >= 1L),
        all(idx <= ncol(ht)))

  res <- as.data.frame(ht)[, idx, drop = FALSE]
  res <- new_huxtable(res)
  res <- arrange_spans(res, ht, cols = idx)

  for (a in huxtable_table_attrs) {
    attr(res, a) <- attr(ht, a)
  }
  for (a in huxtable_row_attrs) {
    attr(res, a) <- attr(ht, a)
  }
  for (a in huxtable_col_attrs) {
    attr(res, a) <- attr(ht, a)[idx]
  }
  for (a in setdiff(huxtable_cell_attrs, c("rowspan", "colspan"))) {
    attr(res, a) <- attr(ht, a)[, idx, drop = FALSE]
  }

  # # deal with colspan
  # right_cells <- col(ht) + colspan(ht) - 1
  # left_cells  <- col(ht)
  # for (one_idx in idx) {
  #   # matrix indexing:
  #   cut_cells <- which(right_cells >= idx & left_cells < idx, arr.ind = TRUE)
  #   colspan(res)[cut_cells] <- colspan(res)[cut_cells] - 1
  # }

  res <- prune_borders(res, ht, cols = idx)
  res <- renormalize_col_width(res)
  res <- set_attr_dimnames(res)

  res
}



#' Subset rows from a huxtable
#'
#' @param ht A huxtable
#' @param idx Integer vector of rows indices
#'
#' @return The huxtable with rows removed/reordered
#' @noRd
subset_rows <- function(ht, idx) {
  assert_that(is_huxtable(ht), is.numeric(idx), all(idx >= 1L),
        all(idx <= nrow(ht)))

  res <- as.data.frame(ht)[idx, , drop = FALSE]
  res <- new_huxtable(res)
  res <- arrange_spans(res, ht, rows = idx)

  for (a in huxtable_table_attrs) {
    attr(res, a) <- attr(ht, a)
  }
  for (a in huxtable_row_attrs) {
    attr(res, a) <- attr(ht, a)[idx]
  }
  for (a in huxtable_col_attrs) {
    attr(res, a) <- attr(ht, a)
  }
  for (a in setdiff(huxtable_cell_attrs, c("rowspan", "colspan"))) {
    attr(res, a) <- attr(ht, a)[idx, , drop = FALSE]
  }

  # # deal with rowspan
  # bottom_cells <- row(ht) + rowspan(ht) - 1
  # top_cells  <- row(ht)
  # for (one_idx in idx) {
  #   # matrix indexing:
  #   cut_cells <- which(bottom_cells >= idx & top_cells < idx, arr.ind = TRUE)
  #   rowspan(res)[cut_cells] <- rowspan(res)[cut_cells] - 1
  # }

  res <- prune_borders(res, ht, rows = idx)
  res <- renormalize_row_height(res)
  res <- set_attr_dimnames(res)

  res
}


replace_properties <- function (ht, i, j, value) {
  assert_that(
          is_hux(ht), is_hux(value),
          is.numeric(i), all(i >= 1L), all(i <= nrow(ht)),
          is.numeric(j), all(j >= 1L), all(j <= ncol(ht)),
          length(i) == nrow(value), length(j) == ncol(value)
        )
  for (a in huxtable_cell_attrs) {
    attr(ht, a)[i, j] <- attr(value, a)
  }
  if (identical(i, seq_len(nrow(ht)))) {
    old_cw <- col_width(ht)[j]
    for (a in huxtable_col_attrs) {
      attr(ht, a)[j] <- attr(value, a)
    }
    # if the new col_widths are numeric, they are treated as % and
    # the old col_widths are redistributed accordingly
    new_cw <- col_width(ht)[j]
    if (is.numeric(old_cw) && is.numeric(new_cw)) {
      col_width(ht)[j] <- new_cw/sum(new_cw) * sum(old_cw)
    }
  }
  if (identical(j, seq_len(ncol(ht)))) {
    old_rh <- row_height(ht)[j]
    for (a in huxtable_row_attrs) {
      attr(ht, a)[i] <- attr(value, a)
    }
    new_rh <- row_height(ht)[j]
    if (is.numeric(old_rh) && is.numeric(new_rh)) {
      row_height(ht)[j] <- new_rh/sum(new_rh) * sum(old_rh)
    }
  }

  # example to help you understand the below:
  # top_border(ht)[1, 2] <- top_border(value)
  # becomes
  # ht <- `top_border<-`(ht, `[<-`(top_border(ht), 1, 2, top_border(value)))
  replace_props <- function (getter, setter) {
    ht <<- setter(ht, `[<-`(getter(ht), i, j, value = getter(value)))
  }
  mapply(
    FUN = replace_props,
    huxtable_border_df$getter,
    huxtable_border_df$setter
  )

  ht <- set_attr_dimnames(ht)
  ht
}


#' Copy row and colspans over appropriately from the subsetted matrix
#'
#' @param new_ht New huxtable, row/colspans not yet set
#' @param old_ht Subsetted huxtable
#' @param rows,cols Which rows/cols are being subsetted
#' @param cols
#'
#' @return The new huxtable with row and colspans set correctly.
#' @noRd
arrange_spans <- function(new_ht,
                          old_ht,
                          rows = seq_len(nrow(old_ht)),
                          cols = seq_len(ncol(old_ht))) {
  if (ncol(new_ht) == 0 || nrow(new_ht) == 0) {
    return(new_ht)
  }

  # == create merge_sets ==
  # a matrix representing spans in old_ht
  # merged cells have the same value:
  # e.g. 1 1
  #      2 3
  # would represent a cell with colspan 2 in the first row
  dc <- display_cells(old_ht)
  merge_sets <- array(
    data = c(dc$display_row, dc$display_col),
    dim = c(nrow(old_ht), ncol(old_ht), 2)
  )

  # == create within-span indices ==
  # the first row within a span has row_number 1, the second is 2 etc.
  # same for cols and col_number
  row_number <- dc$row - dc$display_row + 1L
  col_number <- dc$col - dc$display_col + 1L
  dim(row_number) <- dim(col_number) <- dim(old_ht)

  # == calculate the merge_sets for new_ht ==
  # since merge_sets were always contiguous rectangles, new merge sets
  # will also be rectangles - not necessarily contiguous though.
  # TODO: bug is that if you repeat rows, they will have the same
  # merge_set and colspan will erroneously be set on them
  merge_sets <- merge_sets[rows, cols, , drop = FALSE]

  # == calculate row/col_number for new_ht ==
  row_number <- row_number[rows, cols, drop = FALSE]
  col_number <- col_number[rows, cols, drop = FALSE]

  # == add 0s to ensure end_row/col is ok when a span goes to the end of the row
  new_merge_sets_dims <- c(dim(merge_sets)[1:2] + 1, dim(merge_sets)[3])
  new_merge_sets <- array(0, dim = new_merge_sets_dims)
  new_merge_sets[-new_merge_sets_dims[1], -new_merge_sets_dims[2], ] <- merge_sets
  merge_sets <- new_merge_sets
  row_number <- rbind(row_number, 0)
  row_number <- cbind(row_number, 0)
  col_number <- rbind(col_number, 0)
  col_number <- cbind(col_number, 0)

  # == create new row/colspan from merge_sets and row/col_number ==
  # all 1s:
  nrs <- rowspan(new_ht)
  ncs <- colspan(new_ht)

  row_seq <- seq_len(nrow(new_ht) + 1)
  col_seq <- seq_len(ncol(new_ht) + 1)
  done <- matrix(FALSE, nrow(new_ht), ncol(new_ht))
  for (i in seq_len(nrow(new_ht))) for (j in seq_len(ncol(new_ht))) {
    ms <- merge_sets[i, j, ]
    if (done[i, j]) next # changed by prev cell
    # go down from i/j until you find a different merge set,
    # or you have seen ALL the row_numbers/col_numbers
    end_row <- min(which(
      row_seq >= i &
        apply(merge_sets[, j, ], MARGIN = 1, FUN = \(coords) any(coords != ms))
    )) - 1
    end_col <- min(which(
      col_seq >= j &
        apply(merge_sets[i, , ], MARGIN = 1, FUN = \(coords) any(coords != ms))
    )) - 1

    rn <- row_number[seq(i, end_row), j]
    cn <- col_number[i, seq(j, end_col)]
    # find the first row/col where ALL row/col numbers within this
    # merge_set have been reached
    all_rn <- seq(min(rn), max(rn))
    all_cn <- seq(min(cn), max(cn))
    # max(match(..)) returns either the first element of rn where
    # all numbers in all_rn have been matched, or NA
    all_matched_row <- i - 1 + max(match(all_rn, rn))
    all_matched_col <- j - 1 + max(match(all_cn, cn))

    end_row <- min(end_row, all_matched_row, na.rm = TRUE)
    end_col <- min(end_col, all_matched_col, na.rm = TRUE)

    nrs[i, j] <- end_row - i + 1
    ncs[i, j] <- end_col - j + 1
    done[seq(i, end_row), seq(j, end_col)] <- TRUE
  }

  rowspan(new_ht) <- nrs
  colspan(new_ht) <- ncs

  new_ht
}


#' Subset border properties
#'
#' @param new_ht The new huxtable
#' @param old_ht The old huxtable
#' @param rows Numeric vector of rows
#' @param cols Numeric vector of columns
#'
#' @return `new_ht` with borders set appropriately
#' @noRd
prune_borders <- function (
        new_ht,
        old_ht,
        rows = seq_len(nrow(old_ht)),
        cols = seq_len(ncol(old_ht))
      ) {
  # set bottom/right borders after top/left borders, so that they take priority.

  prune_props <- function (getter, setter) {
    new_ht <<- setter(new_ht, getter(old_ht)[rows, cols])
  }
  mapply(
    FUN = prune_props,
    huxtable_border_df$getter,
    huxtable_border_df$setter
  )

  new_ht
}


#' Take a subset index and return a numeric vector index
#'
#' @param idx Anything that can be passed into [, [[]] etc.
#' @param max_dim The `nrow` or `ncol` of the huxtable.
#' @param dim_names Rownames or colnames of the huxtable.
#'
#' @return A numeric vector. Numbers or characters not found in the object
#'   are returned as NA.
#' @noRd
normalize_index <- function(idx, max_dim, dim_names) {
  # without this, an error is thrown:
  if (missing(idx)) return(seq_len(max_dim))
  UseMethod("normalize_index")
}


#' @export
normalize_index.matrix <- function(idx, max_dim, dim_names) {
  stop("You can't subset a huxtable with a matrix")
}


#' @export
normalize_index.logical <- function(idx, max_dim, dim_names) {
  if (length(idx) > max_dim) {
    stop("More rows/columns specified than found in huxtable")
  }
  if (max_dim %% length(idx) > 0) {
    warning("Length of subscript does not divide huxtable dimension exactly")
  }
  idx <- rep(idx, length.out = max_dim)

  which(idx)
}

#' @export
normalize_index.character <- function(idx, max_dim, dim_names) {
  if (any(is.na(idx))) stop("NA in subscript")

  match(idx, dim_names)
}


#' @export
normalize_index.numeric <- function(idx, max_dim, dim_names) {
  if (any(is.na(idx))) stop("NA in subscript")
  if (any(idx < 0)) {
    if (! all(idx <= 0)) stop("Can't mix positive and negative subscripts")
    if (any(-idx > max_dim)) stop("Negative subscript out of bounds")
    idx <- seq_len(max_dim)[idx]
  }
  idx_oob <- idx > max_dim
  if (any(idx_oob)) {
    filled_in_seq <- seq(max_dim + 1, max(idx[idx_oob]))
    if (! identical(as.integer(sort(idx[idx_oob])), filled_in_seq)) {
      stop("Missing new rows/columns in subscript: huxtable dimension is ", max_dim,
            "but subscripts were ", paste(idx, collapse = " "))
    }
    idx[idx_oob] <- NA_integer_
  }

  idx
}


#' @export
normalize_index.default <- function(idx, max_dim, dim_names) {
  stop("Unrecognized subscript of type ", typeof(idx))
}
