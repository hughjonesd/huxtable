


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
subset_cols <- function (ht, idx) {
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
subset_rows <- function (ht, idx) {
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


arrange_spans <- function (
        new_ht,
        old_ht,
        rows = seq_len(nrow(old_ht)),
        cols = seq_len(ncol(old_ht))
      ) {
  if (ncol(new_ht) == 0 || nrow(new_ht) == 0) return(new_ht)

  merge_sets <- seq_len(ncol(old_ht) * nrow(old_ht))
  dim(merge_sets) <- dim(old_ht)

  rs <- rowspan(old_ht)
  cs <- colspan(old_ht)
  for (i in seq_len(nrow(old_ht))) for (j in seq_len(ncol(old_ht))) {
    rows_to_merge <- seq(i, i + rs[i, j] - 1)
    cols_to_merge <- seq(j, j + cs[i, j] - 1)
    merge_sets[rows_to_merge, cols_to_merge] <- merge_sets[i, j]
  }
  # since merge_sets were always contiguous rectangles, new merge sets
  # will also be rectangles - not necessarily contiguous though.
  new_merge_sets <- merge_sets[rows, cols, drop = FALSE]

  # all 1s:
  nrs <- rowspan(new_ht)
  ncs <- colspan(new_ht)

  # we add 0s to ensure end_i/end_j is right when a span goes to the end of the row
  new_merge_sets <- rbind(new_merge_sets, 0)
  new_merge_sets <- cbind(new_merge_sets, 0)
  row_seq <- seq_len(nrow(new_ht) + 1)
  col_seq <- seq_len(ncol(new_ht) + 1)
  done <- matrix(FALSE, nrow(new_ht), ncol(new_ht))
  for (i in seq_len(nrow(new_ht))) for (j in seq_len(ncol(new_ht))) {
    ms <- new_merge_sets[i, j]
    if (done[i, j]) next  # changed by prev cell
    # go down from i/j until you find a different merge set:
    end_i <- min(which(row_seq >= i & new_merge_sets[, j] != ms)) - 1
    end_j <- min(which(col_seq >= j & new_merge_sets[i, ] != ms)) - 1
    nrs[i, j] <- end_i - i + 1
    ncs[i, j] <- end_j - j + 1
    done[seq(i, end_i), seq(j, end_j)] <- TRUE
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
