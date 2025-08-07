#' Insert rows or columns into a huxtable
#'
#' @param x A huxtable to modify
#' @param y Rows or columns to insert, as a huxtable
#' @param after Row/column number or name after which to insert
#' @param dimno Dimension to bind: 1 for rows, 2 for columns
#' @param copy_cell_props Copy cell properties from `x` to `y`?
#'
#' @return A huxtable with rows or columns inserted
#' @noRd
add_row_cols <- function(x, y, after, dimno, copy_cell_props) {
  dims <- dim(x)
  end_idx <- dims[dimno]
  if (is.character(after)) {
    after_n <- match(after, dimnames(x)[[dimno]])
    if (is.na(after_n)) {
      stop(
        "Could not find row/column name \"",
        after, "\" in huxtable"
      )
    }
    after <- after_n
  }
  assert_that(is.number(after), after >= 0, after <= end_idx)

  first_idxes <- seq_len(after)
  # adding numeric(0) to after gives numeric(0):da
  second_idxes <- after + seq_len(max(end_idx - after, 0))

  has_dims <- function(x) {
    if (is.vector(x)) {
      length(x) > 0
    } else {
      (nrow(x) > 0 && ncol(x) > 0)
    }
  }
  if (dimno == 1) {
    objs <- list(x[first_idxes, ], y, x[second_idxes, ])
    objs <- Filter(has_dims, objs)
    do.call(rbind.huxtable, c(objs,
      copy_cell_props = copy_cell_props
    ))
  } else {
    objs <- list(x[, first_idxes], y, x[, second_idxes])
    objs <- Filter(has_dims, objs)
    do.call(cbind.huxtable, c(objs,
      copy_cell_props = copy_cell_props
    ))
  }
}


# * `bind2_rows(obj1, obj2)` and `bind2_cols(obj1, obj2)`
# - if either obj is not a huxtable, then a new one is created.
# Properties are copied down/across by default.
# - new_huxtable() is called with the data from `obj1` and `obj2`
# - the new object has properties set by merging the properties of the
# two old ones using `merge_properties_across/down(new_obj, obj1, obj2)`
# - should we autoformat new columns (assuming we haven't merged properties
#     across?) Should this be an option? (Maybe not...)

#' rbind two huxtablish objects together
#'
#' @param obj1 huxtable or object which can be represented as one
#' @param obj2 huxtable or object which can be represented as one
#' @param copy_cell_props Flag. Copy cell & row properties from last row of
#'   `obj1` to `obj2`, if `obj2` is not a huxtable already?
#'
#' @return a new huxtable
#' @noRd
bind_rows_2 <- function(obj1, obj2, copy_cell_props) {
  assert_that(
    is.flag(copy_cell_props),
    is.null(ncol(obj1)) || is.null(ncol(obj2)) || ncol(obj1) == ncol(obj2)
  )

  if (!is_huxtable(obj1)) obj1 <- new_huxtable_row(obj1)
  if (!is_huxtable(obj2)) {
    obj2 <- new_huxtable_row(obj2)
    if (copy_cell_props) obj2 <- copy_properties_down(obj1, obj2)
  }

  obj2_dfr <- as.data.frame(obj2)
  colnames(obj2_dfr) <- colnames(obj1)
  new_ht <- rbind(as.data.frame(obj1), obj2_dfr, stringsAsFactors = FALSE)
  new_ht <- new_huxtable(new_ht)
  new_ht <- merge_properties_down(new_ht, obj1, obj2)

  new_ht
}


#' Make an object a huxtable, turning vectors into a single row
#'
#' @param obj A vector, matrix etc.
#'
#' @return A huxtable
#' @noRd
new_huxtable_row <- function(obj) {
  if (is_vectorish(obj)) {
    obj <- matrix(obj, nrow = 1, ncol = length(obj))
  }
  new_huxtable(obj)
}


dot_or_dim_names <- function(..., dimension) {
  objs <- list(...)
  nms <- names(objs)
  if (is.null(nms)) nms <- rep("", length(objs))
  dot_or_dim <- function(obj, name) {
    size <- dim(obj)
    dn <- dimnames(obj)
    if (is.null(size) && is.null(dn)) {
      return(name)
    }
    if (is.null(dn)) {
      return(rep(name, size[[dimension]]))
    }
    return(dn[[dimension]])
  }
  res <- mapply(FUN = dot_or_dim, objs, nms)

  make.unique(unlist(res))
}



#' cbind two huxtablish objects together
#'
#' @param obj1 huxtable or object which can be represented as one
#' @param obj2 huxtable or object which can be represented as one
#' @param copy_cell_props Flag. Copy cell properties from last column of
#'   `obj1` to `obj2`, if `obj2` is not a huxtable already?
#'
#' @return a new huxtable
#' @noRd
bind_cols_2 <- function(obj1, obj2, copy_cell_props) {
  assert_that(
    is.flag(copy_cell_props),
    is.null(nrow(obj1)) || is.null(nrow(obj2)) || nrow(obj1) == nrow(obj2)
  )
  if (!is_huxtable(obj1)) obj1 <- new_huxtable(obj1)
  if (!is_huxtable(obj2)) {
    obj2 <- new_huxtable(obj2)
    if (copy_cell_props) obj2 <- copy_properties_across(obj1, obj2)
  }

  new_ht <- cbind(as.data.frame(obj1), as.data.frame(obj2),
    stringsAsFactors = FALSE
  )
  new_ht <- new_huxtable(new_ht)
  new_ht <- merge_properties_across(new_ht, obj1, obj2)

  new_ht
}


#' Copy cell and row properties from `ht1`'s last row into `ht2`
#'
#' @param ht1 Top huxtable
#' @param ht2 Bottom huxtable
#'
#' @return `ht2`, modified.
#' @noRd
copy_properties_down <- function(ht1, ht2) {
  assert_that(is_huxtable(ht1), is_huxtable(ht2))

  if (nrow(ht1) == 0L) {
    return(ht2)
  }

  for (a in c(copiable_cell_attrs())) {
    for (r in seq_len(nrow(ht2))) {
      attr(ht2, a)[r, ] <- attr(ht1, a)[nrow(ht1), ]
    }
  }
  for (a in huxtable_row_attrs) {
    attr(ht2, a)[] <- attr(ht1, a)[nrow(ht1)]
  }

  copy_down <- function(getter, setter) {
    ht2 <<- setter(ht2, getter(ht1)[nrow(ht1), ])
  }
  mapply(
    FUN = copy_down, huxtable_border_df$getter,
    huxtable_border_df$setter
  )

  ht2
}


#' Copy cell and column properties from `ht1`'s last column into `ht2`
#'
#' @param ht1 Left huxtable
#' @param ht2 Right huxtable
#'
#' @return `ht2`, modified.
#' @noRd
copy_properties_across <- function(ht1, ht2) {
  assert_that(is_huxtable(ht1), is_huxtable(ht2))

  if (ncol(ht1) == 0L) {
    return(ht2)
  }

  for (a in c(copiable_cell_attrs())) {
    for (col in seq_len(ncol(ht2))) {
      attr(ht2, a)[, col] <- attr(ht1, a)[, ncol(ht1)]
    }
  }
  for (a in huxtable_col_attrs) {
    attr(ht2, a)[] <- attr(ht1, a)[ncol(ht1)]
  }


  copy_across <- function(getter, setter) {
    ht2 <<- setter(ht2, getter(ht1)[, ncol(ht1)])
  }
  mapply(
    FUN = copy_across, huxtable_border_df$getter,
    huxtable_border_df$setter
  )

  ht2
}


copiable_cell_attrs <- function() {
  setdiff(huxtable_cell_attrs, c("colspan", "rowspan"))
}



#' Merge properties from `ht1` and `ht2` into `new_ht`
#'
#' @param new_ht New huxtable with `nrow(new_ht) == nrow(ht1) + nrow(ht2)``,
#'   `ncol(new_ht) == ncol(ht2) == ncol(ht2)``
#' @param ht1 top huxtable
#' @param ht2 bottom huxtable
#'
#' @return `new_ht`, modified
#' @noRd
merge_properties_down <- function(new_ht, ht1, ht2) {
  assert_that(is_hux(new_ht), is_hux(ht1), is_hux(ht2))
  for (a in huxtable_table_attrs) {
    attr(new_ht, a) <- attr(ht1, a)
  }
  for (a in huxtable_cell_attrs) {
    attr(new_ht, a) <- rbind(attr(ht1, a), attr(ht2, a))
  }
  for (a in huxtable_row_attrs) {
    attr(new_ht, a) <- c(attr(ht1, a), attr(ht2, a))
  }
  for (a in huxtable_col_attrs) {
    attr(new_ht, a) <- attr(ht1, a)
  }

  # borders are simply joined on; right and bottom take priority as usual.
  copy_borders_down <- function(getter, setter) {
    new_ht <<- setter(new_ht, rbind(getter(ht1), getter(ht2)))
  }
  mapply(
    FUN = copy_borders_down,
    huxtable_border_df$getter,
    huxtable_border_df$setter
  )

  new_ht <- renormalize_row_height(new_ht)

  new_ht
}


#' Merge properties from `ht1` and `ht2` into `new_ht`
#'
#' @param new_ht New huxtable with `nrow == nrow(ht1) == nrow(ht2)``,
#'   `ncol(new_ht) == ncol(ht2) + ncol(ht2)``
#' @param ht1 left huxtable
#' @param ht2 right huxtable
#'
#' @return `new_ht`, modified
#' @noRd
merge_properties_across <- function(new_ht, ht1, ht2) {
  assert_that(is_hux(new_ht), is_hux(ht1), is_hux(ht2))
  for (a in huxtable_table_attrs) {
    attr(new_ht, a) <- attr(ht1, a)
  }
  for (a in huxtable_cell_attrs) {
    attr(new_ht, a) <- cbind(attr(ht1, a), attr(ht2, a))
  }
  for (a in huxtable_row_attrs) {
    attr(new_ht, a) <- attr(ht1, a)
  }
  for (a in huxtable_col_attrs) {
    attr(new_ht, a) <- c(attr(ht1, a), attr(ht2, a))
  }

  copy_borders_across <- function(getter, setter) {
    new_ht <<- setter(new_ht, cbind(getter(ht1), getter(ht2)))
  }
  mapply(
    FUN = copy_borders_across,
    huxtable_border_df$getter,
    huxtable_border_df$setter
  )

  new_ht <- renormalize_col_width(new_ht)

  new_ht
}


renormalize_row_height <- function(ht) {
  if (is.numeric(rh <- row_height(ht))) {
    row_height(ht) <- rh / sum(rh)
  }

  ht
}


renormalize_col_width <- function(ht) {
  if (is.numeric(cw <- col_width(ht))) {
    col_width(ht) <- cw / sum(cw)
  }

  ht
}
