


delete_props <- function (res, old, idx) {
  if (is.logical(idx)) idx <- which(idx)

  for (a in huxtable_col_attrs) {
    attr(res, a) <- attr(res, a)[ -idx]
  }
  for (a in huxtable_cell_attrs) {
    attr(res, a) <- attr(res, a)[, -idx, drop = FALSE]
  }
  # missing values will work here
  res <- prune_borders(res, old, rows = , cols = -idx)

  res
}


# this makes no assumptions about the dimensions of the objects; only
# will use dimensions of tb_border object.
prune_borders <- function (new, old, rows, cols) {
  # set bottom/right borders after top/left borders, so that they take priority.

  prune_props <- function (getter, setter) {
    new <<- setter(new, getter(old)[rows, cols])
  }
  mapply(
    FUN = prune_props,
    huxtable_border_df$getter,
    huxtable_border_df$setter
  )

  new
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
#' @param copy_cell_props Flag. Copy cell properties from last row of
#'   `obj1` to `obj2`, if `obj2` is not a huxtable already?
#'
#' @return a new huxtable
#' @noRd
bind_rows_2 <- function (obj1, obj2, copy_cell_props) {
  assert_that(is.flag(copy_cell_props))

  if (! is_huxtable(obj1)) obj1 <- new_huxtable_row(obj1)
  if (! is_huxtable(obj2)) {
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
new_huxtable_row <- function (obj) {
  if (is.vector(obj) || is.factor(obj)) {
    obj <- matrix(obj, nrow = 1, ncol = length(obj))
  }
  new_huxtable(obj)
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
bind_cols_2 <- function (obj1, obj2, copy_cell_props) {
  assert_that(is.flag(copy_cell_props))
  if (! is_huxtable(obj1)) obj1 <- new_huxtable(obj1)
  if (! is_huxtable(obj2)) {
    obj2 <- new_huxtable(obj2)
    if (copy_cell_props) obj2 <- copy_properties_across(obj1, obj2)
  }

  new_ht <- cbind(as.data.frame(obj1), as.data.frame(obj2),
        stringsAsFactors = FALSE)
  new_ht <- new_huxtable(new_ht)
  new_ht <- merge_properties_across(new_ht, obj1, obj2)

  new_ht
}


#' Copy cell properties from `ht1`'s last row into `ht2`
#'
#' @param ht1 Top huxtable
#' @param ht2 Bottom huxtable
#'
#' @return `ht2`, modified.
#' @noRd
copy_properties_down <- function (ht1, ht2) {
  assert_that(is_huxtable(ht1), is_huxtable(ht2))

  for (a in copiable_cell_attrs()) {
    for (r in seq_len(nrow(ht2))) {
      attr(ht2, a)[r, ] <- attr(ht1, a)[nrow(ht1), ]
    }
  }

  copy_down <- function (getter, setter) {
    ht2 <<- setter(ht2, getter(ht1)[nrow(ht1),])
  }
  mapply(FUN = copy_down, huxtable_border_df$getter,
        huxtable_border_df$setter)

  ht2
}


#' Copy cell properties from `ht1`'s last column into `ht2`
#'
#' @param ht1 Left huxtable
#' @param ht2 Right huxtable
#'
#' @return `ht2`, modified.
#' @noRd
copy_properties_across <- function (ht1, ht2) {
  assert_that(is_huxtable(ht1), is_huxtable(ht2))

  for (a in copiable_cell_attrs()) {
    for (r in seq_len(ncol(ht2))) {
      attr(ht2, a)[, r] <- attr(ht1, a)[, ncol(ht1)]
    }
  }

  copy_across <- function (getter, setter) {
    ht2 <<- setter(ht2, getter(ht1)[, ncol(ht1)])
  }
  mapply(FUN = copy_across, huxtable_border_df$getter,
    huxtable_border_df$setter)

  ht2
}


copiable_cell_attrs <- function () {
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
merge_properties_down <- function (new_ht, ht1, ht2) {
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
  copy_borders_down <- function (getter, setter) {
    new_ht <<- setter(new_ht, rbind(getter(ht1), getter(ht2)))
  }
  mapply(FUN = copy_borders_down,
    huxtable_border_df$getter,
    huxtable_border_df$setter
  )

  if (is.numeric(rh <- row_height(new_ht))) {
    row_height(new_ht) <- rh/sum(rh)
  }

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
merge_properties_across <- function (new_ht, ht1, ht2) {
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

  copy_borders_across <- function (getter, setter) {
    new_ht <<- setter(new_ht, cbind(getter(ht1), getter(ht2)))
  }
  mapply(FUN = copy_borders_across,
    huxtable_border_df$getter,
    huxtable_border_df$setter
  )

  if (is.numeric(cw <- col_width(new_ht))) {
    col_width(new_ht) <- cw/sum(cw)
  }

  new_ht
}
