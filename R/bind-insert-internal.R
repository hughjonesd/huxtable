
bind_hux <- function (..., type, copy_cell_props) {

  objs <- list(...)
  arg_names <- names(objs)

  objs <- lapply(seq_along(objs), function(idx) {
    x <- objs[[idx]]
    if (is.vector(x) || is.factor(x)) {
      x <- as.matrix(x)
      if (! is.null(arg_names) && nzchar(arg_names[idx])) colnames(x) <- arg_names[idx]
      if (type == "rbind") x <- t(x)
    }
    if (is.null(attr(x, "from_real_hux"))) attr(x, "from_real_hux") <- is_hux(x)
    x
  })

  f <- function (ht, x) bind2_hux(ht, x, type, copy_cell_props = copy_cell_props)
  res <- Reduce(f, objs)

  daddy <- Find(is_hux, objs)
  unchanged_attrs <- switch(type, "cbind" = huxtable_row_attrs, "rbind" = huxtable_col_attrs)
  for (a in c(unchanged_attrs, huxtable_table_attrs)) attr(res, a) <- attr(daddy, a)

  attr(res, "from_real_hux") <- NULL
  res
}


bind2_hux <- function (ht, x, type, copy_cell_props) {
  ht_real_hux <- attr(ht, "from_real_hux")
  x_real_hux  <- attr(x, "from_real_hux")

  ht <- as_hux(ht, autoformat = FALSE, add_colnames = FALSE)
  x  <- as_hux(x, autoformat = FALSE, add_colnames = FALSE)
  if (isTRUE(copy_cell_props)) {
    ccp <- setdiff(huxtable_cell_attrs, c("colspan", "rowspan"))
    if (! x_real_hux  && nrow(x) > 0 && ncol(x) > 0) {
      for (a in ccp) {
        attr(x, a)[] <- if (type == "cbind") attr(ht, a)[, ncol(ht)] else
          matrix(attr(ht, a)[nrow(ht), ], nrow(x), ncol(x), byrow = TRUE)
      }
      if (type == "rbind") {
        attr(x, "row_height")[seq_len(nrow(x))] <- attr(ht, "row_height")[nrow(ht)]
      } else {
        attr(x, "col_width")[seq_len(ncol(x))] <- attr(ht, "col_width")[ncol(ht)]
      }
      if (type == "cbind") {
        border_props <- huxtable_border_df[huxtable_border_df$side != "left",]
        copy_props <- function (getter, setter) {
          x <<- setter(x, getter(ht)[, ncol(ht)])
        }
      } else {
        border_props <- huxtable_border_df[huxtable_border_df$side != "top", ]
        copy_props <- function (getter, setter) {
          x <<- setter(x, getter(ht)[nrow(ht), ])
        }
      }
      mapply(FUN = copy_props, border_props$getter, border_props$setter)
    }

    if (! ht_real_hux && x_real_hux && nrow(ht) > 0 && ncol(ht) > 0) {
      for (a in ccp) {
        attr(ht, a)[] <- if (type == "cbind") attr(x, a)[, 1] else
          matrix(attr(x, a)[1, ], nrow(ht), ncol(ht), byrow = TRUE)
      }
      if (type == "rbind") {
        attr(ht, "row_height")[seq_len(nrow(ht))] <- attr(x, "row_height")[1]
      } else {
        attr(ht, "col_width")[seq_len(ncol(ht))] <- attr(x, "col_width")[1]
      }
      if (type == "cbind") {
        border_props <- huxtable_border_df[huxtable_border_df$side != "right", ]
        copy_props <- function (getter, setter) {
          ht <<- setter(ht, getter(x)[, 1])
        }
      } else {
        border_props <- huxtable_border_df[huxtable_border_df$side != "top", ]
        copy_props <- function (getter, setter) {
          ht <<- setter(ht, getter(x)[1, ])
        }
      }
      mapply(FUN = copy_props, border_props$getter, border_props$setter)
    }
  }


  bind_df <- switch(type, "cbind" = cbind.data.frame, "rbind" = function (x, y) {
    if(ncol(x) != ncol(y)) {
      stop("Can't rbind objects as they have different numbers of columns")
    }
    rbind.data.frame(x, setNames(y, names(x)), stringsAsFactors = FALSE)
  })

  res <- new_huxtable(bind_df(ht, x))
  res <- merge_props(res, ht, x, type = type, copy_cell_props = copy_cell_props)

  attr(res, "from_real_hux") <- x_real_hux || ht_real_hux
  res
}


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


# returns res with properties created from 'first' and 'second' huxtables
merge_props <- function (res, first, second, type = c("cbind", "rbind"),
  copy_cell_props = FALSE) {
  type <- match.arg(type)
  # if second is not a huxtable, make it a huxtable; and if ccp is TRUE, copy properties over:
  #  - cell properties copied L-R from last col (cbind) or T-B from last row (rbind)
  #  - row  properties copied from last row (rbind)
  #  - col  properties copied from last col (cbind)
  #  - lr_borders extended
  if (! is_huxtable(second)) {
    second <- new_huxtable(second)
    if (is.character(copy_cell_props)) {
      ccp <- intersect(copy_cell_props, huxtable_cell_attrs)
      for (a in ccp) {
        attr(second, a)[] <- if (type == "cbind") attr(first, a)[, ncol(first)] else
          matrix(attr(first, a)[nrow(first), ], nrow(second), ncol(second), byrow = TRUE)
      }
      if (type == "rbind") for (a in huxtable_row_attrs) {
        attr(second, a) <- rep(attr(first, a)[nrow(first)], nrow(second))
      }
      if (type == "cbind") for (a in huxtable_col_attrs) {
        attr(second, a) <- rep(attr(first, a)[ncol(first)], ncol(second))
      }

    }
  }
  # c- or rbind first and second's properties into res, as follows:
  #  - first gets priority for table properties;
  #  - all cell properties are just c- or rbinded
  #  - row properties are concatenated if type=='rbind', otherwise they are from `first`
  #  - col properties are concatenated if type=='cbind', otherwise they are from `first`

  for (a in huxtable_table_attrs) {
    attr(res, a) <- attr(first, a)
  }
  bind_cells <- switch(type, "cbind" = cbind, "rbind" = rbind)
  for (a in huxtable_cell_attrs) {
    attr(res, a) <- bind_cells(attr(first, a), attr(second, a))
  }
  join_attrs  <- switch(type, "cbind" = huxtable_col_attrs, "rbind" = huxtable_row_attrs)
  first_attrs <- switch(type, "cbind" = huxtable_row_attrs, "rbind" = huxtable_col_attrs)
  for (a in join_attrs) {
    attr(res, a) <- c(attr(first, a), attr(second, a))
  }
  for (a in first_attrs) {
    attr(res, a) <- attr(first, a)
  }

  class(res) <- c("huxtable", class(res))
  # borders are simply joined on; right and bottom take priority as usual.
  copy_props <- function (getter, setter) {
    res <<- setter(res, bind_cells(getter(first), getter(second)))
  }
  mapply(FUN = copy_props,
    huxtable_border_df$getter,
    huxtable_border_df$setter
  )

  # numeric row/col heights are rescaled to add to 1
  for (rh_cw in c("row_height", "col_width")) {
    if (is.numeric(attr(res, rh_cw))) {
      values <- attr(res, rh_cw)
      attr(res, rh_cw) <- values / sum(values)
    }
  }

  res
}
