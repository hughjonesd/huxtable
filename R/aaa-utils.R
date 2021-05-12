
#' @import assertthat
NULL


ncharw <- function (x, type = "width") {
  if (requireNamespace("crayon", quietly = TRUE)) {
    x <- crayon::strip_style(x)
  }
  # we use stringi throughout to keep the same concept of width
  switch(type,
         width = stringi::stri_width(x),
         chars = stringi::stri_length(x),
         stop("Unrecognized type in ncharw")
  )
}


is_vectorish <- function (x) is.null(dim(x)) && ! is.list(x)


is_numeric_or_character <- function (x) is.numeric(x) || is.character(x)


# pinched from rlang
`%||%` <- function (x, y) {
  if (is.null(x)) y else x
}


blank_where <- function (text, cond) {
  stopifnot(length(text) == length(cond))
  text[cond] <- ""
  text
}


nest_strings <- function(...) {
  l <- list(...)
  rev_l <- rev(l)
  surround1 <- function(inner, outer) paste0(outer[1], inner, outer[2],
    collapse = "")
  Reduce(surround1, rev_l)
}


# pinched from HMS. Registers the method or sets a hook to register it on load of other package
register_s3_method <- function (pkg, generic, class = "huxtable") {
  assert_that(is.string(pkg), is.string(generic))
  fun <- get(paste0(generic, ".", class), envir = parent.frame())

  if (pkg %in% loadedNamespaces()) {
    registerS3method(generic, class, fun, envir = asNamespace(pkg))
  }
  setHook(packageEvent(pkg, "onLoad"), function(...) {
    registerS3method(generic, class, fun, envir = asNamespace(pkg))
  })
}


assert_package <- function (fun, package, version = NULL) {
  if (! requireNamespace(package, quietly = TRUE)) stop(glue::glue(
        "`{fun}` requires the \"{package}\" package. To install, type:\n",
        "install.packages(\"{package}\")"))
  if (! is.null(version)) {
    cur_ver <- utils::packageVersion(package)
    if (cur_ver < version) stop(glue::glue(
          "`{fun}` requires version {version} or higher of the \"{package}\" ",
          "package. You have version {cur_ver} installed. To update the package,",
          "type:\n",
          "install.packages(\"{package}\")"))
  }
}


format_color <- function (r_color, default = "white") {
  r_color[is.na(r_color)] <- default
  apply(grDevices::col2rgb(r_color), 2, paste0, collapse = ", ")
}


get_visible_borders <- function (ht) {
  dc <- display_cells(ht)

  # a vertical border is hidden, if it is shadowed by a cell to its left
  vert_borders <- attr(ht, "lr_borders")$thickness
  left_shadowed <- dc[dc$display_col < dc$col, ]
  left_shadowed <- as.matrix(left_shadowed[c("row", "col")])
  vert_borders[left_shadowed] <- 0

  # a horizontal border is hidden, if it is shadowed by a cell above it
  horiz_borders <- attr(ht, "tb_borders")$thickness
  top_shadowed <- dc[dc$display_row < dc$row, ]
  top_shadowed <- as.matrix(top_shadowed[c("row", "col")])
  horiz_borders[top_shadowed] <- 0

  res <- list(vert = vert_borders, horiz = horiz_borders)
  return(res)
}


# returns two rows(+1),cols(+1) arrays of border colors.
collapsed_border_colors <- function (ht) {
  list(
    vert  = attr(ht, "lr_borders")$color,
    horiz = attr(ht, "tb_borders")$color
  )
}


# returns two rows(+1),cols(+1) arrays of border styles.
collapsed_border_styles <- function (ht) {
  list(
    vert  = attr(ht, "lr_borders")$style,
    horiz = attr(ht, "tb_borders")$style
  )
}


str_rep <- function(x, times) {
  mapply(function (s, t) paste0(rep(s, t), collapse = ""), x, times)
}


check_positive_dims <- function (ht) {
  if (nrow(ht) < 1) {
    warning("huxtable has zero rows")
    return(FALSE)
  }
  if (ncol(ht) < 1) {
    warning("huxtable has zero columns")
    return(FALSE)
  }

  return(TRUE)
}


#' Return data frame mapping real positions to the cells displayed in them
#'
#' @param ht A huxtable
#' @param all Show all cells, or only non-shadowed cells? Default TRUE
#' @param new_rowspan Possible new rowspan matrix
#' @param new_colspan Possible new colspan matrix
#'
#' @return
#' A data frame with columns:
#' * row, col: the real cell position
#' * shadowed: TRUE if a cell gets its content from another cell with
#'   colspan or rowspan > 1
#' * display_row, display_col: the "display cell" which provides the content
#' * rowspan, colspan: of the display cell
#' * end_row, end_col: right/bottom position of end of the merged cell
#' The data frame is ordered by row, then col.
#'
#' @noRd
display_cells <- function (ht, all = TRUE, new_rowspan = rowspan(ht), new_colspan = colspan(ht)) {
  rowspan <- new_rowspan
  colspan <- new_colspan
  display_row <- end_row <- row <- row(ht)
  display_col <- end_col <- col <- col(ht)
  displayers <- rowspan > 1 | colspan > 1
  touched <- shadowed <- matrix(FALSE, nrow(ht), ncol(ht))
  for (idx in which(displayers)) {
    rr <- row[idx]
    cc <- col[idx]
    end_r   <- rr + rowspan[idx] - 1
    end_c   <- cc + colspan[idx] - 1
    da_rows <- seq(rr, end_r)
    da_cols <- seq(cc, end_c)
    if (any(touched[da_rows, da_cols])) stop(glue::glue("Overlapping multirow/multicolumn cells in",
          " [{da_rows}, {da_cols}] of huxtable\n"))
    display_row[da_rows, da_cols] <- rr
    display_col[da_rows, da_cols] <- cc
    rowspan[da_rows, da_cols] <- rowspan[idx]
    colspan[da_rows, da_cols] <- colspan[idx]
    end_row[da_rows, da_cols] <- end_r
    end_col[da_rows, da_cols] <- end_c
    shadowed[da_rows, da_cols] <- TRUE
    touched[da_rows, da_cols]  <- TRUE
    shadowed[rr, cc] <- FALSE
  }

  dcells <- data.frame(
          row         = c(row),
          col         = c(col),
          rowspan     = c(rowspan),
          colspan     = c(colspan),
          display_row = c(display_row),
          display_col = c(display_col),
          shadowed    = c(shadowed),
          end_row     = c(end_row),
          end_col     = c(end_col)
        )
  if (! all) dcells <- dcells[! dcells$shadowed, ]

  return(dcells)
}


get_caption_hpos <- function (ht) {
  hpos <- sub(".*(left|center|right)", "\\1", caption_pos(ht))
  if (! hpos %in% c("left", "center", "right")) hpos <- position_no_wrap(ht)

  hpos
}


make_label <- function (ht) {
  lab <- label(ht)
  if (is.na(lab) &&
          getOption("huxtable.autolabel", TRUE) &&
          requireNamespace("knitr", quietly = TRUE) &&
          ! is.null(chunk_label <- knitr::opts_current$get("label"))
        ) {

    if (! is.null(chunk_label)) lab <- paste0("tab:", chunk_label)
  }

  lab
}


position_no_wrap <- function (ht) {
  switch(position(ht),
          "wrapleft"  = "left",
          "wrapright" = "right",
          position(ht)
        )
}


real_align <- function(ht) {
  # align(ht) can be e.g. "." for aligning on a decimal point
  al <- align(ht)
  al[! al %in% c("left", "center", "right")] <- "right"

  al
}
