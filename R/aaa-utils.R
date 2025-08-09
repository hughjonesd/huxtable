#' Is an object a vector (but not a list)?
#'
#' @param x Object to test.
#' @return Logical scalar.
#' @noRd
is_vectorish <- function(x) is.null(dim(x)) && !is.list(x)

#' Is an object numeric or character?
#'
#' @param x Object to test.
#' @return Logical scalar.
#' @noRd
is_numeric_or_character <- function(x) is.numeric(x) || is.character(x)

# pinched from rlang
#' Return left-hand side unless `NULL`
#'
#' `%||%` returns `y` if `x` is `NULL`, otherwise `x`.
#'
#' @param x,y Objects to combine.
#' @return `x` or `y`.
#' @noRd
`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

#' Nest a series of strings
#'
#' Wraps inner strings with outer strings sequentially.
#'
#' @param ... Pairs of strings giving opening and closing parts.
#' @return A combined string.
#' @noRd
nest_strings <- function(...) {
  l <- list(...)
  rev_l <- rev(l)
  surround1 <- function(inner, outer) {
    paste0(outer[1], inner, outer[2],
      collapse = ""
    )
  }
  Reduce(surround1, rev_l)
}


#' Format an R color for CSS output
#'
#' @param r_color Vector of R color names or hex codes.
#' @param default Default color for `NA` values.
#' @return Character vector of "r,g,b" values.
#' @noRd
format_color <- function(r_color, default = "white") {
  r_color[is.na(r_color)] <- default
  apply(grDevices::col2rgb(r_color), 2, paste0, collapse = ", ")
}


#' Check that a huxtable has positive dimensions
#'
#' @param ht A huxtable.
#' @return Logical scalar; issues a warning if dimensions are zero.
#' @noRd
check_positive_dims <- function(ht) {
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
display_cells <- function(ht, all = TRUE, new_rowspan = rowspan(ht), new_colspan = colspan(ht)) {
  rowspan <- new_rowspan
  colspan <- new_colspan
  display_row <- end_row <- row <- row(ht)
  display_col <- end_col <- col <- col(ht)
  displayers <- rowspan > 1 | colspan > 1
  touched <- shadowed <- matrix(FALSE, nrow(ht), ncol(ht))
  for (idx in which(displayers)) {
    rr <- row[idx]
    cc <- col[idx]
    end_r <- rr + rowspan[idx] - 1
    end_c <- cc + colspan[idx] - 1
    da_rows <- seq(rr, end_r)
    da_cols <- seq(cc, end_c)
    if (any(touched[da_rows, da_cols])) {
      stop(glue::glue(
        "Overlapping multirow/multicolumn cells in",
        " [{da_rows}, {da_cols}] of huxtable\n"
      ))
    }
    display_row[da_rows, da_cols] <- rr
    display_col[da_rows, da_cols] <- cc
    rowspan[da_rows, da_cols] <- rowspan[idx]
    colspan[da_rows, da_cols] <- colspan[idx]
    end_row[da_rows, da_cols] <- end_r
    end_col[da_rows, da_cols] <- end_c
    shadowed[da_rows, da_cols] <- TRUE
    touched[da_rows, da_cols] <- TRUE
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
  if (!all) dcells <- dcells[!dcells$shadowed, ]

  return(dcells)
}


get_caption_hpos <- function(ht) {
  hpos <- sub(".*(left|center|right)", "\\1", caption_pos(ht))
  if (!hpos %in% c("left", "center", "right")) hpos <- position_no_wrap(ht)

  hpos
}


make_label <- function(ht) {
  lab <- label(ht)

  has_knitr <- requireNamespace("knitr", quietly = TRUE)
  chunk_label <- if (has_knitr) knitr::opts_current$get("label") else NULL
  if (length(chunk_label) > 0 && grepl("^unnamed-chunk", chunk_label)) {
    chunk_label <- NULL
  }

  if (is.na(lab) &&
    getOption("huxtable.autolabel", TRUE) &&
    has_knitr &&
    !is.null(chunk_label)) {
    lab <- paste0("tab:", chunk_label)
  }

  if (!is.null(chunk_label) &&
    using_quarto("1.4") &&
    getOption(
      "huxtable.knitr_output_format",
      guess_knitr_output_format()
    ) == "latex"
  ) {
    msg <- paste(
      "quarto cell labels do not work with huxtable in TeX for quarto ",
      "version 1.4 or above.",
      "Use huxtable labels instead via `label()` or `set_label()`.",
      "See `?huxtable-FAQ` for more details.",
      sep = "\n"
    )
    if (grepl("^tbl-", chunk_label)) {
      stop(msg)
    } else {
      warning(msg)
    }
  }

  lab
}


using_quarto <- function(min_version = NULL) {
  if (!requireNamespace("knitr", quietly = TRUE)) {
    return(FALSE)
  }
  if (is.null(knitr::opts_knit$get("quarto.version"))) {
    return(FALSE)
  }
  if (is.null(min_version)) {
    return(TRUE)
  }

  # this is risky since they could have quarto without the R package
  if (requireNamespace("quarto", quietly = TRUE)) {
    qv <- quarto::quarto_version()
  } else {
    quarto_path <- Sys.which("quarto")
    if (quarto_path == "") {
      return(FALSE)
    }
    # copy-pasted from quarto package
    qv <- system2(quarto_path, "--version", stdout = TRUE)
    qv <- as.numeric_version(qv)
  }

  return(qv >= min_version)
}


position_no_wrap <- function(ht) {
  switch(position(ht),
    "wrapleft"  = "left",
    "wrapright" = "right",
    position(ht)
  )
}


real_align <- function(ht) {
  # align(ht) can be e.g. "." for aligning on a decimal point
  al <- align(ht)
  al[!al %in% c("left", "center", "right")] <- "right"

  al
}
