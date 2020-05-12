

# unexported utility functions ---------------------------------------------------------------------

#' @import assertthat
NULL


ncharw <- function (x) nchar(x, type = "width")


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


assert_package <- function (fun, package) {
  if (! requireNamespace(package, quietly = TRUE)) stop(glue::glue(
        "`{fun}` requires the \"{package}\" package. To install, type:\n",
        "install.packages(\"{package}\")"))
}


# return character matrix of formatted contents, suitably escaped
clean_contents <- function(
        ht,
        type = c("latex", "html", "screen", "markdown", "word", "excel", "rtf"),
        ...
      ) {
  type <- match.arg(type)
  contents <- as.matrix(as.data.frame(ht))

  for (col in seq_len(ncol(contents))) {
    for (row in seq_len(nrow(contents))) {
      cell <- contents[row, col]
      num_fmt <- number_format(ht)[[row, col]] # a list element, double brackets
      cell <- format_numbers(cell, num_fmt)
      if (is.na(cell)) cell <- na_string(ht)[row, col]
      contents[row, col] <- as.character(cell)
    }
  }
  contents[is.na(contents)] <- na_string(ht)

  for (col in seq_len(ncol(contents))) {
    if (type %in% c("latex", "html", "rtf")) {
      to_esc <- escape_contents(ht)[, col]
      contents[to_esc, col] <-  sanitize(contents[to_esc, col], type)
    }
    # has to be after sanitization because we add &nbsp; for HTML (and non-space stuff for LaTeX):
    pad_chars <- rep(NA, length(col))
    align_pad   <- ncharw(align(ht)[, col]) == 1
    pad_chars[align_pad] <- align(ht)[align_pad, col]
    contents[, col] <- decimal_pad(contents[, col], pad_chars, type)
  }

  contents
}


format_color <- function (r_color, default = "white") {
  r_color[is.na(r_color)] <- default
  apply(grDevices::col2rgb(r_color), 2, paste0, collapse = ", ")
}


# returns two rows(+1),cols(+1) arrays of border widths
collapsed_borders <- function (ht) {
  result <- do_collapse(ht, get_all_borders, default = 0)
  result$vert <- pmax(result$left, result$right)
  result$horiz <- pmax(result$top, result$bottom)

  result[c("vert", "horiz")]
}


# returns two rows(+1),cols(+1) arrays of border colors. right and top borders have priority.
# A border of 0 can still have a color.
collapsed_border_colors <- function (ht) {
  result <- do_collapse(ht, get_all_border_colors, default = NA)
  result$vert <- result$right
  result$vert[is.na(result$right)] <- result$left[is.na(result$right)]
  result$horiz <- result$bottom
  result$horiz[is.na(result$bottom)] <- result$top[is.na(result$bottom)]

  result[c("vert", "horiz")]
}


# returns two rows(+1),cols(+1) arrays of border styles. Non-"solid" styles have priority;
# if two styles are non-"solid" then right and top has priority
# A border of 0 can still have a style.
collapsed_border_styles <- function (ht) {
  result <- do_collapse(ht, get_all_border_styles, default = "solid")
  result$vert <- result$right
  result$vert[result$right == "solid"] <- result$left[result$right == "solid"]
  result$horiz <- result$bottom
  result$horiz[result$bottom == "solid"] <- result$top[result$bottom == "solid"]

  result[c("vert", "horiz")]
}


do_collapse <- function(ht, prop_fun, default) {
  res <- list()
  res$top <- res$left <- res$right <- res$bottom <- matrix(default, nrow(ht), ncol(ht))
  dc <- display_cells(ht, all = TRUE)
  # provides large speedup:
  dc <- as.matrix(dc[, c("row", "col", "display_row", "display_col", "end_row", "end_col")])
  dc_idx <- dc[, c("display_row", "display_col"), drop = FALSE]
  dc_map <- matrix(seq_len(nrow(ht) * ncol(ht)), nrow(ht), ncol(ht))
  dc_map <- dc_map[dc_idx]

  at <- list()
  at$left   <- dc[, "col"] == dc[, "display_col"]
  at$right  <- dc[, "col"] == dc[, "end_col"]
  at$top    <- dc[, "row"] == dc[, "display_row"]
  at$bottom <- dc[, "row"] == dc[, "end_row"]

  properties <- prop_fun(ht)
  for (side in names(at)) {
    at_side <- at[[side]]
    res[[side]][at_side] <- properties[[side]][dc_map][at_side]
  }

  res$left <- cbind(res$left, default)
  res$right <- cbind(default, res$right)
  res$top <- rbind(res$top, default)
  res$bottom <- rbind(default, res$bottom)

  return(res)
}


# Format numeral generics
numeral_formatter <- function (x) {
  UseMethod("numeral_formatter")
}


numeral_formatter.default <- function (x) {
  stop("Unrecognized number_format. Please use a number, string or function.")
}


# If we are a function then return output from the function
numeral_formatter.function <- function (x) {
  return(x)
}


numeral_formatter.character <- function (x) {
  return(function(numeral) sprintf(x, numeral))
}


numeral_formatter.numeric <- function (x) {
  return(function(numeral) formatC(round(numeral, x), format = "f", digits = x))
}


# find each numeric substring, and replace it:
format_numbers <- function (string, num_fmt) {
  if (is.na(string)) return(NA_character_)

  # ! is.function avoids a warning if num_fmt is a function:
  if (! is.function(num_fmt) && is.na(num_fmt)) return(string)

  format_numeral <- numeral_formatter(num_fmt)
  # Breakdown:
  # -?                    optional minus sign
  # [0-9]*                followed by any number of digits
  # \\.?                  optionally followed by a decimal
  # [0-9]+                which may also be followed by any number of digits
  # ([eE]-?[0-9]+)?       optionally including e or E as in scientific notation
  #                       along with (optionally) a sign preceding the digits
  #                       specifying the level of the exponent.
  stringr::str_replace_all(string,  "-?[0-9]*\\.?[0-9]+([eE][+-]?[0-9]+)?", function (x) format_numeral(as.numeric(x)))
}


decimal_pad <- function(col, pad_chars, type) {
  # where pad_chars is NA we do not pad
  orig_col  <- col
  na_pad    <- is.na(pad_chars)
  col       <- col[! na_pad]
  pad_chars <- pad_chars[! na_pad]
  if (length(col) == 0) return(orig_col)

  find_pos  <- function(string, char) {
    regex <- gregexpr(char, string, fixed = TRUE)[[1]]
    regex[length(regex)]
  }
  pos <- mapply(find_pos, col, pad_chars)
  nchars <- nchar(col, type = "width")
  # take the biggest distance from the decimal point
  pos[pos == -1L] <- nchars[pos == -1L] + 1
  chars_after_. <- nchars - pos

  pad_n_spaces <- max(chars_after_.) - chars_after_.
  pad_char <- switch(type,
        "html"   = "&nbsp;",
        "latex"  = "~",
        "screen" = "\u00a0", # screen non-breaking space
        "rtf"    = "\\~",
        " ")
  col <- paste0(col, str_rep(pad_char, pad_n_spaces))

  orig_col[! na_pad] <- col
  orig_col
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


# return data frame mapping real cell positions to cells displayed. `all = TRUE` returns all
# cells, including those shadowed by others.
# data frame is ordered by row then column, i.e. the same as 1-based indexing into a matrix
# columns are row, col (of real cell);
# shadowed if cell is covered by another, the "display cell"; if not, it is its own "display cell";
# display_row, display_col, rowspan, colspan, end_row, end_col of the display cell.
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
          " [{da_rows}, {da_cols}] of huxtable"))
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


# this relies on the fact that knitr documents are knit in their own process.
huxtable_env$SEEN_LABELS <- list()


make_label <- function (ht) {
  lab <- label(ht)
  if (is.na(lab) &&
          getOption("huxtable.autolabel", TRUE) &&
          requireNamespace("knitr", quietly = TRUE) &&
          ! is.null(chunk_label <- knitr::opts_current$get("label"))
        ) {
    chunk_idx <- ""
    if (! is.null(count <- huxtable_env$SEEN_LABELS[[chunk_label]])) {
      chunk_idx <- paste0("-", count)
    }
    if (! is.null(chunk_label)) lab <- paste0("tab:", chunk_label, chunk_idx)
    huxtable_env$SEEN_LABELS[[chunk_label]] <- if (is.null(count)) 1 else count + 1
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
