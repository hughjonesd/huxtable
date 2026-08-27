# Caption and label helpers ------------------------------------------------------------------------


#' Return the horizontal position of a caption
#'
#' Falls back to the table position when `caption_pos()` has no horizontal
#' component.
#'
#' @param ht A huxtable.
#' @return One of `"left"`, `"center"` or `"right"`.
#' @noRd
get_caption_hpos <- function(ht) {
  hpos <- sub(".*(left|center|right)", "\\1", caption_pos(ht))
  if (!hpos %in% c("left", "center", "right")) hpos <- position_no_wrap(ht)

  hpos
}


#' Resolve a table label for output
#'
#' Explicit labels are returned unchanged. Within named knitr chunks, automatic
#' labels are derived from the chunk label and made unique among labels already
#' used by huxtable in the chunk.
#'
#' @param ht A huxtable.
#' @return A string or `NA`.
#' @noRd
make_label <- function(ht) {
  lab <- label(ht)

  has_knitr <- requireNamespace("knitr", quietly = TRUE)
  chunk_label <- if (has_knitr) knitr::opts_current$get("label") else NULL
  if (length(chunk_label) > 0 && grepl("^unnamed-chunk", chunk_label)) {
    chunk_label <- NULL
  }

  used_labels <- if (!is.null(chunk_label)) {
    knitr::opts_current$get("huxtable.used_labels")
  } else {
    NULL
  }
  if (is.null(used_labels)) used_labels <- character()

  if (is.na(lab) &&
    getOption("huxtable.autolabel", TRUE) &&
    !is.null(chunk_label)) {
    base_label <- paste0("tab:", chunk_label)
    lab <- base_label
    suffix <- 2L
    while (lab %in% used_labels) {
      lab <- paste0(base_label, "-", suffix)
      suffix <- suffix + 1L
    }
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

  if (!is.null(chunk_label) && !is.na(lab) && nzchar(lab)) {
    knitr::opts_current$set(
      huxtable.used_labels = unique(c(used_labels, lab))
    )
  }

  lab
}


#' Detect whether bookdown-style captions are needed
#'
#' @return A logical scalar.
#' @noRd
use_bookdown_style_captions <- function() {
  if (!is.null(book_opt <- getOption("huxtable.bookdown", NULL))) {
    return(book_opt)
  }

  if (!requireNamespace("knitr", quietly = TRUE)) {
    return(FALSE)
  }
  if (!requireNamespace("rmarkdown", quietly = TRUE)) {
    return(FALSE)
  }
  input_path <- knitr::current_input(dir = TRUE)
  if (is.null(input_path)) {
    return(FALSE)
  }
  rmd_of <- rmarkdown::default_output_format(input_path)$name

  return(grepl("bookdown|blogdown", rmd_of))
}


#' Build caption text for an output format
#'
#' Adds the label syntax expected by bookdown and blogdown. Other output modes
#' receive the raw caption unchanged.
#'
#' @param ht A huxtable.
#' @param label Resolved table label.
#' @param format Output format.
#' @return Caption text or `NA`.
#' @noRd
make_caption <- function(ht, label, format = c("html", "latex", "md", "typst")) {
  format <- match.arg(format)

  raw_cap <- caption(ht)

  if (is.na(label) || label == "") {
    return(raw_cap)
  }
  if (!use_bookdown_style_captions()) {
    return(raw_cap)
  }

  if (!grepl("^tab:", label)) label <- paste0("tab:", label)

  # even if there's no caption, we make one if we need it for the label:
  if (is.na(raw_cap)) raw_cap <- ""
  cap_with_label <- if (format == "latex") {
    sprintf("(\\#%s) %s", label, raw_cap)
  } else {
    sprintf("(#%s) %s", label, raw_cap)
  }

  # LaTeX uses this to avoid adding a second label outside the caption.
  if (format == "latex") attr(cap_with_label, "has_label") <- TRUE
  return(cap_with_label)
}
