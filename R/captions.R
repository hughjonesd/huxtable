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


#' Return the vertical position of a caption
#'
#' @param ht A huxtable.
#' @return Either `"top"` or `"bottom"`.
#' @noRd
get_caption_vpos <- function(ht) {
  if (grepl("top", caption_pos(ht))) "top" else "bottom"
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


#' Resolve caption text and its label for an output format
#'
#' Adds the label syntax expected by bookdown and blogdown, while reporting
#' separately whether that syntax already contains the label. Renderers can
#' then decide whether they also need a native label without relying on
#' attributes attached to the caption string.
#'
#' @param ht A huxtable.
#' @param format Output format.
#' @return A list with five elements:
#' * `text`: caption text, including bookdown label syntax when required, or
#'   `NA` when there is no caption;
#' * `label`: the explicit or automatically generated table label, or `NA`;
#' * `label_in_caption`: whether `text` contains the label in bookdown syntax;
#' * `quarto_caption`: whether Quarto owns the table caption;
#' * `quarto_label`: whether Quarto owns the table label.
#' @noRd
resolve_caption <- function(ht, format = c("html", "latex", "md", "typst", "docx")) {
  format <- match.arg(format)
  cap <- caption(ht)
  lab <- label(ht)
  explicit_cap <- !is.na(cap) && nzchar(cap)
  explicit_lab <- !is.na(lab) && nzchar(lab)

  has_knitr <- requireNamespace("knitr", quietly = TRUE)
  chunk_options <- if (has_knitr) knitr::opts_current$get() else NULL
  chunk_label <- chunk_options$label
  if (length(chunk_label) > 0 && grepl("^unnamed-chunk", chunk_label)) {
    chunk_label <- NULL
  }

  is_quarto <- using_quarto()
  quarto_caption <- is_quarto &&
    (!is.null(chunk_options[["tbl-cap"]]) || !is.null(chunk_options[["tbl-subcap"]]))
  quarto_label <- is_quarto &&
    !is.null(chunk_label) &&
    nzchar(chunk_label) &&
    (quarto_caption || grepl("^tbl-", chunk_label))

  conflicts <- character()
  if (quarto_caption && explicit_cap) conflicts <- c(conflicts, "caption")
  if (quarto_label && explicit_lab) conflicts <- c(conflicts, "label")
  if (length(conflicts) > 0) {
    fields <- paste(conflicts, collapse = " and ")
    warning(
      "Quarto table options override the huxtable ", fields, ".",
      call. = FALSE
    )
  }

  if (quarto_caption) cap <- NA_character_
  if (quarto_label) lab <- NA_character_

  same_chunk <- identical(chunk_label, huxtable_env$autolabel_chunk$label) &&
    rlang::is_reference(chunk_options, huxtable_env$autolabel_chunk$options)
  if (!same_chunk) {
    huxtable_env$autolabel_cache <- list()
    huxtable_env$autolabel_chunk <- list(
      label = chunk_label,
      options = chunk_options
    )
  }
  used_labels <- if (!is.null(chunk_label)) {
    huxtable_env$autolabel_cache[[chunk_label]]
  } else {
    NULL
  }
  if (is.null(used_labels)) used_labels <- character()

  if (is.na(lab) &&
    !quarto_label &&
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

  if (!is.null(chunk_label) && !is.na(lab) && nzchar(lab)) {
    huxtable_env$autolabel_cache[[chunk_label]] <- unique(c(used_labels, lab))
  }

  label_in_caption <- FALSE

  if (!is.na(lab) && nzchar(lab) &&
    format != "docx" &&
    use_bookdown_style_captions()) {
    bookdown_label <- if (grepl("^tab:", lab)) lab else paste0("tab:", lab)
    # Bookdown needs a caption, even an empty one, to carry the label.
    if (is.na(cap)) cap <- ""
    cap <- if (format == "latex") {
      sprintf("(\\#%s) %s", bookdown_label, cap)
    } else {
      sprintf("(#%s) %s", bookdown_label, cap)
    }
    label_in_caption <- TRUE
  }

  list(
    text = cap,
    label = lab,
    label_in_caption = label_in_caption,
    quarto_caption = quarto_caption,
    quarto_label = quarto_label
  )
}
