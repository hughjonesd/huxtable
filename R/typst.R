#' Build Typst cell options
#'
#' Retrieve cell padding and translate to Typst inset arguments.
#'
#' @param ht A huxtable.
#' @return A character matrix of Typst cell options.
#' @noRd
#'
typst_cell_options <- function(ht) {
  left <- left_padding(ht)
  right <- right_padding(ht)
  top <- top_padding(ht)
  bottom <- bottom_padding(ht)
  nrows <- nrow(ht)
  ncols <- ncol(ht)
  opts <- matrix("", nrow = nrows, ncol = ncols)
  for (r in seq_len(nrows)) {
    for (c in seq_len(ncols)) {
      pads <- c(
        top = top[r, c], right = right[r, c],
        bottom = bottom[r, c], left = left[r, c]
      )
      pads <- pads[!is.na(pads)]
      if (!length(pads)) next
      if (length(pads) == 4 && length(unique(pads)) == 1) {
        opts[r, c] <- sprintf("inset: %spt", pads[1])
      } else {
        parts <- paste0(names(pads), ": ", pads, "pt")
        opts[r, c] <- paste0("inset: (", paste(parts, collapse = ", "), ")")
      }
    }
  }
  opts
}
