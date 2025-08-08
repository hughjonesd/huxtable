#' Experimental screen output using pillar and cli
#'
#' Render a huxtable by delegating column formatting to
#' [pillar::tbl_format_setup()] and drawing a surrounding box via
#' [cli::cat_boxx()]. This is a minimal proof of concept and is not
#' feature complete. Row and column spans are respected by blanking
#' covered cells.
#'
#' @param ht A huxtable.
#' @return The input huxtable, invisibly.
#' @noRd
cli_pillar_screen <- function(ht) {
  if (!requireNamespace("pillar", quietly = TRUE) ||
      !requireNamespace("cli", quietly = TRUE) ||
      !requireNamespace("tibble", quietly = TRUE)) {
    stop("Packages 'pillar', 'cli' and 'tibble' are required for cli_pillar_screen().")
  }
  df <- as.data.frame(ht, stringsAsFactors = FALSE)
  rs <- rowspan(ht)
  cs <- colspan(ht)
  mat <- as.matrix(df)
  for (r in seq_len(nrow(mat))) {
    for (c in seq_len(ncol(mat))) {
      if (rs[r, c] > 1 || cs[r, c] > 1) {
        rr <- r + seq_len(rs[r, c]) - 1
        cc <- c + seq_len(cs[r, c]) - 1
        for (ri in rr) {
          for (ci in cc) {
            if (ri != r || ci != c) mat[ri, ci] <- ""
          }
        }
      }
    }
  }
  df <- tibble::as_tibble(mat, .name_repair = "minimal")
  setup <- pillar::tbl_format_setup(df, width = getOption("width"))
  lines <- c(
    pillar::tbl_format_header(setup),
    pillar::tbl_format_body(setup),
    pillar::tbl_format_footer(setup)
  )
  cli::cat_boxx(paste(lines, collapse = "\n"), padding = 0)
  invisible(ht)
}
