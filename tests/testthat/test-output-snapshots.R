skip_without_latex_deps <- function() {
  skip("LaTeX dependencies not available")
}

skip_without_typst <- function() {
  if (Sys.which("typst") == "") skip("typst CLI not found")
}

make_tables <- function() {
  base <- hux(a = 1:3, b = 4:6, c = 7:9)
  long <- hux(text = c("This is a very very long piece of text that should wrap", "short", "medium"),
              num = 1:3)
  list(
    basic = base,
    align = set_align(base, 1:3, 1:3, "center"),
    bold = set_bold(base, 1, 1:3, TRUE),
    cell_background = set_background_color(base, 1, 1, "yellow"),
    text_color = set_text_color(base, 2, 2, "red"),
    column_width = set_col_width(base, 1:3, c(.2, .3, .5)),
    row_height = set_row_height(base, 1:3, c(.2, .3, .5)),
    row_background = set_background_color(base, 2, 1:3, "grey90"),
    column_span = {ht <- base; ht <- set_colspan(ht, 1, 1, 2); ht[1, 2] <- NA; ht},
    row_span = {ht <- base; ht <- set_rowspan(ht, 1, 1, 2); ht[2, 1] <- NA; ht},
    caption = set_caption(base, "Sample caption"),
    borders = set_all_borders(base, 1:3, 1:3, 1),
    padding = set_all_padding(base, 1:3, 1:3, 4),
    rotation = {ht <- base; ht <- set_rotation(ht, 1, 1, 90); ht},
    wrap = {ht <- long; ht <- set_wrap(ht, 1:3, 1, TRUE); ht}
  )
}


test_that("pdf snapshots", {
  skip_without_latex_deps()
  tables <- make_tables()
  for (nm in names(tables)) {
    f <- tempfile(fileext = ".pdf")
    quick_pdf(tables[[nm]], file = f, open = FALSE)
    expect_snapshot_file(f, paste0(nm, ".pdf"))
  }
})


test_that("typst pdf snapshots", {
  skip_without_typst()
  tables <- make_tables()
  for (nm in names(tables)) {
    f <- tempfile(fileext = ".pdf")
    quick_typst_pdf(tables[[nm]], file = f, open = FALSE)
    expect_snapshot_file(f, paste0(nm, "-typst.pdf"))
  }
})


test_that("docx snapshots", {
  skip_if_not_installed("officer")
  skip_if_not_installed("flextable")
  tables <- make_tables()
  for (nm in names(tables)) {
    f <- tempfile(fileext = ".docx")
    quick_docx(tables[[nm]], file = f, open = FALSE)
    expect_snapshot_file(f, paste0(nm, ".docx"))
  }
})

