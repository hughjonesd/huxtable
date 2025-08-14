
skip_on_cran()

skip_without_typst <- function() {
  if (Sys.which("typst") == "") skip("typst CLI not found")
}

make_tables <- function() {
  base <- hux(a = 1:3, b = 4:6, c = letters[1:3])
  base <- set_all_borders(base)
  base[1, ] <- c("This is a very very long piece of text",
                 "short", "Lorem ipsum dolor sit amet")

  list(
    basic = base,
    align = set_align(base, 1:3, 1:3, "center"),
    bold = set_bold(base, 1, 1:3, TRUE),
    text_color = set_text_color(base, 2, 2, "red"),
    col_width = set_col_width(base, c(.2, .3, .5)),
    row_height = set_row_height(base, c(.15, .15, .3, .5)),
    background_color = set_background_color(base, 2, 1:3, "grey90"),
    colspan = set_colspan(base, 1, 1, 2),
    rowspan = set_rowspan(base, 1, 1, 2),
    caption = set_caption(base, "Sample caption"),
    borders = set_all_borders(base, 1:3, 1:3, 1),
    padding = set_all_padding(base, 1:3, 1:3, 4),
    rotation = set_rotation(base, 1, 1, 90),
    wrap = set_wrap(base, 1:3, 1, TRUE)
  )
}


test_that("pdf snapshots", {
  tables <- make_tables()
  for (nm in names(tables)) {
    f <- tempfile(pattern = nm, fileext = ".pdf")
    quick_pdf(tables[[nm]], file = f, open = FALSE)
    expect_snapshot_file(f, paste0(nm, ".pdf"))
  }
})


test_that("typst pdf snapshots", {
  skip_without_typst()
  tables <- make_tables()
  for (nm in names(tables)) {
    f <- tempfile(pattern = nm, fileext = ".pdf")
    quick_typst_pdf(tables[[nm]], file = f, open = FALSE)
    expect_snapshot_file(f, paste0(nm, "-typst.pdf"))
  }
})


test_that("docx snapshots", {
  skip_if_not_installed("officer")
  skip_if_not_installed("flextable")
  tables <- make_tables()
  for (nm in names(tables)) {
    f <- tempfile(pattern = nm, fileext = ".docx")
    quick_docx(tables[[nm]], file = f, open = FALSE)
    expect_snapshot_file(f, paste0(nm, ".docx"))
  }
})


test_that("html snapshots", {
  tables <- make_tables()
  for (nm in names(tables)) {
    f <- tempfile(pattern = nm, fileext = ".docx")
    quick_html(tables[[nm]], file = f, open = FALSE)
    expect_snapshot_file(f, paste0(nm, ".html"))
  }
})
