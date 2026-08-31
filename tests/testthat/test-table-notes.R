test_that("table notes are separate table metadata", {
  ht <- hux(a = 1:2, b = 3:4)
  original_nrow <- nrow(ht)

  expect_length(table_notes(ht), 0L)
  ht <- set_table_notes(ht, c("Note one", "Note two"))
  ht <- add_table_note(ht, "Note three")

  expect_equal(table_notes(ht), c("Note one", "Note two", "Note three"))
  expect_equal(nrow(ht), original_nrow)
  expect_equal(table_notes(ht[1:2, 1, drop = FALSE]), table_notes(ht))

  table_notes(ht) <- NULL
  expect_length(table_notes(ht), 0L)
})


test_that("table note values are validated", {
  ht <- hux(a = 1)
  expect_error(set_table_notes(ht, 1))
  expect_error(set_table_notes(ht, c("Note", NA)), "cannot contain")
  expect_error(add_table_note(ht, NA_character_))
})


test_that("text outputs render table notes", {
  ht <- set_table_notes(
    hux(a = 1, b = 2),
    c("Note: A & B", "Source: x_y")
  )

  html <- to_html(ht, dependencies = FALSE)
  expect_match(html, "<tfoot class=\"huxtable-notes\">", fixed = TRUE)
  expect_match(html, "colspan=\"2\">Note: A &amp; B", fixed = TRUE)

  latex <- to_latex(ht, dependencies = FALSE)
  expect_match(latex, "\\begin{tablenotes}[flushleft]", fixed = TRUE)
  expect_match(latex, "\\item[] Note: A \\& B", fixed = TRUE)
  expect_match(latex, "Source: x\\_y", fixed = TRUE)

  typst <- to_typst(ht)
  expect_match(typst, "table.footer(", fixed = TRUE)
  expect_match(typst, "repeat: false", fixed = TRUE)
  expect_match(typst, "table.cell(colspan: 2)[Source: x\\_y]", fixed = TRUE)

  expect_match(to_md(ht), "Note: A & B\n\nSource: x_y", fixed = TRUE)
  expect_match(to_rtf(ht), "{\\pard \\ql {Note: A & B} \\par}", fixed = TRUE)
  expect_match(to_screen(ht, color = FALSE), "Source: x_y", fixed = TRUE)
})


test_that("breakable LaTeX tables render notes once at the end", {
  ht <- set_breakable(
    set_table_notes(hux(a = 1:2), c("Note one", "Note two")),
    TRUE
  )
  latex <- to_latex(ht, dependencies = FALSE)

  expect_match(latex, "\\begin{ThreePartTable}", fixed = TRUE)
  expect_match(latex, "\\begin{TableNotes}[flushleft]", fixed = TRUE)
  expect_match(latex, "\\insertTableNotes\n\\endlastfoot", fixed = TRUE)
  expect_length(gregexpr("Note one", latex, fixed = TRUE)[[1]], 1L)
})


test_that("flextable uses its footer for table notes", {
  skip_if_not_installed("flextable")
  ht <- set_table_notes(hux(a = 1, b = 2), c("Note one", "Note two"))
  ft <- as_flextable(ht)

  expect_equal(nrow(ft$footer$dataset), 2L)
  expect_equal(ft$footer$dataset[[1]], c("Note one", "Note two"))
})


test_that("Excel writes notes below data and above a bottom caption", {
  skip_if_not_installed("openxlsx")
  ht <- set_caption_pos(
    set_caption(
      set_table_notes(hux(a = 1, b = 2), c("Note one", "Note two")),
      "Bottom caption"
    ),
    "bottom"
  )
  path <- tempfile(fileext = ".xlsx")
  on.exit(unlink(path))

  wb <- as_Workbook(ht)
  openxlsx::saveWorkbook(wb, path)
  contents <- openxlsx::read.xlsx(
    path, colNames = FALSE, skipEmptyRows = FALSE
  )

  expect_equal(contents[[1]], c("a", "1", "Note one", "Note two", "Bottom caption"))
})


test_that("add_footnote keeps its compatibility behaviour", {
  ht <- add_footnote(hux(a = 1, b = 2), "Legacy note")

  expect_equal(nrow(ht), 3L)
  expect_equal(ht[[3, 1]], "Legacy note")
  expect_length(table_notes(ht), 0L)
})


test_that("huxreg uses explicit table notes", {
  model <- lm(Sepal.Width ~ Sepal.Length, iris)
  ht <- huxreg(model)

  expect_length(table_notes(ht), 1L)
  expect_match(table_notes(ht), "p <", fixed = TRUE)
})
