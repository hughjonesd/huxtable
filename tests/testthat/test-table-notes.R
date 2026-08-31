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
  expect_error(set_table_notes(ht, c("Note", NA)))
  expect_error(add_table_note(ht, NA_character_))
})


test_that("cell notes are cell properties resolved when rendering", {
  ht <- hux(matrix(1:4, nrow = 2), add_colnames = FALSE)
  expect_true(all(is.na(cell_note(ht))))

  ht <- set_cell_note(ht, 1:2, 1, "Shared note")
  ht <- set_cell_note(ht, 1, 2, "Other note")
  resolved <- resolve_cell_notes(ht)

  expect_equal(resolved$notes, c("Shared note", "Other note"))
  expect_equal(resolved$markers, c("1", "2"))
  expect_equal(
    unname(resolved$cell_markers),
    matrix(c("1", "1", "2", NA), nrow = 2)
  )
  expect_equal(unname(cell_note(ht)[1:2, 1]), rep("Shared note", 2))

  subset <- ht[2, , drop = FALSE]
  expect_equal(resolve_cell_notes(subset)$markers, "1")
  expect_equal(resolve_cell_notes(subset)$notes, "Shared note")

  ordered <- hux(matrix(1:4, nrow = 2), add_colnames = FALSE)
  ordered <- set_cell_note(ordered, 2, 1, "Later row")
  ordered <- set_cell_note(ordered, 1, 2, "Earlier row")
  expect_equal(resolve_cell_notes(ordered)$notes, c("Earlier row", "Later row"))
})


test_that("cell note marks support named and custom sequences", {
  ht <- hux(seq_len(28), add_colnames = FALSE)
  cell_note(ht) <- paste0("Note ", seq_len(28))

  expect_equal(resolve_cell_notes(ht)$markers, as.character(seq_len(28)))
  expect_equal(
    resolve_cell_notes(set_note_symbol(ht, "roman"))$markers[1:4],
    c("i", "ii", "iii", "iv")
  )
  expect_equal(
    tail(resolve_cell_notes(set_note_symbol(ht, "alphabetic"))$markers, 3),
    c("z", "aa", "ab")
  )
  expect_equal(
    resolve_cell_notes(set_note_symbol(ht[1:5, ], "+*"))$markers,
    c("+", "*", "++", "+*", "*+")
  )

  expect_error(set_note_symbol(ht, ""))
  expect_error(set_note_symbol(ht, 1))
})


test_that("notes on shadowed cells are ignored", {
  ht <- hux(a = 1, b = 2, add_colnames = FALSE)
  ht <- set_colspan(ht, 1, 1, 2)
  ht <- set_cell_note(ht, 1, 2, "Hidden note")

  expect_length(resolve_cell_notes(ht)$notes, 0L)
})


test_that("text outputs render table notes", {
  ht <- set_note_symbol(set_table_notes(
    hux(a = 1, b = 2),
    c("Note: A & B", "Source: x_y")
  ), "+*")
  ht <- set_cell_note(ht, 2, 1, "Shared_note")
  ht <- set_cell_note(ht, 2, 2, "Other note")

  html <- to_html(ht, dependencies = FALSE)
  expect_match(html, "<tfoot class=\"huxtable-notes\">", fixed = TRUE)
  expect_match(html, "colspan=\"2\">Note: A &amp; B", fixed = TRUE)
  expect_match(html, "1<sup class=\"huxtable-note-ref\">+</sup>", fixed = TRUE)
  expect_match(html, "<sup class=\"huxtable-note-ref\">+</sup> Shared_note", fixed = TRUE)
  css <- huxtable_html_css()
  expect_match(css, ".huxtable-note {", fixed = TRUE)
  expect_match(css, "padding-bottom: 2pt;", fixed = TRUE)
  expect_match(css, "padding-top: 2pt;", fixed = TRUE)
  expect_match(css, "text-align: left;", fixed = TRUE)

  latex <- to_latex(ht, dependencies = FALSE)
  expect_match(latex, "\\begin{tablenotes}[flushleft]", fixed = TRUE)
  expect_match(latex, "\\item[] Note: A \\& B", fixed = TRUE)
  expect_match(latex, "Source: x\\_y", fixed = TRUE)
  expect_match(latex, "1\\tnote{+}", fixed = TRUE)
  expect_match(latex, "\\item[+] Shared\\_note", fixed = TRUE)

  typst <- to_typst(ht)
  expect_match(typst, "table.footer(", fixed = TRUE)
  expect_match(typst, "repeat: false", fixed = TRUE)
  expect_match(typst, "table.cell(colspan: 2, align: left)[Source: x\\_y]", fixed = TRUE)
  expect_match(typst, "1#super[\\+]", fixed = TRUE)
  expect_match(typst, "table.cell(colspan: 2, align: left)[#super[\\+]", fixed = TRUE)

  expect_match(to_md(ht), "Note: A & B\n\nSource: x_y", fixed = TRUE)
  expect_match(to_md(ht), "1[+]", fixed = TRUE)
  expect_match(to_md(ht), "[+] Shared_note", fixed = TRUE)
  rtf <- to_rtf(ht)
  expect_match(rtf, "{\\pard \\ql \\li2160\\ri2160 {Note: A & B} \\par}", fixed = TRUE)
  expect_match(rtf, "1{\\super +\\nosupersub}", fixed = TRUE)
  expect_match(to_screen(ht, color = FALSE), "Source: x_y", fixed = TRUE)
  expect_match(to_screen(ht, color = FALSE), "[+] Shared_note", fixed = TRUE)

  centered <- set_position(set_table_notes(hux(a = 1), "Left note"), "center")
  expect_match(to_screen(centered, color = FALSE), "\nLeft note\n", fixed = TRUE)
})


test_that("RTF note paragraphs match the table edges", {
  ht <- set_table_notes(hux(a = 1, add_colnames = FALSE), "Note")

  expect_match(to_rtf(ht), "\\li2160\\ri2160 {Note}", fixed = TRUE)
  expect_match(to_rtf(set_position(ht, "left")), "\\li0\\ri4320 {Note}", fixed = TRUE)
  expect_match(to_rtf(set_position(ht, "right")), "\\li4320\\ri0 {Note}", fixed = TRUE)
})


test_that("LaTeX suppresses orphan cell note references", {
  ht <- set_cell_note(hux(a = 1, add_colnames = FALSE), 1, 1, "Cell note")
  latex <- to_latex(ht, dependencies = FALSE)

  expect_match(latex, "1\\tnote{1}", fixed = TRUE)
  expect_match(latex, "\\item[1] Cell note", fixed = TRUE)
  expect_false(grepl("\\item[]", latex, fixed = TRUE))
  expect_false(grepl("\\tnote", to_latex(ht, tabular_only = TRUE), fixed = TRUE))
})


test_that("breakable LaTeX tables render notes once at the end", {
  ht <- set_cell_note(hux(a = 1:2), 2, 1, "Cell note")
  ht <- set_breakable(ht, TRUE)
  latex <- to_latex(ht, dependencies = FALSE)

  expect_match(latex, "\\begin{ThreePartTable}", fixed = TRUE)
  expect_match(latex, "\\begin{TableNotes}[flushleft]", fixed = TRUE)
  expect_match(latex, "\\insertTableNotes\n\\endlastfoot", fixed = TRUE)
  expect_match(latex, "\\item[1] Cell note", fixed = TRUE)
  expect_length(gregexpr("Cell note", latex, fixed = TRUE)[[1]], 1L)
  expect_false(grepl("\\item[]", latex, fixed = TRUE))
})


test_that("flextable uses its footer for table notes", {
  skip_if_not_installed("flextable")
  ht <- set_table_notes(hux(a = 1, b = 2), c("Note one", "Note two"))
  ht <- set_cell_note(ht, 2, 1, "Cell note")
  ft <- as_flextable(ht)

  expect_equal(nrow(ft$footer$dataset), 3L)
  expect_equal(ft$footer$dataset[[1]], c("Note one", "Note two", "[1] Cell note"))
  expect_true(all(ft$footer$styles$pars$text.align$data == "left"))
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
  ht <- set_cell_note(ht, 2, 1, "Cell note")
  path <- tempfile(fileext = ".xlsx")
  on.exit(unlink(path))

  wb <- as_Workbook(ht)
  openxlsx::saveWorkbook(wb, path)
  contents <- openxlsx::read.xlsx(
    path, colNames = FALSE, skipEmptyRows = FALSE
  )

  expect_equal(
    contents[[1]],
    c("a", "1[1]", "Note one", "Note two", "[1] Cell note", "Bottom caption")
  )
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
