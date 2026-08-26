local_edition(3)


test_that("breakable is a logical table property", {
  ht <- hux(a = 1:3, b = 4:6)
  expect_false(breakable(ht))
  expect_false(breakable(jams))

  breakable(ht) <- TRUE
  expect_true(breakable(ht))
  expect_true(breakable(ht[1:2, 1, drop = FALSE]))
  expect_true(breakable(unserialize(serialize(ht, NULL))))
  expect_true(breakable(rbind(ht, c(7, 8))))
  expect_true(breakable(cbind(ht, c = 7:10)))

  expect_false(breakable(set_breakable(ht, FALSE)))
  expect_error(breakable(ht) <- "yes")
})


test_that("breakable LaTeX tables use longtable", {
  ht <- hux(a = 1:3, b = 4:6)
  caption(ht) <- "A caption"
  label(ht) <- "tab:breakable"
  breakable(ht) <- TRUE

  tex <- to_latex(ht)
  expect_match(tex, "\\begin{longtable}[c]", fixed = TRUE)
  expect_match(tex, "\\setlength{\\LTcapwidth}{\\linewidth}", fixed = TRUE)
  expect_match(tex, "\\endfirsthead", fixed = TRUE)
  expect_match(tex, "\\endhead", fixed = TRUE)
  expect_match(tex, "\\caption{A caption}", fixed = TRUE)
  expect_match(tex, "\\label{tab:breakable}", fixed = TRUE)
  expect_false(grepl("\\begin{table}", tex, fixed = TRUE))
  expect_false(grepl("threeparttable", tex, fixed = TRUE))
  expect_false(grepl("centerbox", tex, fixed = TRUE))
  expect_false(grepl("resizebox", tex, fixed = TRUE))

  breakable(ht) <- FALSE
  expect_match(to_latex(ht), "\\begin{table}[ht]", fixed = TRUE)
})


test_that("breakable LaTeX tables repeat only leading header rows", {
  ht <- hux(matrix(1:8, 4, 2), add_colnames = FALSE)
  header_rows(ht) <- c(TRUE, TRUE, FALSE, TRUE)
  breakable(ht) <- TRUE
  tex <- to_latex(ht)
  first_head <- sub(".*?\\\\begin\\{longtable\\}.*?\\n", "", tex, perl = TRUE)
  first_head <- sub("\\\\endfirsthead.*", "", first_head)

  expect_match(first_head, "1", fixed = TRUE)
  expect_match(first_head, "2", fixed = TRUE)
  expect_false(grepl("3", first_head, fixed = TRUE))
  expect_false(grepl("4", first_head, fixed = TRUE))
})


test_that("breakable LaTeX captions respect position and width", {
  ht <- hux(a = 1:2)
  caption(ht) <- "Bottom caption"
  caption_pos(ht) <- "bottomright"
  caption_width(ht) <- 0.4
  width(ht) <- 0.6
  position(ht) <- "right"
  breakable(ht) <- TRUE

  tex <- to_latex(ht)
  expect_match(tex, "\\begin{longtable}[r]", fixed = TRUE)
  expect_match(tex, "\\setlength{\\LTcapwidth}{0.4\\textwidth}", fixed = TRUE)
  expect_match(tex, "justification=raggedleft", fixed = TRUE)
  expect_lt(regexpr("1", tex, fixed = TRUE), regexpr("\\caption{Bottom caption}", tex, fixed = TRUE))

  caption_width(ht) <- NA
  expect_match(
    to_latex(ht),
    "\\setlength{\\LTcapwidth}{0.6\\textwidth}",
    fixed = TRUE
  )
})


test_that("breakable LaTeX tables handle incompatible properties", {
  ht <- set_breakable(hux(a = 1:2), TRUE)

  height(ht) <- 0.5
  expect_error(to_latex(ht), "fixed height")
  height(ht) <- NA

  position(ht) <- "wrapleft"
  expect_error(to_latex(ht), "wrapping position")
  position(ht) <- "center"

  tabular_environment(ht) <- "tabularx"
  table_environment(ht) <- "table*"
  latex_float(ht) <- "b"
  expect_warning(
    tex <- to_latex(ht),
    "tabular_environment, table_environment, latex_float",
    fixed = TRUE
  )
  expect_match(tex, "\\begin{longtable}", fixed = TRUE)

  ht <- set_breakable(hux(a = 1:2), TRUE)
  tabular_environment(ht) <- "longtable"
  expect_warning(to_latex(ht), "tabular_environment", fixed = TRUE)
})


test_that("tabular_only returns an uncaptioned longtable", {
  ht <- set_breakable(set_caption(hux(a = 1:2), "Caption"), TRUE)
  tex <- to_latex(ht, tabular_only = TRUE)
  expect_match(tex, "\\begin{longtable}", fixed = TRUE)
  expect_false(grepl("\\caption", tex, fixed = TRUE))
})


test_that("Typst applies locally scoped break rules", {
  ht <- hux(a = 1:2)
  typst <- to_typst(ht)
  expect_false(grepl("#show figure", typst, fixed = TRUE))

  breakable(ht) <- TRUE
  typst <- to_typst(ht)
  expect_match(typst, "#show figure: set block(breakable: true)", fixed = TRUE)
  expect_match(
    typst,
    "#show table.cell: set table.cell(breakable: false)",
    fixed = TRUE
  )
})


test_that("HTML applies table and row break rules", {
  ht <- hux(a = 1:2)
  html <- to_html(ht)
  expect_match(html, "break-inside: avoid; page-break-inside: avoid;", fixed = TRUE)
  expect_match(html, '<tr style="break-inside: avoid;', fixed = TRUE)

  breakable(ht) <- TRUE
  html <- to_html(ht)
  expect_match(html, "break-inside: auto; page-break-inside: auto;", fixed = TRUE)
  expect_match(html, '<tr style="break-inside: avoid;', fixed = TRUE)
})


test_that("RTF keeps rows intact and conditionally keeps them together", {
  ht <- hux(a = 1:2, add_colnames = FALSE)
  rtf <- to_rtf(ht)
  expect_length(gregexpr("\\trkeep ", rtf, fixed = TRUE)[[1]], 2L)
  expect_length(gregexpr("\\trkeepfollow ", rtf, fixed = TRUE)[[1]], 1L)

  breakable(ht) <- TRUE
  rtf <- to_rtf(ht)
  expect_length(gregexpr("\\trkeep ", rtf, fixed = TRUE)[[1]], 2L)
  expect_false(grepl("\\trkeepfollow", rtf, fixed = TRUE))
})


test_that("flextable maps breakable to Word pagination", {
  skip_if_not_installed("flextable", minimum_version = "0.9.1")
  ht <- hux(a = 1:2, b = 3:4)

  ft <- as_flextable(ht)
  expect_false(ft$properties$opts_word$split)
  expect_true(all(ft$body$styles$pars$keep_with_next$data))

  breakable(ht) <- TRUE
  ft <- as_flextable(ht)
  expect_false(ft$properties$opts_word$split)
  expect_false(any(ft$body$styles$pars$keep_with_next$data))
})


test_that("breakable LaTeX tables compile across pages", {
  skip_on_cran()
  required_tools <- Sys.which(c("pdflatex", "pdfinfo", "pdftotext"))
  skip_if(any(required_tools == ""), "TeX and Poppler command-line tools are required")

  top <- hux(
    c("Heading", sprintf("top row %03d", 1:120)),
    c("Value", 1:120),
    add_colnames = FALSE
  )
  header_rows(top)[1] <- TRUE
  breakable(top) <- TRUE
  caption(top) <- "Top caption"
  label(top) <- "tab:breakable-top"

  bottom <- hux(
    c("Heading", sprintf("bottom row %03d", 1:120)),
    c("Value", 1:120),
    add_colnames = FALSE
  )
  header_rows(bottom)[1] <- TRUE
  breakable(bottom) <- TRUE
  caption(bottom) <- "Bottom caption"
  caption_pos(bottom) <- "bottom"
  label(bottom) <- "tab:breakable-bottom"

  tex_dir <- tempfile("breakable-latex-")
  dir.create(tex_dir)
  old_wd <- setwd(tex_dir)
  on.exit(setwd(old_wd), add = TRUE)
  tex <- paste0(
    "\\documentclass{article}\n",
    report_latex_dependencies(quiet = TRUE, as_string = TRUE),
    "\\begin{document}\n",
    "References: \\ref{tab:breakable-top} and \\ref{tab:breakable-bottom}.\n",
    to_latex(top), "\n", to_latex(bottom),
    "\n\\end{document}\n"
  )
  writeLines(tex, "breakable.tex")

  latex_args <- c("-interaction=nonstopmode", "-halt-on-error", "breakable.tex")
  first_run <- system2(required_tools[["pdflatex"]], latex_args, stdout = TRUE, stderr = TRUE)
  second_run <- system2(required_tools[["pdflatex"]], latex_args, stdout = TRUE, stderr = TRUE)
  expect_null(attr(first_run, "status"), info = paste(first_run, collapse = "\n"))
  expect_null(attr(second_run, "status"), info = paste(second_run, collapse = "\n"))
  expect_false(any(grepl("Float too large", second_run, fixed = TRUE)))
  expect_false(any(grepl("undefined references", second_run, ignore.case = TRUE)))

  pdf_info <- system2(required_tools[["pdfinfo"]], "breakable.pdf", stdout = TRUE)
  page_count <- as.integer(sub("Pages:[[:space:]]+", "", grep("^Pages:", pdf_info, value = TRUE)))
  expect_gt(page_count, 1L)

  expect_equal(
    system2(required_tools[["pdftotext"]], c("-layout", "breakable.pdf", "breakable.txt")),
    0L
  )
  pdf_text <- paste(readLines("breakable.txt", warn = FALSE), collapse = "\n")
  pdf_pages <- strsplit(pdf_text, "\f", fixed = TRUE)[[1]]
  nonempty_pages <- which(nzchar(trimws(pdf_pages)))
  expect_equal(which(grepl("Top caption", pdf_pages, fixed = TRUE)), 1L)
  expect_equal(which(grepl("Bottom caption", pdf_pages, fixed = TRUE)), max(nonempty_pages))
  expect_lt(regexpr("Top caption", pdf_text, fixed = TRUE), regexpr("top row 001", pdf_text, fixed = TRUE))
  expect_lt(regexpr("bottom row 120", pdf_text, fixed = TRUE), regexpr("Bottom caption", pdf_text, fixed = TRUE))
  expect_gte(length(gregexpr("Heading", pdf_text, fixed = TRUE)[[1]]), page_count)
  expect_false(grepl("??", pdf_text, fixed = TRUE))
})


test_that("breakable Typst tables compile across pages", {
  skip_on_cran()
  required_tools <- Sys.which(c("typst", "pdfinfo", "pdftotext"))
  skip_if(any(required_tools == ""), "Typst and Poppler command-line tools are required")

  ht <- hux(
    c("Heading", sprintf("row %03d", 1:120)),
    c("Value", 1:120),
    add_colnames = FALSE
  )
  header_rows(ht)[1] <- TRUE
  breakable(ht) <- TRUE
  caption(ht) <- "Typst caption"
  label(ht) <- "tab:breakable-typst"

  typ_dir <- tempfile("breakable-typst-")
  dir.create(typ_dir)
  typ_file <- file.path(typ_dir, "breakable.typ")
  pdf_file <- file.path(typ_dir, "breakable.pdf")
  text_file <- file.path(typ_dir, "breakable.txt")
  writeLines(c(to_typst(ht), "Reference: @tab:breakable-typst"), typ_file)

  typst_run <- system2(
    required_tools[["typst"]],
    c("compile", typ_file, pdf_file),
    stdout = TRUE,
    stderr = TRUE
  )
  expect_null(attr(typst_run, "status"), info = paste(typst_run, collapse = "\n"))

  pdf_info <- system2(required_tools[["pdfinfo"]], pdf_file, stdout = TRUE)
  page_count <- as.integer(sub("Pages:[[:space:]]+", "", grep("^Pages:", pdf_info, value = TRUE)))
  expect_gt(page_count, 1L)
  expect_equal(system2(required_tools[["pdftotext"]], c("-layout", pdf_file, text_file)), 0L)
  pdf_text <- paste(readLines(text_file, warn = FALSE), collapse = "\n")
  expect_gte(length(gregexpr("Heading", pdf_text, fixed = TRUE)[[1]]), page_count)
  expect_false(grepl("??", pdf_text, fixed = TRUE))
})
