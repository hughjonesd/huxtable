local_edition(3)


test_that("to_screen gives warning with colour if crayon not installed", {
  ht <- hux(a = 1:2)
  with_mocked_bindings(
    {
      expect_warning(to_screen(ht, color = TRUE), "crayon")
    },
    requireNamespace = function(...) FALSE
  )
})


test_that("to_md and to_screen keep to max_width", {
  ht <- hux(a = paste(sample(LETTERS), collapse = "..."), b = 1:2)

  for (mw in 2:12 * 10) {
    for (func in list(to_md, to_screen)) {
      output <- func(ht, max_width = mw)
      lines <- strsplit(output, "\n", fixed = TRUE)[[1]]
      expect_true(all(nchar(lines, type = "width") <= mw))
    }
  }

  caption(ht) <- "a very very very long caption"
  output <- to_screen(ht, max_width = 18)
  lines <- strsplit(output, "\n", fixed = TRUE)[[1]]
  expect_true(all(nchar(lines, type = "width") <= 18))
  # we don't test captions for to_md because I don't think markdown can handle multiline captions
})


test_that("to_md and to_screen keep to min_width", {
  ht <- hux(a = "foo", b = "bar")
  for (mw in c(2, 10, 20)) {
    for (func in list(to_md, to_screen)) {
      output <- func(ht, min_width = mw)
      lines <- strsplit(output, "\n", fixed = TRUE)[[1]]
      lines <- lines[nchar(lines) > 0] # empty lines after table itself
      lines <- lines[!grepl("Column names", lines)]
      expect_true(all(nchar(lines, type = "width") >= mw))
    }
  }
})


test_that("to_md warns on unimplemented features", {
  ht <- hux(a = 1:2, b = 1:2)
  colspan(ht)[1, 1] <- 2
  expect_warning(to_md(ht), "colspan")
  colspan(ht)[1, 1] <- 1
  rowspan(ht)[1, 1] <- 2
  expect_warning(to_md(ht), "rowspan")
  rowspan(ht)[1, 1] <- 1
  align(ht)[1, ] <- c("left", "right")
  expect_warning(to_md(ht), "align")
})


test_that("to_md prints bold and italic", {
  short_strings <- c("bold", "both", "italic")
  long_strings <- strrep(toupper(short_strings), 40)
  ht <- hux(a = short_strings, b = long_strings, add_colnames = FALSE)
  bold(ht)[1:2, 1:2] <- TRUE
  italic(ht)[2:3, 1:2] <- TRUE
  expect_silent(res <- to_md(ht))
  expect_match(res, regexp = "\\*italic\\*")
  expect_match(res, regexp = "\\*\\*bold\\*\\*")
  expect_match(res, regexp = "\\*\\*\\*both\\*\\*\\*")

  res <- strsplit(res, "\\n")[[1]]

  # any strings with ITALIC should have * at start and end
  italic <- stringr::str_match(res, "(.)[ITALIC]{0,6}(ITALIC)+[ITALIC]{0,6}(.)")
  italic <- stats::na.omit(italic)
  expect_true(all(italic[, 2] == "*"))
  expect_true(all(italic[, 4] == "*"))

  # any strings with BOTH should have *** at start and end
  both <- stringr::str_match(res, "(...)[BOTH]{0,4}(BOTH)+[BOTH]{0,4}(...)")
  both <- stats::na.omit(both)
  expect_true(all(both[, 2] == "***"))
  expect_true(all(both[, 4] == "***"))

  # any strings with BOLD should have ** at start and end
  bold <- stringr::str_match(res, "(..)[BOLD]{0,4}(BOLD)+[BOLD]{0,4}(..)")
  bold <- stats::na.omit(bold)
  expect_true(all(bold[, 2] == "**"))
  expect_true(all(bold[, 4] == "**"))
})


test_that("to_screen borders respect spans", {
  ht <- hux(a = 1:2, b = 3:4, add_colnames = FALSE)
  ht <- set_all_borders(ht)
  ht2 <- ht

  colspan(ht)[1, 1] <- 2
  # a line with just: some spaces, │, 1, some spaces, │, some spaces
  # NB that this character: │ is NOT the "or" character, so don't try to type it
  expect_match(to_screen(ht),
    "\\n\\s*│\\s*1\\s*│\\s*\\n",
    perl = TRUE
  )

  rowspan(ht2)[1, 1] <- 2
  # a line after "3" with some spaces, a │ and some more spaces
  expect_match(to_screen(ht2), "3.*?\\n\\s*│\\s*", perl = TRUE)
})


test_that("to_screen shows border styles", {
  h <- hux("a")
  h <- set_all_borders(h)
  h1 <- set_all_border_styles(h, "double")
  expect_match(to_screen(h1), "╔", fixed = TRUE)
  h2 <- set_all_border_styles(h, "dashed")
  expect_match(to_screen(h2), "┄", fixed = TRUE)
  h3 <- set_all_border_styles(h, "dotted")
  expect_match(to_screen(h3), "┈", fixed = TRUE)
})


test_that("to_screen positioning", {
  ht <- hux("foo")
  position(ht) <- "centre"
  to_s_res <- to_screen(ht, max_width = 80)
  expect_match(to_s_res, "\\s{30,}foo", perl = TRUE)

  position(ht) <- "right"
  to_s_res <- to_screen(ht, max_width = 80)
  expect_match(to_s_res, "\\s{60,}.*foo", perl = TRUE)

  ht <- set_bold(ht)
  to_s_res <- to_screen(ht, max_width = 80)
  # the stars are for the control characters making foo bold
  expect_match(to_s_res, "\\s{60,}.*f.*o.*o", perl = TRUE)
})


test_that("hux_logo works", {
  # there"s randomization, so:
  for (i in 1:100) expect_silent(hux_logo())
  expect_silent(hux_logo(latex = TRUE))
})


test_that("Multi-rowspan screen output is sane", {
  ht <- hux(
    a = rep("aaaaaa", 10), b = rep("bbbbbb", 10),
    add_colnames = TRUE
  )
  rowspan(ht)[1, 1] <- 10
  expect_snapshot_value(to_screen(ht), style = "serialize")
})


test_that("to_screen does not cut off multicols", {
  ht <- hux(a = 1:2, b = 1:2)
  ht[2, 1] <- "some very long long text"
  colspan(ht)[2, 1] <- 2
  expect_match(to_screen(ht), "some very long long text", fixed = TRUE)
})


test_that("to_screen alignment not messed up by markdown", {
  skip_if_not_installed("crayon")

  jams_md <- jams
  jams_md <- set_markdown_contents(jams_md, 1, 2, "**Price**")
  jams_output <- to_screen(jams)
  jams_md_output <- to_screen(jams_md)
  jams_output <- crayon::strip_style(jams_output)
  jams_md_output <- crayon::strip_style(jams_md_output)
  spaces <- stringr::str_match(jams_output, "Type(.*?)Price")[1, 2]
  spaces_md <- stringr::str_match(jams_md_output, "Type(.*?)Price")[1, 2]
  expect_identical(spaces, spaces_md)
})


test_that("output works with zero-dimension huxtables", {
  h_nrow0 <- hux(a = character(0), b = character(0), add_colnames = FALSE)
  expect_silent(to_screen(h_nrow0))
  expect_warning(to_md(h_nrow0), "row")
  expect_warning(to_html(h_nrow0), "row")
  expect_warning(to_latex(h_nrow0), "row")

  h_ncol0 <- hux(a = 1:2)[, FALSE]
  expect_silent(to_screen(h_ncol0))
  expect_warning(to_md(h_ncol0), "col")
  expect_warning(to_html(h_ncol0), "col")
  expect_warning(to_latex(h_ncol0), "col")
})


test_that("output works with 1x1 huxtables", {
  h_1x1 <- hux(a = 1, add_colnames = FALSE)

  expect_silent(to_screen(h_1x1))
  expect_silent(to_md(h_1x1))
  expect_silent(to_html(h_1x1))
  expect_silent(to_latex(h_1x1))
})


test_that("Bugfix: print_html doesn't duplicate rows", {
  html <- to_html(hux(letters[1:2]))
  tbody_count <- length(regmatches(html, gregexpr("<tbody>", html, fixed = TRUE))[[1]])
  td_count <- length(regmatches(html, gregexpr("<td", html, fixed = TRUE))[[1]])
  expect_identical(tbody_count, 1L)
  expect_identical(td_count, 2L)
})


test_that("format.huxtable works", {
  ht <- hux(a = 1:3, b = 1:3)
  for (output in c("latex", "html", "md", "screen", "typst")) {
    direct_call <- paste0("to_", output)
    expect_identical(do.call(direct_call, list(ht)), format(ht, output = output))
  }
})


test_that("set_print_method() works", {
  ht <- hux(a = 1:2, b = 1:2)
  oo <- options(huxtable.print = print_html)
  expect_match(capture.output(print(ht)), "<table", fixed = TRUE, all = FALSE)
  options(huxtable.print = print_latex)
  expect_match(capture.output(print(ht)), "tabular", fixed = TRUE, all = FALSE)
  options(huxtable.print = print_typst)
  expect_match(capture.output(print(ht)), "#figure", fixed = TRUE, all = FALSE)
  options(huxtable.print = "print_html")
  expect_match(capture.output(print(ht)), "<table", fixed = TRUE, all = FALSE)
  options(huxtable.print = "print_typst")
  expect_match(capture.output(print(ht)), "#figure", fixed = TRUE, all = FALSE)
  options(oo)
})


test_that("HTML gives warnings for double borders not wide enough", {
  ht <- hux(a = 1)
  top_border(ht) <- 2
  top_border_style(ht) <- "double"
  expect_warning(to_html(ht), "double")
  top_border(ht) <- 3
  expect_silent(to_html(ht))
})


test_that("Markdown in rtf", {
  ht <- hux("Some *italic*, **bold**, ~strikethrough~, [a link](https://google.com)")
  expect_silent(to_rtf(ht))
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


test_that("LaTeX uses a colorbox for the table background", {
  ht <- set_table_background_color(hux(a = 1), "red")
  tex <- to_latex(ht)
  expect_match(tex, "\\setlength{\\fboxsep}{0pt}", fixed = TRUE)
  expect_match(tex, "\\colorbox[RGB]{255, 0, 0}{\\begin{tabular}", fixed = TRUE)
  expect_false(grepl("\\cellcolor[RGB]{255, 0, 0}", tex, fixed = TRUE))

  breakable(ht) <- TRUE
  tex <- to_latex(ht)
  expect_false(grepl("\\colorbox", tex, fixed = TRUE))
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
  expect_silent(tex <- to_latex(ht))
  expect_match(tex, "\\begin{longtable}", fixed = TRUE)
})


test_that("tabular_only returns an uncaptioned longtable", {
  ht <- set_breakable(set_caption(hux(a = 1:2), "Caption"), TRUE)
  tex <- to_latex(ht, tabular_only = TRUE)
  expect_match(tex, "\\begin{longtable}", fixed = TRUE)
  expect_false(grepl("\\caption", tex, fixed = TRUE))
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


test_that("RTF uses table background for otherwise unfilled cells", {
  ht <- set_table_background_color(hux(a = 1:2, add_colnames = FALSE), "red")
  background_color(ht)[1, 1] <- "blue"
  rtf <- to_rtf(ht)
  expect_match(rtf, "\\clcbpat1", fixed = TRUE)
  expect_match(rtf, "\\clcbpat2", fixed = TRUE)
  expect_equal(as.character(rtf_fc_tables(ht)$colors), c("blue", "red"))
})


test_that("Screen output colors the whole table background", {
  skip_if_not_installed("crayon")
  local_reproducible_output(crayon = TRUE)
  ht <- set_table_background_color(
    set_all_borders(hux(a = 1, add_colnames = FALSE)),
    "red"
  )
  background_color(ht)[1, 1] <- "blue"
  screen <- to_screen(ht, color = TRUE)
  expect_match(screen, "\033[41m─", fixed = TRUE)
  expect_match(screen, "\033[44m1", fixed = TRUE)
})


test_that("Chinese characters are not repeated", {
  skip_on_appveyor()
  skip_on_os("windows")
  skip_on_cran()
  ht <- hux("\u4e2d\u6587")
  expect_silent(s <- to_screen(ht))
  expect_match(s, "^[[:space:]]*\u4e2d\u6587[[:space:]]*(\\n.*)?$", all = FALSE)
})


test_that("Bugfix: print_screen with colnames = FALSE prints final newline", {
  h <- hux(col = 1)
  s <- to_screen(h, colnames = FALSE)
  expect_match(s, "\\n$")
})


test_that("Bugfix: wide characters lead to infinite loop in to_screen", {
  w <- options(width = 100)
  on.exit({
    setTimeLimit() # resets time limit
    options(w)
  })
  chars <- sapply(32:5000, intToUtf8)
  wide_chars <- chars[sapply(chars, stringi::stri_width) == 2]
  wide_strings <- rep(paste(wide_chars[101:120], collapse = ""), 5)
  df <- as.data.frame(as.list(wide_strings), col.names = paste0("V", 1:5))
  ht <- as_huxtable(df)
  setTimeLimit(elapsed = 5, transient = TRUE)
  expect_silent(to_screen(ht))
})
