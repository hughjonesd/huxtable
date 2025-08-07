local_edition(2)

test_that("to_typst basic table structure", {
  ht <- hux(a = 1:2, b = 3:4, add_colnames = FALSE)
  res <- to_typst(ht)
  expected <- paste0(
    "#table(\n",
    "  columns: (auto, auto)\n",
    ")[\n",
    "  cell(align: (right, top))[1] cell(align: (right, top))[3]\n",
    "  cell(align: (right, top))[2] cell(align: (right, top))[4]\n",
    "]\n"
  )
  expect_identical(res, expected)
})

test_that("header rows rendered separately", {
  ht <- hux(a = 1:2, b = 3:4, add_colnames = FALSE)
  header_rows(ht) <- c(TRUE, FALSE)
  res <- to_typst(ht)
  expected <- paste0(
    "#table(\n",
    "  columns: (auto, auto)\n",
    ")[\n",
    "  table.header[\n",
    "    cell(align: right)[1] cell(align: right)[3]\n",
    "  ]\n",
    "  cell(align: right)[2] cell(align: right)[4]\n",
    "]\n"
  )
  expect_identical(res, expected)
})

test_that("to_typst maps properties", {
  ht <- hux(a = 1:3, b = 4:6, c = 7:9, add_colnames = FALSE)
  caption(ht) <- "A cap"
  col_width(ht) <- c(.2, .3, .5)
  width(ht) <- 0.5
  height(ht) <- 0.25
  align(ht)[1, 3] <- "right"
  colspan(ht)[1, 1] <- 2
  rowspan(ht)[2, 3] <- 2
  background_color(ht)[1, 1] <- "red"
  top_border(ht)[1, 1] <- brdr(1, "solid", "blue")
  bold(ht)[1, 1] <- TRUE
  italic(ht)[1, 1] <- TRUE
  font_size(ht)[1, 1] <- 12
  font(ht)[1, 1] <- "Courier"

  res <- to_typst(ht)

  expect_match(res, "caption: \\[A cap\\]")
  expect_match(res, "columns: \\(0.2fr, 0.3fr, 0.5fr\\)")
  expect_match(res, "width: 50\\.000%")
  expect_match(res, "height: 25\\.000%")
  expect_match(res, "colspan: 2")
  expect_match(res, "rowspan: 2")
  expect_match(res, "align: (right, top)", fixed = TRUE)
  expect_match(res, "fill: rgb")
  expect_match(res, "stroke: \\(top: 1pt \\+ solid \\+ rgb")
  expect_match(res, "text\\(weight: \"bold\", style: \"italic\", size: 12pt, family: \"Courier\"\\)\\[1\\]")
})

test_that("to_typst handles vertical alignment", {
  ht <- hux(a = 1:2, b = 3:4, add_colnames = FALSE)
  valign(ht)[1, 1] <- "middle"
  valign(ht)[2, 2] <- "bottom"

  res <- to_typst(ht)

  expect_match(res, "cell(align: (right, center))[1]", fixed = TRUE)
  expect_match(res, "cell(align: (right, bottom))[4]", fixed = TRUE)
})

test_that("print_typst outputs to stdout", {
  ht <- hux(a = 1)
  expect_output(print_typst(ht), trimws(to_typst(ht), which = "right"), fixed = TRUE)
})
