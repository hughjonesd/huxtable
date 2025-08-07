local_edition(2)

test_that("to_typst basic table structure", {
  ht <- hux(a = 1:2, b = 3:4, add_colnames = FALSE)
  valign(ht) <- NA
  res <- to_typst(ht)
  expected <- paste0(
    "#table(\n",
    "  columns: (auto, auto)\n",
    ")[\n",
    "  cell(align: (right + top))[1] cell(align: (right + top))[3]\n",
    "  cell(align: (right + top))[2] cell(align: (right + top))[4]\n",
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
    "    cell(align: (right + top))[1] cell(align: (right + top))[3]\n",
    "  ]\n",
    "  cell(align: (right + top))[2] cell(align: (right + top))[4]\n",
    "]\n"
  )
  expect_identical(res, expected)
})

test_that("to_typst handles vertical alignment", {
  ht <- hux(a = 1:2, b = 3:4, add_colnames = FALSE)
  valign(ht) <- matrix(c("middle", "top", "top", "bottom"), 2, 2)
  res <- to_typst(ht)
  expect_match(res, "cell\\(align: \\(right, center\\), inset: 6pt\\)\\[1\\]")
  expect_match(res, "cell\\(align: \\(right, bottom\\), inset: 6pt\\)\\[4\\]")
})

test_that("to_typst maps properties", {
  ht <- hux(a = 1:3, b = 4:6, c = 7:9, add_colnames = FALSE)
  valign(ht) <- NA
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
  text_color(ht)[1, 1] <- "blue"

  left_padding(ht)[1, 3] <- 1
  right_padding(ht)[1, 3] <- 2
  top_padding(ht)[1, 3] <- 3
  bottom_padding(ht)[1, 3] <- 4

  res <- to_typst(ht)
  expect_match(res, "figure: (", fixed = TRUE)
  expect_match(res, "caption: \\[A cap\\]")
  expect_match(res, "columns: \\(0.2fr, 0.3fr, 0.5fr\\)")
  expect_match(res, "width: 50\\.000%")
  expect_match(res, "height: 25\\.000%")
  expect_match(res, "colspan: 2")
  expect_match(res, "rowspan: 2")
  expect_match(res, "align: (right + top)", fixed = TRUE)
  expect_match(res, "fill: rgb")
  expect_match(res, "stroke: \\(top: 1pt \\+ solid \\+ rgb")
  expect_match(res, "text\\(weight: \"bold\", style: \"italic\", size: 12pt, family: \"Courier\", fill: rgb")
})

test_that("to_typst handles vertical alignment", {
  ht <- hux(a = 1:2, b = 3:4, add_colnames = FALSE)
  valign(ht)[1, 1] <- "middle"
  valign(ht)[2, 2] <- "bottom"

  res <- to_typst(ht)

  expect_match(res, "cell(align: (right + horizon))[1]", fixed = TRUE)
  expect_match(res, "cell(align: (right + bottom))[4]", fixed = TRUE)
})

test_that("print_typst outputs to stdout", {
  ht <- hux(a = 1)
  expect_output(print_typst(ht), trimws(to_typst(ht), which = "right"), fixed = TRUE)
})

test_that("to_typst handles caption position", {
  ht <- hux(a = 1:2, b = 3:4, add_colnames = FALSE)
  caption(ht) <- "cap"
  caption_pos(ht) <- "top"
  expect_match(to_typst(ht), "position: top")
  caption_pos(ht) <- "bottom"
  expect_match(to_typst(ht), "position: bottom")
})

test_that("to_typst handles table alignment", {
  ht <- hux(a = 1:2, b = 3:4, add_colnames = FALSE)
  position(ht) <- "left"
  expect_match(to_typst(ht), "align: left")
  position(ht) <- "right"
  expect_match(to_typst(ht), "align: right")
})

test_that("to_typst respects wrap", {
  long <- strrep("a", 100)
  ht <- hux(a = long, add_colnames = FALSE)
  res_wrap <- to_typst(ht)
  expect_false(grepl("box\\(breakable: false\\)", res_wrap))

  wrap(ht) <- FALSE
  res_nowrap <- to_typst(ht)
  expect_match(res_nowrap, paste0("box\\(breakable: false\\)\\[", long, "\\]"))
})
