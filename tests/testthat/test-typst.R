local_edition(2)

test_that("to_typst basic table structure", {
  ht <- hux(a = 1:2, b = 3:4, add_colnames = FALSE)
  valign(ht) <- NA
  res <- to_typst(ht)
  expect_match(res, "#figure(", fixed = TRUE)
  expect_match(res, "table(", fixed = TRUE)
  expect_match(res, "columns: (auto, auto)", fixed = TRUE)
  expect_equal(length(gregexpr("table.cell", res, fixed = TRUE)[[1]]), 4L)
  expect_match(res, "caption: none", fixed = TRUE)
})


test_that("header rows rendered separately", {
  ht <- hux(a = 1:2, b = 3:4, add_colnames = FALSE)
  header_rows(ht) <- c(TRUE, FALSE)
  res <- to_typst(ht)
  expect_match(res, "table.header(", fixed = TRUE)
  expect_match(res, "table.cell(align: (right + top))[1]", fixed = TRUE)
  expect_match(res, "table.cell(align: (right + top))[3]", fixed = TRUE)
})

test_that("to_typst handles vertical alignment", {
  ht <- hux(a = 1:2, b = 3:4, add_colnames = FALSE)
  valign(ht) <- matrix(c("middle", "top", "top", "bottom"), 2, 2)
  res <- to_typst(ht)
  expect_match(res, "table.cell\\(align: \\(right \\+ horizon\\)\\)\\[1\\]")
  expect_match(res, "table.cell\\(align: \\(right \\+ bottom\\)\\)\\[4\\]")
})

test_that("to_typst maps properties", {
  ht <- hux(a = 1:3, b = 4:6, c = 7:9, add_colnames = FALSE)
  valign(ht) <- NA
  caption(ht) <- "A cap"
  col_width(ht) <- c(.2, .3, .5)
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
  expect_match(res, "#figure(", fixed = TRUE)
  expect_match(res, "caption: \\[A cap\\]")
  expect_match(res, "columns: \\(0.2fr, 0.3fr, 0.5fr\\)")
  expect_match(res, "colspan: 2")
  expect_match(res, "rowspan: 2")
  expect_match(res, "align: (right + top)", fixed = TRUE)
  expect_match(res, "fill: rgb")
  expect_match(
    res,
    "stroke: \\(top: stroke\\(thickness: 1pt, paint: rgb\\(0, 0, 255\\)\\)\\)"
  )
  expect_match(
    res,
    "text\\(weight: \"bold\", style: \"italic\", size: 12pt, family: \"Courier\", fill: rgb\\(0, 0, 255\\)\\)\\[1\\]"
  )
  expect_match(res, "inset: \\(top: 3pt, right: 2pt, bottom: 4pt, left: 1pt\\)")
})

test_that("Bugfix: solid border generates valid typst", {
  ht <- hux(a = 1:2, b = 3:4)
  ht <- set_top_border(ht, 1, 1)
  res <- to_typst(ht)
  expect_match(
    res,
    "stroke: \\(top: stroke\\(thickness: 0.4pt, paint: rgb\\(0, 0, 0\\)\\)\\)"
  )
})

test_that("Bugfix: merge_cells in Typst output produces no empty rows", {
  ht <- merge_cells(jams, 1:2, 1:2)
  typ <- to_typst(ht)
  lines <- strsplit(typ, "\n")[[1]]
  expect_false(any(trimws(lines) == ","))
})

test_that("Bugfix: typst tables without borders have no stroke", {
  ht <- hux(a = 1:2, b = 3:4, add_colnames = FALSE)
  valign(ht) <- NA
  res <- to_typst(ht)
  expect_match(res, "stroke: none", fixed = TRUE)
  expect_false(grepl("stroke\\(", res))
})

test_that("Bugfix: caption escapes special characters", {
  ht <- hux(a = 1:2, b = 3:4)
  caption(ht) <- "#notfun"
  res <- to_typst(ht)
  expect_match(res, "caption: \\[\\\\#notfun\\]")
})

test_that("print_typst outputs to stdout", {
  ht <- hux(a = 1)
  expect_output(print_typst(ht), "#figure", fixed = TRUE)
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
