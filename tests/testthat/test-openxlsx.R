skip_if_not_installed("openxlsx")


test_that("Simple conversion works", {
  hx <- huxtable(a = 1:3, b = 4:6)
  expect_silent(wb <- as_Workbook(hx))
  expect_s4_class(wb, "Workbook")
})


test_that("Text properties work", {
  hx <- huxtable(a = 1:3, b = 4:6)
  font(hx)[1, 1] <- "Times New Roman"
  font_size(hx)[1, 2] <- 14
  bold(hx)[2, 1] <- TRUE
  italic(hx)[2, 2] <- TRUE
  text_color(hx)[3, 1] <- "red"
  number_format(hx)[3, 2] <- "%3.2f"
  expect_silent(wb <- as_Workbook(hx))
})


test_that("Table background fills otherwise unfilled Excel cells", {
  hx <- set_table_background_color(
    huxtable(a = 1:2, b = 3:4, add_colnames = FALSE),
    "red"
  )
  background_color(hx)[1, 1] <- "blue"
  wb <- as_Workbook(hx)

  xlsx <- tempfile(fileext = ".xlsx")
  on.exit(unlink(xlsx))
  openxlsx::saveWorkbook(wb, xlsx, overwrite = TRUE)

  styles <- xml2::read_xml(unz(xlsx, "xl/styles.xml"))
  styles_ns <- xml2::xml_ns(styles)
  fills <- xml2::xml_find_all(styles, ".//d1:fills/d1:fill", styles_ns)
  fill_colors <- vapply(fills, function(fill) {
    fg_color <- xml2::xml_find_first(fill, ".//d1:fgColor", styles_ns)
    xml2::xml_attr(fg_color, "rgb")
  }, character(1))
  cell_xfs <- xml2::xml_find_all(styles, ".//d1:cellXfs/d1:xf", styles_ns)
  fill_ids <- as.integer(xml2::xml_attr(cell_xfs, "fillId")) + 1L

  sheet <- xml2::read_xml(unz(xlsx, "xl/worksheets/sheet1.xml"))
  sheet_ns <- xml2::xml_ns(sheet)
  cells <- xml2::xml_find_all(sheet, ".//d1:sheetData/d1:row/d1:c", sheet_ns)
  cell_refs <- xml2::xml_attr(cells, "r")
  style_ids <- as.integer(xml2::xml_attr(cells, "s")) + 1L
  cell_colors <- setNames(fill_colors[fill_ids[style_ids]], cell_refs)

  expect_equal(cell_colors["A1"], c(A1 = "FF0000FF"))
  expect_equal(unname(cell_colors[c("B1", "A2", "B2")]), rep("FFFF0000", 3))
})


test_that("Borders work", {
  hx <- huxtable(a = 1:3, b = 4:6)
  top_border(hx)[1, ] <- 1
  bottom_border(hx)[1, ] <- 2
  left_border(hx)[, 1] <- 1
  right_border(hx)[, 2] <- 1
  expect_silent(as_Workbook(hx))
})


test_that("Widths and alignment work", {
  hx <- huxtable(a = 1:3, b = 4:6, add_colnames = FALSE)
  col_width(hx) <- c(.7, .3)
  width(hx) <- .9
  expect_silent(as_Workbook(hx))
  row_height(hx) <- c(.2, .6, .2)
  height(hx) <- .8
  expect_silent(as_Workbook(hx))
  align(hx)[1, 1] <- "right"
  valign(hx)[1, 2] <- "middle"
  align(hx)[2, 1] <- "centre"
  expect_silent(as_Workbook(hx))
})


test_that("Non-numeric colwidths and widths do not fail", {
  hx <- huxtable(a = 1:3, b = 4:6)
  col_width(hx) <- c(.7, .3)
  width(hx) <- "200px"
  expect_silent(as_Workbook(hx))
  col_width(hx) <- c("50px", "150px")
  expect_silent(as_Workbook(hx))
})


test_that("Captions work", {
  hx <- huxtable(a = 1:3, b = 4:6)
  caption(hx) <- "Caption here"
  for (pos in c("top", "topleft", "topcentre", "topright", "bottom", "bottomleft", "bottomcentre", "bottomright")) {
    caption_pos(hx) <- pos
    expect_silent(as_Workbook(hx))
  }
})


test_that("Can add to an existing workbook", {
  hx <- huxtable(a = 1:3, b = 4:6)
  wb <- openxlsx::createWorkbook()
  expect_silent(wb <- as_Workbook(hx, Workbook = wb))
  expect_silent(as_Workbook(hx, Workbook = wb, sheet = "Another sheet"))
})


test_that("Can add to an existing worksheet", {
  hx <- huxtable(a = 1:3, b = 4:6)
  wb <- openxlsx::createWorkbook()
  wb <- as_Workbook(hx, Workbook = wb, sheet = "sheet1")
  expect_silent(as_Workbook(hx, Workbook = wb, sheet = "sheet1", start_row = 6))
})


test_that("Can write with offset rows and columns", {
  hx <- huxtable(a = 1:3, b = 4:6)
  expect_silent(wb <- as_Workbook(hx, start_col = 3, start_row = 2))
})


test_that("Works for single-column huxtables with and without row names", {
  hx <- huxtable(1, 2, 3)
  wb <- openxlsx::createWorkbook()
  expect_silent(wb <- as_Workbook(hx, Workbook = wb))
  hx <- huxtable(a = 1, b = 2, c = 3)
  wb <- openxlsx::createWorkbook()
  expect_silent(wb <- as_Workbook(hx, Workbook = wb))
})


test_that("Works for zero-dimension huxtables", {
  h_nrow0 <- hux(a = character(0), b = character(0), add_colnames = FALSE)
  h_ncol0 <- hux(a = 1:2)[, FALSE]

  expect_silent(as_Workbook(h_nrow0))
  expect_silent(as_Workbook(h_ncol0))
})


test_that("Data written in appropriate format", {
  hx <- huxtable(a = 1:2 + 0.5, b = -1:-2 + 0.5, d = letters[1:2], add_colnames = TRUE)
  wb <- as_Workbook(hx)
  expect_error(openxlsx::saveWorkbook(wb, file = "test-xlsx.xlsx", overwrite = TRUE),
    regexp = NA
  ) # openxlsx may emit messages
  on.exit(
    if (file.exists("test-xlsx.xlsx")) file.remove("test-xlsx.xlsx")
  )
  dfr <- openxlsx::read.xlsx("test-xlsx.xlsx")

  expect_type(dfr[[1]], "double")
  expect_type(dfr[[2]], "double")
  expect_type(dfr[[3]], "character")
  expect_equal(dfr[[1]], 1:2 + 0.5)
  expect_equal(dfr[[2]], -1:-2 + 0.5)
  expect_equal(dfr[[3]], letters[1:2])
})
