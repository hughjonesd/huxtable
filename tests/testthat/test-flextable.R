skip_if_not_installed("flextable")

test_that("Simple conversion works", {
  hx <- huxtable(a = 1:3, b = 4:6)
  expect_error(ft <- as_flextable(hx), regexp = NA)
  expect_s3_class(ft, "flextable")
})


test_that("Text properties work", {
  hx <- huxtable(a = 1:3, b = 4:6)
  font(hx)[1, 1] <- "Times"
  font_size(hx)[1, 2] <- 14
  bold(hx)[2, 1] <- TRUE
  italic(hx)[2, 2] <- TRUE
  text_color(hx)[3, 1] <- "red"
  number_format(hx)[3, 2] <- "%3.2f"
  expect_error(as_flextable(hx), regexp = NA)
})


test_that("Borders work", {
  hx <- huxtable(a = 1:3, b = 4:6)
  top_border(hx)[1, ] <- 1
  bottom_border(hx)[1, ] <- 2
  left_border(hx)[, 1] <- 1
  right_border(hx)[, 2] <- 1
  expect_error(as_flextable(hx), regexp = NA)
})


test_that("background colour works", {
  hx <- huxtable(a = 1:3, b = 4:6)
  background_color(hx)[1:2, ] <- "yellow"
  expect_silent(as_flextable(hx))
})


test_that("table background fills otherwise unfilled flextable cells", {
  hx <- set_table_background_color(
    huxtable(a = 1:2, b = 3:4, add_colnames = FALSE),
    "red"
  )
  background_color(hx)[1, 1] <- "blue"
  ft <- as_flextable(hx)
  expect_equal(
    unname(ft$body$styles$cells$background.color$data),
    matrix(c("blue", "red", "red", "red"), 2, 2)
  )
})


test_that("merged cells work", {
  hx <- huxtable(a = 1:3, b = 4:6)
  colspan(hx)[1, 1] <- 2
  rowspan(hx)[2, 1] <- 2
  expect_silent(as_flextable(hx))
})


test_that("row heights and column widths work", {
  hx <- huxtable(a = 1:3, b = 4:6, add_colnames = FALSE)
  row_height(hx) <- c(.5, .25, .25)
  col_width(hx) <- c(.6, .4)
  expect_silent(as_flextable(hx))
})


test_that("colnames_to_header argument", {
  hx <- huxtable(a = 1:3, b = 4:6)
  expect_error(as_flextable(hx, colnames_to_header = FALSE), regexp = NA)
  expect_error(as_flextable(hx, colnames_to_header = TRUE), regexp = NA)
})


test_that("rotation works", {
  hx <- huxtable(a = 1:3, b = 4:6)
  rotation(hx)[1, 1] <- 90
  expect_silent(as_flextable(hx))
  rotation(hx)[1, 1] <- 45
  expect_warning(as_flextable(hx), "can only handle rotation")
})


test_that("caption works", {
  hx <- huxtable(a = 1:3, b = 4:6)
  caption(hx) <- "a caption"
  expect_silent(as_flextable(hx))
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


test_that("Bugfix: Quarto captions override huxtable captions in Word", {
  skip_if_not_installed("flextable")
  skip_if_not_installed("knitr")
  old_current <- knitr::opts_current$get()
  old_knit <- knitr::opts_knit$get()
  on.exit({
    knitr::opts_current$restore(old_current)
    knitr::opts_knit$restore(old_knit)
  })
  knitr::opts_knit$set(quarto.version = "1.7.0")
  knitr::opts_current$set(label = "tbl-quarto", `tbl-cap` = "Quarto caption")

  expect_warning(
    ft <- as_flextable(set_caption(hux(a = 1), "Huxtable caption")),
    "caption"
  )
  expect_null(ft$caption$value)
})


test_that("0-row/0-column huxtables work", {
  h_nrow0 <- hux(a = character(0), b = character(0), add_colnames = FALSE)
  h_ncol0 <- hux(a = 1:2)[, FALSE]
  skip("0-length tables don't work in flextable yet")
  expect_warning(as_flextable(h_nrow0), "row")
  expect_warning(as_flextable(h_ncol0), "col")
})
