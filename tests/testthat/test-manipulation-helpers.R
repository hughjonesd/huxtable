
context("object manipulation helpers")


test_that("add_colnames() does not screw up dates and similar", {
  date_str <- rep("2015/05/05 12:00", 2)
  dfr <- data.frame(
    date    = as.Date(date_str),
    POSIXct = as.POSIXct(date_str),
    POSIXlt = as.POSIXlt(date_str)
  )
  ht <- as_hux(dfr, add_colnames = TRUE)
  ht2 <- add_colnames(as_hux(dfr, add_colnames = FALSE))
  for (h in list(ht, ht2)) for (col in colnames(dfr)) {
    expect_match(to_screen(h[, col]), "2015-05-05")
  }
})


test_that("add_footnote works", {
  ht_orig <- hux(a = 1:2, b = 1:2)
  ht_orig <- add_footnote(ht_orig, "Some footnote text", italic = TRUE)
  expect_equivalent(nrow(ht_orig), 3)
  expect_equivalent(colspan(ht_orig)[3, 1], ncol(ht_orig))
  expect_true(italic(ht_orig)[3, 1])
})


test_that("insert_column and insert_row work", {
  ht_orig <- hux(a = 1:2, b = 1:2)
  ht <- insert_row(ht_orig, 8, 9)
  expect_equivalent(nrow(ht), 3)
  expect_equivalent(ht[1, 2], 9)

  ht <- insert_row(ht_orig, 8, 9, after = 1)
  expect_equivalent(nrow(ht), 3)
  expect_equivalent(ht[, 2], huxtable(b = c(1, 9, 2)))

  ht <- insert_column(ht_orig, 8, 9)
  expect_equivalent(ncol(ht), 3)
  expect_equivalent(ht[2, 1], 9)

  ht <- insert_column(ht_orig, 8, 9, after = 1)
  expect_equivalent(ncol(ht), 3)
  expect_equivalent(ht[1, ], huxtable(a = 1, 8, b = 1))

  bold(ht_orig) <- TRUE
  ht <- insert_column(ht_orig, 8, 9, after = 1)
  expect_true(bold(ht)[1, 2])
  ht <- insert_column(ht_orig, 8, 9, after = 1, copy_cell_props = FALSE)
  expect_false(bold(ht)[1, 2])
})


test_that("insert_column works with column names", {
  ht_orig <- hux(a = 1:2, b = 1:2)
  ht <- insert_column(ht_orig, 8, 9, after = "a")
  expect_equivalent(ncol(ht), 3)
  expect_equivalent(ht[, 2], huxtable(8:9))
})


test_that("add_rows and add_columns work", {
  ht <- hux(a = 1:2, b = 1:2, add_colnames = FALSE)
  expect_silent(res <- add_rows(ht, 3:4))
  expect_equivalent(nrow(res), 3)
  expect_equivalent(res[[3, 1]], 3)
  expect_silent(res <- add_rows(ht, 3:4, after = 0))
  expect_equivalent(nrow(res), 3)
  expect_equivalent(res[[1, 1]], 3)

  mx <- matrix(3:6, 2, 2)
  hx2 <- hux(3:4, 5:6)
  for (obj in list(mx, hx2)) {
    expect_silent(res <- add_rows(ht, obj))
    expect_equivalent(res[[4, 2]], 6)
    expect_equivalent(nrow(res), 4)
    expect_silent(res <- add_rows(ht, obj, after = 1))
    expect_equivalent(nrow(res), 4)
    expect_equivalent(res[[3, 2]], 6)
    expect_silent(res <- add_columns(ht, obj, after = "a"))
    expect_equivalent(ncol(res), 4)
    expect_equivalent(res[[1, 2]], 3)
  }

  bold(ht) <- TRUE
  expect_silent(res <- add_rows(ht, mx, copy_cell_props = TRUE))
  expect_true(bold(res)[3, 1])
})


test_that("add_columns and add_rows work with data frames", {
  ht <- hux(a = 1:2, b = 1:2, add_colnames = FALSE)
  bold(ht) <- TRUE
  dfr <- data.frame(a = 1:2, b = 1:2)
  expect_silent(res <- add_rows(ht, dfr))
  expect_equivalent(nrow(res), 4)
  expect_equivalent(bold(res)[3, 1], TRUE)

  dfr <- data.frame(c = 1:2, d = 1:2)
  expect_silent(res <- add_columns(ht, dfr))
  expect_equivalent(ncol(res), 4)
  expect_equivalent(bold(res)[1, 3], TRUE)
})


test_that("add_rows and add_columns don't break with matrices", {
  ht <- as_hux(matrix(1, 1, 1))
  prob <- rbind(ht, rbind(1,1))
  expect_equivalent(length(row_height(prob)), 3)
})
