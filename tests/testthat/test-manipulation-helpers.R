local_edition(3)


test_that("add_colnames() does not screw up dates and similar", {
  date_str <- rep("2015/05/05 12:00", 2)
  dfr <- data.frame(
    date    = as.Date(date_str),
    POSIXct = as.POSIXct(date_str),
    POSIXlt = as.POSIXlt(date_str)
  )
  ht <- as_hux(dfr, add_colnames = TRUE)
  ht2 <- add_colnames(as_hux(dfr, add_colnames = FALSE))
  for (h in list(ht, ht2)) {
    for (col in colnames(dfr)) {
      expect_match(to_screen(h[, col]), "2015-05-05")
    }
  }
})


test_that("add_footnote works", {
  ht_orig <- hux(a = 1:2, b = 1:2)
  ht_orig <- add_footnote(ht_orig, "Some footnote text", italic = TRUE)
  expect_equal(nrow(ht_orig), 4)  # header + 2 data + 1 footnote
  expect_equal(colspan(ht_orig)[[4, 1]], ncol(ht_orig))  # footnote is in row 4
  expect_true(italic(ht_orig)[4, 1])  # footnote is in row 4
})


test_that("insert_column and insert_row work", {
  ht_orig <- hux(a = 1:2, b = 1:2)
  ht <- insert_row(ht_orig, 8, 9)
  expect_equal(nrow(ht), 4)  # header + 2 data + 1 inserted row
  expect_equal(as.character(ht[1, 2]), "9")  # Inserted at top, column 2

  ht <- insert_row(ht_orig, 8, 9, after = 1)
  expect_equal(nrow(ht), 4)  # header + 1 data + 1 inserted + 1 data
  expect_equal(as.character(ht[[2, 2]]), "9")  # Check individual values
  expect_equal(as.character(ht[[3, 2]]), "1")
  expect_equal(as.character(ht[[4, 2]]), "2")

  ht <- insert_row(ht_orig, 1, fill = 0, after = 1)
  expect_equal(unname(as.matrix(ht[2, ])), unname(as.matrix(huxtable("1", "0", add_colnames = FALSE))))

  ht <- insert_column(ht_orig, c("X", "8", "9"))  # Header + data for all 3 rows
  expect_equal(ncol(ht), 3)
  expect_equal(as.character(ht[2, 1]), "8")  # First data row, first column

  ht <- insert_column(ht_orig, c("X", "8", "9"), after = 1)
  expect_equal(ncol(ht), 3)
  expect_equal(as.character(ht[1, 1]), "a")  # Check individual values instead
  expect_equal(as.character(ht[1, 2]), "X")  # Check individual values instead
  expect_equal(as.character(ht[1, 3]), "b")  # Check individual values instead

  ht <- insert_column(ht_orig, c("Y", "3", "0"), fill = 0)
  expect_equal(as.character(ht[[2, 1]]), "3")  # Check individual values
  expect_equal(as.character(ht[[3, 1]]), "0")

  ht <- insert_column(ht_orig, "3", fill = 0, rowspan = 2)
  expect_equal(as.vector(rowspan(ht)[1:2, 1]), c(2, 1))  # Header and first data row

  ht <- insert_row(ht_orig, 3, after = 1, fill = 0, colspan = 2)
  expect_equal(as.vector(colspan(ht)[2, ]), c(2, 1))

  bold(ht_orig) <- TRUE
  ht <- insert_column(ht_orig, c("X", "8", "9"), after = 1)
  expect_true(bold(ht)[1, 2])
  ht <- insert_column(ht_orig, c("X", "8", "9"), after = 1, copy_cell_props = FALSE)
  expect_false(bold(ht)[1, 2])
})


test_that("insert_column works with column names", {
  ht_orig <- hux(a = 1:2, b = 1:2)
  ht <- insert_column(ht_orig, c("X", "8", "9"), after = "a")  # Header + data for all rows
  expect_equal(ncol(ht), 3)
  expect_equal(as.character(ht[[2, 2]]), "8")  # Check individual values
  expect_equal(as.character(ht[[3, 2]]), "9")
})


test_that("bug: insert_row/column doesn't overwrite caption", {
  ht <- hux("1", add_colnames = FALSE)  # Keep clean for this bug test
  caption(ht) <- "Caption"
  ht <- insert_column(ht, "2")
  expect_equal(caption(ht), "Caption")

  ht <- hux("1", add_colnames = FALSE)  # Keep clean for this bug test
  caption(ht) <- "Caption"
  ht <- insert_row(ht, "2")
  expect_equal(caption(ht), "Caption")
})


test_that("add_rows and add_columns work", {
  ht <- hux(a = 1:2, b = 1:2, add_colnames = FALSE)
  expect_silent(res <- add_rows(ht, 3:4))
  expect_equal(nrow(res), 3)
  expect_equal(res[[3, 1]], 3)
  expect_silent(res <- add_rows(ht, 3:4, after = 0))
  expect_equal(nrow(res), 3)
  expect_equal(res[[1, 1]], 3)

  mx <- matrix(3:6, 2, 2)
  hx2 <- hux(3:4, 5:6, add_colnames = FALSE)  # Keep clean for comparison
  for (obj in list(mx, hx2)) {
    expect_silent(res <- add_rows(ht, obj))
    expect_equal(res[[4, 2]], 6)
    expect_equal(nrow(res), 4)
    expect_silent(res <- add_rows(ht, obj, after = 1))
    expect_equal(nrow(res), 4)
    expect_equal(res[[3, 2]], 6)
    expect_silent(res <- add_columns(ht, obj, after = "a"))
    expect_equal(ncol(res), 4)
    expect_equal(res[[1, 2]], 3)
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
  expect_equal(nrow(res), 4)
  expect_equal(bold(res)[[3, 1]], TRUE)

  dfr <- data.frame(c = 1:2, d = 1:2)
  expect_silent(res <- add_columns(ht, dfr))
  expect_equal(ncol(res), 4)
  expect_equal(bold(res)[[1, 3]], TRUE)
})


test_that("add_rows and add_columns don't break with matrices", {
  ht <- as_hux(matrix(1, 1, 1))
  prob <- rbind(ht, rbind(1, 1))
  expect_equal(length(row_height(prob)), 3)
})


test_that("add_footnote sets number_format correctly", {
  ht <- hux(1, add_colnames = FALSE)  # Keep clean for number formatting test
  ht2 <- add_footnote(ht, "a 1985 b")
  expect_match(to_screen(ht2), "a 1985 b")
  ht3 <- add_footnote(ht, "a 1 b", number_format = 2)
  expect_match(to_screen(ht3), "a 1.00 b")
})
