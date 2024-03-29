

local_edition(2)


test_that("One-argument [", {
  ht <- hux(a = 1:3, b = 1:3, d = 1:3)
  expect_silent(ht1 <- ht[1])
  expect_identical(ht1, ht[, 1])
  expect_silent(ht12 <- ht[1:2])
  expect_identical(ht12, ht[, 1:2])
  expect_silent(ht_all <- ht[])
  expect_identical(ht, ht_all)
  expect_silent(hta <- ht["a"])
  expect_identical(hta, ht[, 1])
  expect_silent(ht_TFF <- ht[c(TRUE, FALSE, FALSE)])
  expect_identical(ht_TFF, ht[, 1])

  expect_error(ht["q"])
  expect_warning(ht[c(TRUE, FALSE)])
  expect_error(ht[4])
})


test_that("Zero-argument [<-", {
  ht <- hux(a = 1:2, b = 1:2, add_colnames = TRUE)
  expect_silent(ht[] <- matrix(1:6, 3, 2))
  expect_identical(ht[[3, 2]], 6L)

  ht <- hux(a = 1:2, b = 1:2, add_colnames = TRUE)
  expect_silent(ht[ , ] <- matrix(1:6, 3, 2))
  expect_identical(ht[[3, 2]], 6L)
})


test_that("Subsetting preserves rownames", {
  ht <- huxtable(a = 1:3, b = 1:3)
  rownames(ht) <- letters[1:3]
  expect_equal(rownames(ht[1:2, ]), letters[1:2])
})


test_that("Subset assignment of hux into hux preserves attributes", {
  ht <- hux(a = 1:3, b = 1:3, d = 1:3)
  ht2 <- hux(1:2, 3:4)
  font(ht2) <- "italic"
  expect_silent(ht[2:3, 2:3] <- ht2)
  expect_equivalent(font(ht), matrix(c(rep(NA, 4), rep("italic", 2), NA, rep("italic", 2)), 3, 3))

  ht3 <- hux(1, 1, 1)
  row_height(ht3) <- "40px"
  ht[1, ] <- ht3
  expect_equivalent(row_height(ht)[1], "40px")

  ht4 <- hux(1:3)
  col_width(ht4) <- "20px"
  ht[, 2] <- ht4
  expect_equivalent(col_width(ht)[2], "20px")

  ht5 <- hux(a = 1:3, b = 1:3, d = 1:3)
  font(ht5) <- "times"
  expect_silent(ht[] <- ht5)
  expect_equivalent(font(ht), matrix("times", 3, 3))

  ht7 <- hux(1:2, 1:2, 1:2)
  bold(ht7)[1, ] <- TRUE
  expect_silent(ht[1:2, ] <- ht7) # assignment of a non-square matrix
  expect_equivalent(bold(ht)[1, ], rep(TRUE, 3))
  expect_equivalent(bold(ht)[2, ], rep(FALSE, 3))
})


test_that("Add columns by standard replacement methods", {
  ht <- hux(a = 1:2, b = 1:2)
  expect_silent(ht$c <- 1:2)
  expect_equivalent(font(ht), matrix(NA_character_, 2, 3))
  expect_equivalent(colnames(ht), c("a", "b", "c"))
  expect_equivalent(col_width(ht), rep(NA_real_, 3))

  ht2 <- hux(a = 1:2, b = 1:2)
  expect_silent(ht2[, "c"] <- 1:2)
  expect_equivalent(font(ht2), matrix(NA_character_, 2, 3))
  expect_equivalent(colnames(ht2), c("a", "b", "c"))
  expect_equivalent(col_width(ht2), rep(NA_real_, 3))

  ht3 <- hux(a = 1:2, b = 1:2)
  expect_silent(ht3[["c"]] <- 1:2)
  expect_equivalent(font(ht3), matrix(NA_character_, 2, 3))
  expect_equivalent(colnames(ht3), c("a", "b", "c"))
  expect_equivalent(col_width(ht3), rep(NA_real_, 3))

  ht4 <- hux(a = 1:2, b = 1:2)
  expect_silent(ht4[3:4] <- matrix(1:4, 2, 2))
  expect_equivalent(dim(ht4), c(2, 4))

  ht5 <- hux(a = 1:2, b = 1:2)
  expect_silent(ht5[, 3:4] <- matrix(1:4, 2, 2))
  expect_equivalent(dim(ht5), c(2, 4))

  ht6 <- hux(a = 1:2, b = 1:2)
  expect_silent(ht6[3:4, ] <- matrix(1:4, 2, 2))
  expect_equivalent(dim(ht6), c(4, 2))
})


test_that("Can delete columns from a huxtable by setting it to `NULL`", {
  ht <- hux(a = 1:2, b = 1:2)
  expect_silent(ht$a <- NULL)
  expect_equivalent(font(ht), matrix(NA_character_, 2, 1))
  expect_equivalent(col_width(ht), NA_real_)

  ht2 <- hux(a = 1:2, b = 1:2)
  expect_silent(ht2[["a"]] <- NULL)
  expect_equivalent(font(ht2), matrix(NA_character_, 2, 1))
  expect_equivalent(col_width(ht2), NA_real_)

  ht3 <- hux(a = 1:2, b = 1:2)
  expect_silent(ht3["a"] <- NULL)
  expect_equivalent(font(ht3), matrix(NA_character_, 2, 1))
  expect_equivalent(col_width(ht3), NA_real_)

  # this kind of subsetting doesn't seem to work in earlier Rs
  if (getRversion() >= "3.3.3") {
    ht4 <- hux(a = 1:2, b = 1:2, c = 1:2)
    expect_silent(ht4[ c("a", "b")] <- NULL)
    expect_equivalent(font(ht4), matrix(NA_character_, 2, 1))
    expect_equivalent(col_width(ht4), NA_real_)

    ht5 <- hux(a = 1:2, b = 1:2, c = 1:2)
    expect_silent(ht5[, c("a", "b")] <- NULL)
    expect_equivalent(font(ht5), matrix(NA_character_, 2, 1))
    expect_equivalent(col_width(ht5), NA_real_)
  }
})


test_that("Add row(s) by standard replacement methods", {
  ht <- hux(a = 1:2, b = 1:2)
  expect_silent(ht[3, ] <- c(3, 3))
  expect_equivalent(font(ht), matrix(NA_character_, 3, 2))
  expect_equivalent(row_height(ht), rep(NA_real_, 3))

  expect_error(ht[4, 1] <- 4)
  expect_error(ht[5:6, ] <- 5:6)

  # can't add new columns with wrong dimension
  ht2 <- hux(a = 1:2, b = 1:2)
  expect_error(ht2[, 3:4] <- 1:2)
  expect_error(ht2[, 2:3] <- 3:4)

  # can't add new rows and columns simultaneously
  ht3 <- hux(a = 1:2, b = 1:2)
  expect_error(ht3[3:4, 3] <- 1)
})


test_that("[[<-", {
  ht <- hux(a = 1:2, b = 1:2)
  expect_silent(ht[[1, 1]] <- 0)
  expect_equivalent(ht[[1, 1]], 0)
  expect_silent(ht[[2, "b"]] <- 0)
  expect_equivalent(ht[[2, 2]], 0)
})


test_that("Setting only some rows/cols to NULL is an error", {
  ht <- hux(1:3, 1:3, 1:3)
  expect_error(ht[1:2, 1:2] <- NULL)
  expect_error(ht[[1, 1]] <- NULL)
})
