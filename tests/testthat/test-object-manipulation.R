local_edition(3)


test_that("Multirow/multicol cells cannot shadow other multirow/multicol cells", {
  ht <- hux(a = 1:3, b = 1:3, d = 1:3)
  colspan(ht)[1, 2] <- 2
  expect_error(colspan(ht)[1, 1] <- 2)

  # going diagonally
  ht <- hux(a = 1:3, b = 1:3, d = 1:3)
  colspan(ht)[1, 2] <- 2
  rowspan(ht)[1, 2] <- 2
  expect_error(colspan(ht)[2, 1] <- 2)
})


test_that("huxtable manipulation preserves attribute dimnames", {
  ht <- hux(a = 1:3, b = 1:3, d = 1:3)
  expect_equal(colnames(align(ht)), c("a", "b", "d"), ignore_attr = TRUE)
  ht2 <- rbind(ht, 1:3)
  expect_equal(colnames(align(ht2)), c("a", "b", "d"), ignore_attr = TRUE)
  ht3 <- cbind(ht, e = 1:4)  # Need 4 values to match ht's row count
  expect_equal(colnames(align(ht3)), c("a", "b", "d", "e"), ignore_attr = TRUE)
  ht4 <- ht
  ht4$e <- 1:4  # Need 4 values to match ht's row count
  expect_equal(colnames(align(ht4)), c("a", "b", "d", "e"), ignore_attr = TRUE)
  ht5 <- ht
  ht5$a <- NULL
  expect_equal(colnames(align(ht5)), c("b", "d"), ignore_attr = TRUE)
})


test_that("colnames<- and friends affect attribute dimnames", {
  ht <- hux(a = 1:3, b = 1:3, d = 1:3)
  colnames(ht) <- c("e", "f", "g")
  expect_equal(colnames(align(ht)), c("e", "f", "g"), ignore_attr = TRUE)

  ht2 <- hux(a = 1:3, b = 1:3, d = 1:3)
  names(ht2) <- c("e", "f", "g")
  expect_equal(colnames(align(ht2)), c("e", "f", "g"), ignore_attr = TRUE)
  rownames(ht2) <- letters[1:4]  # Need 4 row names for 4 rows
  expect_equal(rownames(align(ht2)), letters[1:4], ignore_attr = TRUE)

  ht3 <- hux(a = 1:3, b = 1:3, d = 1:3)  # Now 4 rows (header + 3 data)
  dimnames(ht3) <- list(letters[1:4], letters[24:26])  # Need 4 row names
  expect_equal(colnames(align(ht3)), letters[24:26], ignore_attr = TRUE)
  expect_equal(rownames(align(ht3)), letters[1:4], ignore_attr = TRUE)
})


test_that("rbind and cbind work and copy properties", {
  ht <- hux(1:2, 1:2)
  italic(ht) <- TRUE
  bold(ht) <- TRUE
  row_height(ht) <- c("1in", "2in")
  col_width(ht) <- c("2cm", "1cm")

  expect_silent(ht_rbind <- rbind(ht, c(3, 3), copy_cell_props = TRUE))
  expect_equal(row_height(ht_rbind), c("1in", "2in", "2in"), ignore_attr = TRUE)
  expect_equal(italic(ht_rbind), matrix(TRUE, 3, 2), ignore_attr = TRUE)

  ht_rbind <- rbind(ht, c(3, 3), copy_cell_props = FALSE)
  expect_equal(row_height(ht_rbind), c("1in", "2in", NA), ignore_attr = TRUE)
  expect_equal(italic(ht_rbind)[3, ], c(FALSE, FALSE), ignore_attr = TRUE)

  expect_silent(ht_cbind <- cbind(ht, 1:2, copy_cell_props = TRUE))
  expect_equal(col_width(ht_cbind), c("2cm", "1cm", "1cm"), ignore_attr = TRUE)
  expect_equal(italic(ht_cbind), matrix(TRUE, 2, 3), ignore_attr = TRUE)

  ht_cbind <- cbind(ht, 1:2, copy_cell_props = FALSE)
  expect_equal(col_width(ht_cbind), c("2cm", "1cm", NA), ignore_attr = TRUE)
  expect_equal(italic(ht_cbind)[, 3], c(FALSE, FALSE), ignore_attr = TRUE)
})


test_that("rbind and cbind make numeric row_height/col_width sum to 1", {
  ht <- hux(1:2, 1:2)
  ht2 <- hux(1:2, 1:2)
  row_height(ht) <- c(.5, .5)
  row_height(ht2) <- c(.5, .5)
  col_width(ht) <- c(.5, .5)
  col_width(ht2) <- c(.5, .5)

  ht_cbind <- cbind(ht, ht2)
  expect_equal(col_width(ht_cbind), rep(.25, 4), ignore_attr = TRUE)
  ht_rbind <- rbind(ht, ht2)
  expect_equal(row_height(ht_rbind), rep(.25, 4), ignore_attr = TRUE)
})


test_that("Column names are not uglified", {
  ht <- hux("A long column name" = 1:3, "Another name" = 1:3, add_colnames = TRUE)
  expect_match(to_screen(ht), "A long column name", fixed = TRUE, all = FALSE)
  ht <- hux("A long column name" = 1:3, "Another name" = 1:3, add_colnames = FALSE)
  ht <- huxtable::add_colnames(ht)
  expect_match(to_screen(ht), "A long column name", fixed = TRUE, all = FALSE)
})


test_that("Huxtables can be transposed", {
  ht <- huxtable(Alphabet = LETTERS[1:4], Month = month.name[1:4])
  rowspan(ht)[1, 1] <- 2
  colspan(ht)[3, 1] <- 2
  font(ht)[2, 1] <- "italic"
  caption(ht) <- "A caption"
  expect_silent(trans <- t(ht))
  expect_equal(rowspan(trans)[1, 1], 1, ignore_attr = TRUE)
  expect_equal(colspan(trans)[1, 1], 2, ignore_attr = TRUE)
  expect_equal(rowspan(trans)[1, 3], 2, ignore_attr = TRUE)
  expect_equal(colspan(trans)[1, 3], 1, ignore_attr = TRUE)
  expect_equal(font(trans), matrix(c(rep(NA, 2), "italic", rep(NA, 7)), 2, 5), ignore_attr = TRUE)  # 2x5 matrix now
  expect_equal(caption(trans), "A caption", ignore_attr = TRUE)
})



test_that("cbind and rbind work with 0-dimension objects", {
  ht <- hux(a = 1:2, b = 1:2, add_colnames = FALSE)  # Keep clean for 0-dimension testing
  expect_silent(ht_nrow0 <- ht[FALSE, ])
  expect_silent(ht_ncol0 <- ht[, FALSE])

  expect_silent(res <- cbind(ht, ht_ncol0))
  expect_equal(dim(res), c(2, 2), ignore_attr = TRUE)
  expect_silent(res <- cbind(ht_ncol0, ht))
  expect_equal(dim(res), c(2, 2), ignore_attr = TRUE)

  expect_silent(res <- rbind(ht, ht_nrow0))
  expect_equal(dim(res), c(2, 2), ignore_attr = TRUE)
  expect_silent(res <- rbind(ht_nrow0, ht))
  expect_equal(dim(res), c(2, 2), ignore_attr = TRUE)

  mx <- matrix(1:4, 2, 2)
  mx_nrow0 <- mx[FALSE, ]
  mx_ncol0 <- mx[, FALSE]

  expect_silent(res <- cbind(ht, mx_ncol0))
  expect_equal(dim(res), c(2, 2), ignore_attr = TRUE)
  expect_silent(res <- cbind(mx_ncol0, ht))
  expect_equal(dim(res), c(2, 2), ignore_attr = TRUE)

  expect_silent(res <- rbind(ht, mx_nrow0))
  expect_equal(dim(res), c(2, 2), ignore_attr = TRUE)
  expect_silent(res <- rbind(mx_nrow0, ht))
  expect_equal(dim(res), c(2, 2), ignore_attr = TRUE)

  vec <- 1:2

  expect_silent(res <- rbind(ht_nrow0, vec))
  expect_equal(dim(res), c(1, 2), ignore_attr = TRUE)

  expect_silent(res <- cbind(ht_ncol0, vec))
  expect_equal(dim(res), c(2, 1), ignore_attr = TRUE)
})
