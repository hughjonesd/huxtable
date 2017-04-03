
context("Object manipulation")


source('functions.R')


ht <- huxtable(a = 1:3, b = 1:3)


test_that('Object creation examples unchanged', {
  test_ex_same('huxtable')
})


test_that('Object subsetting and replacement examples unchanged', {
  test_ex_same('extract-methods')
  test_ex_same('add_colnames')
  test_ex_same('cbind.huxtable')
  test_ex_same('t.huxtable')
})


test_that('Subsetting preserves rownames', {
  rownames(ht) <- letters[1:3]
  expect_equal(rownames(ht[1:2, ]), letters[1:2])
})


test_that('Subsetting cuts colspan', {
  ht <- hux(a = 1:3, b = 1:3, d = 1:3)
  rowspan(ht)[1, 1] <- 3
  colspan(ht)[1, 2] <- 2
  ss <- ht[1:2, 1:2]
  expect_equivalent(rowspan(ss)[1, 1], 2)
  expect_equivalent(colspan(ss)[1, 2], 1)
})


test_that('Spans can\'t be overwritten', {
  ht <- hux(a = 1:3, b = 1:3, d = 1:3)
  colspan(ht)[1, 2] <- 2
  expect_error(colspan(ht)[1, 1] <- 2)
})


test_that('Subset assignment of hux into hux preserves attributes', {
  ht <- hux(a = 1:3, b = 1:3, d = 1:3)
  ht2 <- hux(1:2, 3:4)
  font(ht2) <- 'italic'
  expect_silent(ht[2:3, 2:3] <- ht2)
  expect_equivalent(font(ht), matrix(c(rep(NA, 4), rep('italic', 2), NA, rep('italic', 2)), 3, 3))
  ht3 <- hux(1, 1, 1)
  row_height(ht3) <- '40px'
  ht[1,] <- ht3
  expect_equivalent(row_height(ht)[1], '40px')
  ht4 <- hux(1:3)
  col_width(ht4) <- '20px'
  ht[,2] <- ht4
  expect_equivalent(col_width(ht)[2], '20px')
})


test_that('Column names are not uglified', {
  skip('Can\'t solve this one at the moment.')
  ht <- hux('A long column name' = 1:3, 'Another name' = 1:3, add_colnames = TRUE)
  expect_match(to_screen(ht), 'A long column name', fixed = TRUE, all = FALSE)
  ht <- hux('A long column name' = 1:3, 'Another name' = 1:3, add_colnames = TRUE)
  ht <- huxtable::add_colnames(ht)
  expect_match(to_screen(ht), 'A long column name', fixed = TRUE, all = FALSE)
})


test_that('Huxtables can be transposed', {
  ht <- huxtable(Alphabet = LETTERS[1:4], Month = month.name[1:4])
  rowspan(ht)[1,1] <- 2
  colspan(ht)[3,1] <- 2
  font(ht)[2,1] <- 'italic'
  caption(ht) <- 'A caption'
  expect_silent(trans <- t(ht))
  expect_equivalent(rowspan(trans)[1,1], 1)
  expect_equivalent(colspan(trans)[1,1], 2)
  expect_equivalent(rowspan(trans)[1,3], 2)
  expect_equivalent(colspan(trans)[1,3], 1)
  expect_equivalent(font(trans), matrix(c(rep(NA, 2), 'italic', rep(NA, 5)), 2, 4))
  expect_equivalent(caption(trans), 'A caption')
})

test_that('add_colnames works with as_hux for matrices', {
  mat <- matrix(1:4, 2, 2, dimnames = list(letters[1:2], LETTERS[1:2]))
  ht <- as_hux(mat, add_colnames = TRUE, add_rownames = TRUE)
  expect_equivalent(ht[1, 2:3], colnames(mat))
  expect_equivalent(ht$rownames[2:3], rownames(mat))
})


test_that('add_footnote works', {
  ht_orig <- hux(a = 1:2, b = 1:2)
  ht_orig <- add_footnote(ht_orig, 'Some footnote text', italic = TRUE)
  expect_equivalent(nrow(ht_orig), 3)
  expect_equivalent(colspan(ht_orig)[3, 1], ncol(ht_orig))
  expect_true(italic(ht_orig)[3, 1])
})
