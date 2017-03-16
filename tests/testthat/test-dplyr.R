

context('dplyr functions')


source('functions.R')


test_that('select and rename work', {
  ht <- hux(a = 1:2, b = 1:2, c = 1:2, d = 1:2)
  bold(ht)[1,] <- TRUE
  ht2 <- dplyr::select(ht, b:c)
  expect_equivalent(bold(ht2), matrix(c(TRUE, FALSE, TRUE, FALSE), 2, 2))
  expect_identical(ht2, ht[,2:3])
  ht3 <- dplyr::rename(ht, jim = d, bob = c)
  expect_equivalent(colnames(ht3), c('a', 'b', 'bob', 'jim'))
  expect_equivalent(bold(ht3), bold(ht))
})

test_that('slice, filter and arrange work', {
  ht <- hux(a = 1:3, b = 3:1)
  row_height(ht) <- c(.7,.2, .1)

  ht2 <- dplyr::slice(ht, 1:2)
  expect_identical(ht2, ht[1:2,])

  ht3 <- dplyr::arrange(ht, b)
  expect_identical(ht3, ht[3:1,])

  ht4 <- dplyr::filter(ht, a <= 2, b <= 2)
  expect_identical(ht4, ht[2,])
})

test_that('mutate and transmute work', {
  ht <- hux(a = 1:3, b = 1:3)
  bold(ht)[1,] <- TRUE

  ht2 <- dplyr::mutate(ht, x = a + b)
  expect_equivalent(ht2$x, c(2, 4, 6))
  expect_equivalent(bold(ht2)[,3], c(TRUE, FALSE, FALSE))

  ht3 <- dplyr::mutate(ht, x = a + b, copy_cell_props = FALSE)
  expect_equivalent(bold(ht3)[,3], c(FALSE, FALSE, FALSE))

  ht4 <- dplyr::transmute(ht, x = a + b, a = a + b)
  expect_equivalent(ht4$x, c(2, 4, 6))
  expect_equivalent(bold(ht4)[,1], c(FALSE, FALSE, FALSE))
  expect_equivalent(bold(ht4)[,2], c(TRUE, FALSE, FALSE))
})
