
test_that("split_across", {
  ht <- as_hux(col(matrix(NA, 4, 4)))

  expect_length(htl <- split_across(ht, c(1, 3)), 3)
  expect_identical(htl[[1]], ht[1, ])
  expect_identical(htl[[2]], ht[2:3, ])
  expect_identical(htl[[3]], ht[4, ])

  expect_error(split_across(ht, NA))
  expect_error(split_across(ht, "unicorn"))
  expect_error(split_across(ht, -1))
  expect_error(split_across(ht, 5))
})


test_that("split_down", {
  ht <- as_hux(row(matrix(NA, 4, 4)))

  expect_length(htl <- split_down(ht, c(1, 3)), 3)
  expect_identical(htl[[1]], ht[1])
  expect_identical(htl[[2]], ht[2:3])
  expect_identical(htl[[3]], ht[4])

  expect_error(split_down(ht, NA))
  expect_error(split_down(ht, "unicorn"))
  expect_error(split_down(ht, -1))
  expect_error(split_down(ht, 5))
})


test_that("split_down tidyselect", {
  jsplit <- split_down(jams, 1)
  expect_identical(split_down(jams, "Type"), jsplit)
  expect_identical(split_down(jams, starts_with("T")), jsplit)
  expect_identical(split_down(jams, odds), jsplit)
})


test_that("restack_across", {

})
