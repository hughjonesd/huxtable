local_edition(3)


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
  expect_identical(split_down(jams, tidyselect::starts_with("T")), jsplit)
  expect_identical(split_down(jams, odds), jsplit)
})


test_that("split width/height", {
  square <- as_hux(matrix(1:16, 4, 4))
  col_width(square) <- c(.2, .3, .3, .2)
  row_height(square) <- c(.2, .3, .3, .2)

  expect_equal(
    split_down(square, after = 2),
    split_down(square, width = 0.5),
    ignore_attr = TRUE
  )
  expect_equal(
    split_across(square, after = 2),
    split_across(square, height = 0.5),
    ignore_attr = TRUE
  )

  expect_error(split_across(square, height = .25, after = 2))
  expect_error(split_across(square, height = c(.25, .5)))
  expect_error(split_across(square, height = c(.25, .5)))
  expect_error(split_across(square, height = NA))
  expect_error(split_across(square, height = "unicorn"))

  expect_error(split_down(square, width = .25, after = 2))
  expect_error(split_down(square, width = c(.25, .5)))
  expect_error(split_down(square, width = c(.25, .5)))
  expect_error(split_down(square, width = NA))
  expect_error(split_down(square, width = "unicorn"))
})


test_that("basic restack", {
  square <- as_hux(matrix(1:16, 4, 4))
  expect_silent(wide <- restack_across(square, 2))
  expect_equal(dim(wide), c(2, 8))
  expect_silent(long <- restack_down(square, 2))
  expect_equal(dim(long), c(8, 2))

  expect_warning(restack_across(square, 3))
  expect_warning(restack_down(square, 3))
  expect_silent(restack_across(square, 3, on_remainder = "fill"))
  expect_silent(restack_down(square, 3, on_remainder = "fill"))
  expect_error(restack_across(square, 3, on_remainder = "stop"))
  expect_error(restack_down(square, 3, on_remainder = "stop"))
})


test_that("restack headers", {
  jams_l <- jams[c(1, 2, 3, 4, 4), ]
  expect_silent(wide_jams <- restack_across(jams_l, 3))
  expect_equal(
    as.character(wide_jams[1, ]),
    rep(c("Type", "Price"), 2),
    ignore_attr = TRUE
  )
  expect_equal(header_rows(wide_jams), c(TRUE, FALSE, FALSE), ignore_attr = TRUE)

  expect_silent(jw2 <- restack_across(jams, 2, headers = FALSE))
  expect_equal(
    as.character(jw2[1, 1:3]),
    c("Type", "Price", "Raspberry"),
    ignore_attr = TRUE
  )
})
