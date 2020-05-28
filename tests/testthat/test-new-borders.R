
test_that("bdr objects", {
  expect_silent(b <- bdr(1, "solid", "red"))
  expect_s3_class(b, "bdr")
  expect_true(is_bdr(b))
  expect_true(is_borderish(b))
  expect_true(is_borderish(1))
  expect_false(is_borderish(-1))

  expect_silent(b <- bdr())
  expect_equivalent(b$thickness, 0.4)
  expect_equivalent(b$style, "solid")
  expect_equivalent(b$color, NA_character_)
})


test_that("bdr object printing", {
  expect_silent(b <- bdr(2, "solid", "red"))
  expect_match(format(b), "2.*solid.*red")
  expect_output(print(b), "2.*solid.*red")
})



test_that("left_border", {
  ht <- hux(1:3, 1:3)
  expect_s3_class(left_border(ht), "matrix")
  expect_true(is.matrix(left_border(ht)[1:2, 1:2]))
  expect_type(left_border(ht)[1, 1], "double")
})


test_that("left_border(ht) <- number", {
  ht <- hux(1:3, 1:3)
  expect_silent(left_border(ht) <- 1)
  expect_equivalent(left_border(ht)[], matrix(1, 3, 2))
  expect_equivalent(left_border(ht)[1, 1], 1)
})


test_that("left_border(ht) <- bdr(...)", {
  ht <- hux(1:3, 1:3)
  expect_silent(left_border(ht) <- bdr(1, "double", "red"))
  expect_equivalent(left_border(ht)[], matrix(1, 3, 2))
  expect_equivalent(left_border_style(ht), matrix("double", 3, 2))
  expect_equivalent(left_border_color(ht), matrix("red", 3, 2))
})


test_that("left_border(ht)[i,j] <-number", {
  ht <- hux(1:3, 1:3)
  expect_silent(left_border(ht)[1:2, 1] <- 1)
  expect_equivalent(left_border(ht)[], matrix(c(1, 1, 0, 0, 0, 0), 3, 2))
  expect_equivalent(left_border(ht)[1, 1], 1)
})


test_that("left_border(ht)[i, j] <- bdr(...)", {
  ht <- hux(1:3, 1:3)
  expect_silent(left_border(ht)[1:2, 1] <- bdr(1, "double", "red"))
  expect_equivalent(left_border(ht)[1, 1], 1)
})


test_that("set_left_border(ht, number)", {
  ht <- hux(1:3, 1:3)
  ht2 <- set_left_border(ht, 1)
  expect_equivalent(left_border(ht2)[], matrix(1, 3, 2))

  ht3 <- set_left_border(ht)
  expect_equivalent(left_border(ht3)[], matrix(0.4, 3, 2))
})



test_that("set_left_border(ht, bdr)", {
  ht <- hux(1:3, 1:3)
  ht2 <- set_left_border(ht, bdr(1, "double", "red"))
  expect_equivalent(left_border(ht2)[], matrix(1, 3, 2))
  expect_equivalent(left_border_style(ht2), matrix("double", 3, 2))
  expect_equivalent(left_border_color(ht2), matrix("red", 3, 2))
})


test_that("set_left_border(ht, row, col, number)", {
  ht <- hux(1:3, 1:3)
  ht2 <- set_left_border(ht, 1, 1, 1)
  expect_equivalent(left_border(ht2)[], matrix(c(1, 0, 0, 0, 0, 0), 3, 2))

  ht3 <- set_left_border(ht, everywhere, 1, 1)
  expect_equivalent(left_border(ht3)[], matrix(c(1, 1, 1, 0, 0, 0), 3, 2))

  colnames(ht) <- c("a", "b")
  ht4 <- set_left_border(ht, everywhere, "a", 1)
  expect_equivalent(left_border(ht4)[], matrix(c(1, 1, 1, 0, 0, 0), 3, 2))

  ht5 <- set_left_border(ht, everywhere, 1)
  expect_equivalent(left_border(ht5)[], matrix(c(0.4, 0.4, 0.4, 0, 0, 0), 3, 2))
})


test_that("set_left_border(ht, row, col, bdr)", {
  ht <- hux(1:3, 1:3)
  ht2 <- set_left_border(ht, 1, 1, bdr(1, "double", "red"))
  expect_equivalent(left_border(ht2)[], matrix(c(1, 0, 0, 0, 0, 0), 3, 2))
  expect_equivalent(left_border_style(ht2),
        matrix(c("double", rep("solid", 5)), 3, 2))
  expect_equivalent(left_border_color(ht2), matrix(c("red", rep(NA, 5)), 3, 2))
})


test_that("map_left_border", {
  ht <- hux(1:2, 2:1)
  ht2 <- map_left_border(ht, by_ranges(1.5, c(0, 1)))
  expect_equivalent(left_border(ht2)[], matrix(c(0, 1, 1, 0), 2, 2))

  ht3 <- map_left_border(ht, by_ranges(1.5, list(bdr(0), bdr(1))))
  expect_equivalent(left_border(ht3)[], matrix(c(0, 1, 1, 0), 2, 2))

  ht4 <- map_left_border(ht, by_rows(bdr(0), bdr(1)))
  expect_equivalent(left_border(ht4)[], matrix(c(0, 1, 0, 1), 2, 2))

  ht5 <- map_left_border(ht, by_cols(bdr(0), bdr(1)))
  expect_equivalent(left_border(ht5)[], matrix(c(0, 0, 1, 1), 2, 2))

  # ht8 <- map_left_border(ht, by_cases(. == 2 ~ bdr(1), TRUE ~ bdr(0)))
  # expect_equivalent(left_border(ht8)[], matrix(c(0, 1, 1, 0), 2, 2))

  ht9 <- map_left_border(ht, by_quantiles(0.5, list(bdr(0), bdr(1))))
  expect_equivalent(left_border(ht9)[], matrix(c(0, 1, 1, 0), 2, 2))

  ht10 <- map_left_border(ht, by_equal_groups(2, list(bdr(0), bdr(1))))
  expect_equivalent(left_border(ht10)[], matrix(c(0, 1, 1, 0), 2, 2))

  ht_char <- hux(letters[1:2], letters[2:1])
  ht6 <- map_left_border(ht_char, by_values("a" = bdr(0), "b" = bdr(1)))
  expect_equivalent(left_border(ht6)[], matrix(c(0, 1, 1, 0), 2, 2))

  ht7 <- map_left_border(ht, by_function(function (x) bdr(1)))
  expect_equivalent(left_border(ht7)[], matrix(1, 2, 2))
})


test_that("map_all_borders", {
  ht <- hux(1:2, 2:1)
  ht2 <- map_all_borders(ht, by_ranges(1.5, c(0, 1)))
  # these can be guaranteed whatever the order
  expect_equivalent(left_border(ht2)[2, 1], 1)
  expect_equivalent(bottom_border(ht2)[2, 1], 1)
  expect_equivalent(right_border(ht2)[2, 2], 0)
  expect_equivalent(left_border(ht2)[1, 1], 0)

  ht3 <- map_all_borders(ht, by_ranges(1.5, list(bdr(0), bdr(1))))
  expect_equivalent(left_border(ht3)[2, 1], 1)
  expect_equivalent(bottom_border(ht3)[2, 1], 1)
  expect_equivalent(right_border(ht3)[2, 2], 0)
  expect_equivalent(left_border(ht3)[1, 1], 0)


  ht4 <- map_all_borders(ht, 1, 1:2, by_ranges(1.5, list(bdr(0), bdr(1))))
  expect_equivalent(left_border(ht4)[2, 1], 0)
  expect_equivalent(bottom_border(ht4)[2, 1], 0)
  expect_equivalent(right_border(ht4)[2, 2], 0)
  expect_equivalent(left_border(ht4)[1, 1], 0)
  expect_equivalent(right_border(ht4)[1, 2], 1)
  expect_equivalent(top_border(ht4)[1, 2], 1)
})


test_that("set-multiple with bdr", {
  ht <- hux(a = 1:2, b = 1:2, add_colnames = FALSE)
  b <- bdr(2, "dotted", "green")

  expect_silent(ht2 <- set_all_borders(ht, b))
  expect_equivalent(left_border_color(ht2), matrix("green", 2, 2))
  expect_equivalent(left_border_style(ht2), matrix("dotted", 2, 2))
  expect_equivalent(left_border(ht2)[], matrix(2, 2, 2))
  expect_equivalent(right_border_color(ht2), matrix("green", 2, 2))
  expect_equivalent(right_border_style(ht2), matrix("dotted", 2, 2))
  expect_equivalent(right_border(ht2)[], matrix(2, 2, 2))
  expect_equivalent(top_border_color(ht2), matrix("green", 2, 2))
  expect_equivalent(top_border_style(ht2), matrix("dotted", 2, 2))
  expect_equivalent(top_border(ht2)[], matrix(2, 2, 2))
  expect_equivalent(bottom_border_color(ht2), matrix("green", 2, 2))
  expect_equivalent(bottom_border_style(ht2), matrix("dotted", 2, 2))
  expect_equivalent(bottom_border(ht2)[], matrix(2, 2, 2))

  expect_silent(ht3 <- set_all_borders(ht, 1, 1, b))
  # sets right border of (1,1) = left border of (1,2)
  expect_equivalent(left_border_color(ht3),
        matrix(c("green", NA, "green", NA), 2, 2))

  expect_silent(ht4 <- set_tb_borders(ht, b))
  expect_equivalent(left_border_color(ht4), matrix(NA_character_, 2, 2))
  expect_equivalent(top_border_color(ht4), matrix("green", 2, 2))
  expect_equivalent(left_border_style(ht4), matrix("solid", 2, 2))
  expect_equivalent(top_border_style(ht4), matrix("dotted", 2, 2))

  expect_silent(ht5 <- set_tb_borders(ht, 1, 1, b))
  expect_equivalent(left_border_color(ht5), matrix(NA_character_, 2, 2))
  expect_equivalent(top_border_color(ht5),
        matrix(c("green", "green", NA_character_, NA_character_), 2, 2))
  expect_equivalent(left_border_style(ht5), matrix("solid", 2, 2))
  expect_equivalent(top_border_style(ht5),
        matrix(c("dotted", "dotted", "solid", "solid"), 2, 2))

  expect_silent(ht6 <- set_lr_borders(ht, b))
  expect_equivalent(left_border_color(ht6), matrix("green", 2, 2))
  expect_equivalent(top_border_color(ht6), matrix(NA_character_, 2, 2))
  expect_equivalent(left_border_style(ht6), matrix("dotted", 2, 2))
  expect_equivalent(top_border_style(ht6), matrix("solid", 2, 2))

  expect_silent(ht7 <- set_lr_borders(ht, 1, 1, b))
  expect_equivalent(left_border_color(ht7),
        matrix(c("green", NA_character_, "green", NA_character_), 2, 2))
  expect_equivalent(top_border_color(ht7), matrix(NA_character_, 2, 2))
  expect_equivalent(left_border_style(ht7),
        matrix(c("dotted", "solid", "dotted", "solid"), 2, 2))
  expect_equivalent(top_border_style(ht7), matrix("solid", 2, 2))
})


test_that("edit_defaults with borders", {
  expect_silent(
    old <- set_default_properties(border = 2, border_style = "double", border_color = "green")
  )
  on.exit(
    set_default_properties(old)
  )

  ht <- hux(1)
  expect_equivalent(left_border(ht)[], matrix(2, 1, 1))
  expect_equivalent(left_border_style(ht), matrix("double", 1, 1))
  expect_equivalent(left_border_color(ht), matrix("green", 1, 1))
})
