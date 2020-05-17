
context("map interface")


test_that("Standard map_xxx", {

  ht <- huxtable(
          Type  = c("Strawberry", "Raspberry", "Plum"),
          Price = c(1.90, 2.10, 1.80),
          add_colnames = FALSE
        )
  align(ht) <- "left"

  test_map <- function (map_fn, result, rows = 1:nrow(ht), cols = 1:ncol(ht)) {
    result <- strsplit(result, "")[[1]]
    result <- c("l" = "left", "c" = "center", "r" = "right")[result]
    expect_equivalent(
            align(map_align(ht, rows, cols, map_fn)),
            matrix(result, nrow(ht), ncol(ht))
          )
  }

  test_map(by_cols("centre", "right"),
       "cccrrr")
  test_map(by_cols("centre", "right"), "cclrrl", 1:2, 1:2)
  test_map(by_cols("right"), "lllrrr", 1:3, 2)

  test_map(by_rows("left", "centre", "right"), "lcrlcr")
  test_map(by_rows("left", "centre", "right"), "lcrlll", 1:3, 1)
  test_map(by_rows("right"), "llrllr", 3, 1:2)

  test_map(by_values(Strawberry = "right", Plum = "right", "centre"), "rcrccc")

  f <- function (x) ifelse(x == "Plum", "center", "right")
  test_map(by_function(f), "rrcrrr")

  test_map(by_regex(".*berry" = "right", "\\." = "centre"), "rrlccc")

  test_map(by_equal_groups(3, c("left", "centre", "right")), "lllcrl", 1:3, 2)
  test_map(by_quantiles(0.75, c("centre", "right")), "lllcrc", 1:3, 2)

  ht_2col <- hux(1:3, 4:6)
  expect_equivalent(
          align(map_align(ht_2col,
            by_equal_groups(3, c("left", "center", "right"), colwise = TRUE))),
          matrix(rep(c("left", "center", "right"), 2), 3, 2)
        )
  expect_equivalent(
    align(map_align(ht_2col,
      by_quantiles(c(.1, .9), c("left", "center", "right"), colwise = TRUE))),
    matrix(rep(c("left", "center", "right"), 2), 3, 2)
  )

  test_map(by_ranges(c(1.85, 2.05), c("left", "centre", "right")), "lllcrl", 1:3, 2)

  skip_if_not_installed("dplyr")

  test_map(by_cases(. == "Plum" ~ "centre", grepl("berry", .) ~ "right"),
        "rrclll")

  skip_if_not_installed("scales")

  expect_silent(ht2 <- map_text_color(ht, by_colorspace("red", "yellow", na_color = "green")))
  expect_equivalent(text_color(ht2)[, 1], rep("green", 3))
  expect_silent(col2rgb(text_color(ht2)))

  expect_equivalent(
    text_color(map_text_color(ht_2col, by_colorspace("red", "white", "blue", colwise = TRUE))),
    matrix(rep(c("#FF0000", "#FFFFFF", "#0000FF"), 2), 3, 2)
  )
})


test_that("standard ways to mention columns work", {
  ht <- hux(a = 1:3, b = 1:3, add_colnames = TRUE)

  br <- by_rows("left", "centre", "right")
  ht2 <- map_align(ht, 1:3, 1, br)
  expect_equivalent(ht2, map_align(ht, 1:3, "a", br))

  skip_if_not_installed("dplyr")
  expect_equivalent(ht2, map_align(ht, 1:3, dplyr::matches("a"), br))
  expect_equivalent(ht2, map_align(ht, everywhere, dplyr::matches("a"), br))
  expect_equivalent(ht2, map_align(ht, everywhere, dplyr::starts_with("a"), br))
  expect_equivalent(ht2, map_align(ht, everywhere, -2, br))
  expect_equivalent(ht2, map_align(ht, everywhere, odds, br))

  ht3 <- map_align(ht, c(1, 3), 1:2, br)
  expect_equivalent(ht3, map_align(ht, -2, 1:2, br))
  expect_equivalent(ht3, map_align(ht, odds, 1:2, br))
})


test_that("map_all_*", {
  # we include the NAs because we don't make guarantees about
  # what happens when borders overlap!
  m <- matrix(c(1, NA, 2, NA, NA, NA, 2, NA, 1), 3, 3)
  ht <- as_huxtable(m)
  m1 <- ! is.na(m) & m == 1
  m2 <- ! is.na(m) & m == 2

  ht2 <- map_all_borders(ht, by_ranges(1.5, c(1, 2)))
  expect_true(all(left_border(ht2)[m1] == 1))
  expect_true(all(right_border(ht2)[m1] == 1))
  expect_true(all(top_border(ht2)[m1] == 1))
  expect_true(all(bottom_border(ht2)[m1] == 1))
  expect_true(all(left_border(ht2)[m2] == 2))
  expect_true(all(right_border(ht2)[m2] == 2))
  expect_true(all(top_border(ht2)[m2] == 2))
  expect_true(all(bottom_border(ht2)[m2] == 2))

  ht3 <- map_all_border_colors(ht, by_ranges(1.5, c("red", "black")))
  expect_true(all(left_border_color(ht3)[m1]   == "red"))
  expect_true(all(right_border_color(ht3)[m1]  == "red"))
  expect_true(all(top_border_color(ht3)[m1]    == "red"))
  expect_true(all(bottom_border_color(ht3)[m1] == "red"))
  expect_true(all(left_border_color(ht3)[m2]   == "black"))
  expect_true(all(right_border_color(ht3)[m2]  == "black"))
  expect_true(all(top_border_color(ht3)[m2]    == "black"))
  expect_true(all(bottom_border_color(ht3)[m2] == "black"))


  ht4 <- map_all_border_styles(ht, by_ranges(1.5, c("solid", "double")))
  expect_true(all(left_border_style(ht4)[m1]   == "solid"))
  expect_true(all(right_border_style(ht4)[m1]  == "solid"))
  expect_true(all(top_border_style(ht4)[m1]    == "solid"))
  expect_true(all(bottom_border_style(ht4)[m1] == "solid"))
  expect_true(all(left_border_style(ht4)[m2]   == "double"))
  expect_true(all(right_border_style(ht4)[m2]  == "double"))
  expect_true(all(top_border_style(ht4)[m2]    == "double"))
  expect_true(all(bottom_border_style(ht4)[m2] == "double"))

  ht5 <- map_all_padding(ht, by_ranges(3, c(0, 10)))
  expect_equivalent(left_padding(ht5), 10 * (as.matrix(ht) >= 3))
  expect_equivalent(right_padding(ht5), 10 * (as.matrix(ht) >= 3))

  ht6 <- map_all_borders(ht, 1:3, 1, by_ranges(1.5, c(1, 2)))
  expect_equivalent(left_border(ht6)[1:3, 1], c(1, 0, 2))
  expect_equivalent(left_border(ht6)[1:3, 2], c(0, 0, 0))
})


test_that("map_lr/tb_*", {
  ht <- huxtable(1:5, 5:1, add_colnames = FALSE)
  ht2 <- map_lr_border_styles(ht, by_ranges(3, c("solid", "double")))
  expected <- matrix(ifelse(as.matrix(ht) >= 3, "double", "solid"), 5, 2)
  expect_equivalent(left_border_style(ht2), expected)
  expect_equivalent(right_border_style(ht2), expected)
  expect_equivalent(top_border_style(ht2), matrix("solid", 5, 2))
  expect_equivalent(bottom_border_style(ht2), matrix("solid", 5, 2))

  ht3 <- map_tb_border_styles(ht, by_ranges(3, c("solid", "double")))
  expected <- matrix(ifelse(as.matrix(ht) >= 3, "double", "solid"), 5, 2)
  expect_equivalent(left_border_style(ht3), matrix("solid", 5, 2))
  expect_equivalent(right_border_style(ht3), matrix("solid", 5, 2))
  expect_equivalent(top_border_style(ht3), expected)
  expect_equivalent(bottom_border_style(ht3), expected)

})

