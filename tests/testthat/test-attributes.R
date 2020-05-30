
ht <- huxtable(a = 1:5, b = letters[1:5], d = 1:5)


test_that("Can refer to properties by colnames", {
  ht <- huxtable(a = 1:5, b = letters[1:5], d = 1:5)
  number_format(ht)[1, 1] <- 3
  col_width(ht) <- c(.2, .6, .2)
  row_height(ht) <- rep(.2, 5)
  expect_equal(number_format(ht)[1, "a"], list(3))
  expect_equivalent(col_width(ht)["a"], .2)
})


test_that("Assignment to attributes preserves colnames", {
  ht <- huxtable(a = 1:5, b = letters[1:5], d = 1:5)
  cn <- colnames(align(ht))
  align(ht) <- "right"
  expect_equal(cn, colnames(align(ht)))
  align(ht)[1, 1] <- "left"
  expect_equal(cn, colnames(align(ht)))
})


test_that("Can assign numeric to width, col_width etc. after assigning character", {
  ht <- huxtable(letters[1:3])
  width(ht) <- "300pt"
  width(ht) <- 0.5
  expect_type(width(ht), "double")
  row_height(ht) <- paste0(1:3, "em")
  row_height(ht) <- rep(1 / 3, 3)
  expect_type(row_height(ht), "double")
  number_format(ht) <- "%.3f"
  number_format(ht) <- 2L
  nf <- number_format(ht)
  expect_type(nf[1, 1][[1]], "integer")
  expect_equivalent(dim(nf), dim(ht))
})


test_that("na_string works", {
  ht <- huxtable(a = c(1, 2, NA), b = c(NA, 1, 2))
  na_string(ht) <- "foo"
  na_string(ht)[3, 1] <- "bar"
  expect_silent(cc <- huxtable:::clean_contents(ht))

  expect_match(cc[1, 2], "foo")
  expect_match(cc[3, 1], "bar")
})

test_that("Can combine numbers and characters in number_format", {
  ht <- huxtable(a = c(1.11111, 1.11111, 1.11111, 1.11111), autoformat = FALSE)
  number_format(ht)[1, ] <- "%3.3f"
  number_format(ht)[2, ] <- 1
  number_format(ht)[3, ] <- list(function(x) ifelse(x > 0, "+", "-"))
  number_format(ht)[4, ] <- NA
  expect_equivalent(huxtable:::clean_contents(ht, "latex")[1, 1], "1.111")
  expect_equivalent(huxtable:::clean_contents(ht, "latex")[2, 1], "1.1")
  expect_equivalent(huxtable:::clean_contents(ht, "latex")[3, 1], "+")
  expect_equivalent(huxtable:::clean_contents(ht, "latex")[4, 1], "1.11111")
})


test_that("number_format works on cells with multiple numbers", {
  ht <- huxtable(a = "1 2.3556, some text; -33 -44.8908")
  number_format(ht)[1, 1] <- 1
  expect_equivalent(huxtable:::clean_contents(ht, "latex")[1, 1], "1.0 2.4, some text; -33.0 -44.9")
  number_format(ht)[1, 1] <- "%3.3f"
  expect_equivalent(huxtable:::clean_contents(ht, "latex")[1, 1], "1.000 2.356, some text; -33.000 -44.891")
  number_format(ht)[1, 1] <- list(function(x) ifelse(x > 0, "+", "-"))
  expect_equivalent(huxtable:::clean_contents(ht, "latex")[1, 1], "+ +, some text; - -")
})


test_that("number_format treats scientific notation equivalently to sprintf", {
  ht <- huxtable(c("1.12e3", "1.12E3", "1.12e7", "1.12e-3", "1.12A3", "1.12e3 4.8 and 5.6"))
  number_format(ht) <- 4
  expect_equivalent(huxtable:::clean_contents(ht, "latex")[1, 1], "1120.0000")
  expect_equivalent(huxtable:::clean_contents(ht, "latex")[2, 1], "1120.0000")
  expect_equivalent(huxtable:::clean_contents(ht, "latex")[3, 1],
                    "11200000.0000")
  expect_equivalent(huxtable:::clean_contents(ht, "latex")[4, 1], "0.0011")
  # the next is not scientific notation so both numbers should be affected
  expect_equivalent(huxtable:::clean_contents(ht, "latex")[5, 1], "1.1200A3.0000")
  expect_equivalent(huxtable:::clean_contents(ht, "latex")[6, 1], "1120.0000 4.8000 and 5.6000")
})


test_that("number_format works with various interesting cases", {
  expect_equivalent(huxtable:::format_numbers("1.1234", "%.3f"), "1.123")
  expect_equivalent(huxtable:::format_numbers("1", "%.3f"), "1.000")
  expect_equivalent(huxtable:::format_numbers("1 2 3", "%.3f"), "1.000 2.000 3.000")
  expect_equivalent(huxtable:::format_numbers("1 -2 -3.1 -.4 .5", "%.3f"), "1.000 -2.000 -3.100 -0.400 0.500")
  expect_equivalent(huxtable:::format_numbers("1.1234-1.1234", "%.3f"), "1.123-1.123")
  expect_equivalent(huxtable:::format_numbers("1.1234e-2", "%.3f"), "0.011")
  expect_equivalent(huxtable:::format_numbers("1.1234e-12", "%.3f"), "0.000")
  expect_equivalent(huxtable:::format_numbers("1.1234e12", "%.3f"), "1123400000000.000")
  # Make sure user can actually request scientific notation if desired
  # ("e" format) or get them as needed ("g" format)
  expect_equivalent(huxtable:::format_numbers("1.1234e12", "%.3g"), "1.12e+12")
  expect_equivalent(huxtable:::format_numbers("1.1234e8 3", "%.3f"),
                    "112340000.000 3.000")
  expect_equivalent(huxtable:::format_numbers("1.1234e8 3", "%.3g"),
                    "1.12e+08 3")
  expect_equivalent(huxtable:::format_numbers("1.1234e8 3", "%.1e"),
                    "1.1e+08 3.0e+00")
  # this is pretty brutal:
  expect_equivalent(huxtable:::format_numbers("-1.1e3-1.2e3", "%.3f"), "-1100.000-1200.000")
  expect_equivalent(huxtable:::format_numbers("-1.1e-3-1.2e3", "%.3f"), "-0.001-1200.000")
  # Signed zeroes
  expect_equivalent(huxtable:::format_numbers("-1.1e-3", "%.1f"), "-0.0")
  expect_equivalent(huxtable:::format_numbers("-1.1e-3", "%.1g"), "-0.001")
  expect_equivalent(huxtable:::format_numbers("-1.1e-3", 1), "-0.0")

})


test_that("Decimal padding works", {
  expect_identical(
          huxtable:::decimal_pad(
            c("do not pad.", "1.00532", "33", "33.6 *"),
            c(NA, rep(".", 3)),
            type = "screen"
          ),
          c("do not pad.", "1.00532", "33\u00a0\u00a0\u00a0\u00a0\u00a0\u00a0", "33.6 *\u00a0\u00a0")
        )
  # the characters are non-breaking spaces
})


test_that("Can pad with align", {
  ht <- hux(a = c("1.5", "2.5"))
  ht2 <- ht
  expect_silent(align(ht) <- ".")
  expect_identical(huxtable:::clean_contents(ht), huxtable:::clean_contents(ht2))
})


test_that("Can set attributes to NA", {
  ht <- huxtable(a = 1:3, b = 1:3)
  # expect no error
  expect_error(caption(ht) <- NA, regexp = NA)
  expect_error(font(ht) <- NA, regexp = NA)
  expect_error(col_width(ht) <- NA, regexp = NA)
})


test_that("set_default_properties", {
  old <- set_default_properties(bold = TRUE)
  expect_equivalent(bold(hux(a = 1)), matrix(TRUE, 1, 1))
  set_default_properties(old)
  expect_equivalent(bold(hux(a = 1)), matrix(FALSE, 1, 1))

  expect_error(set_default_properties(unknown = 1))
})


test_that("get_default_properties", {
  expect_equivalent(get_default_properties("bold"), FALSE)
  expect_error(get_default_properties("unknown"))
})


test_that("collapsed_border_colors works", {
  ht <- hux(a = 1:2, b = 1:2)
  left_border_color(ht)[1, 2] <- "pink"
  top_border_color(ht)[2, 1] <- "green"
  cbc <- huxtable:::collapsed_border_colors(ht)
  expect_type(cbc, "list")
  expect_equivalent(cbc$vert, matrix(c(NA, "pink", NA, NA, NA, NA), 2, 3, byrow = TRUE))
  expect_equivalent(cbc$horiz, matrix(c(NA, NA, "green", NA, NA, NA), 3, 2, byrow = TRUE))
  right_border_color(ht)[1, 1] <- "blue" # overrides
  bottom_border_color(ht)[1, 1] <- "purple"
  cbc <- huxtable:::collapsed_border_colors(ht)
  expect_equivalent(cbc$vert, matrix(c(NA, "blue", NA, NA, NA, NA), 2, 3, byrow = TRUE))
  expect_equivalent(cbc$horiz, matrix(c(NA, NA, "purple", NA, NA, NA), 3, 2, byrow = TRUE))
})


test_that("collapsed_border_styles works", {
  ht <- hux(a = 1:2, b = 1:2)
  left_border_style(ht)[1, 2] <- "dashed"
  top_border_style(ht)[2, 1] <- "double"
  cbs <- huxtable:::collapsed_border_styles(ht)
  expect_type(cbs, "list")
  vert <- matrix("solid", 2, 3)
  vert[1, 2] <- "dashed"
  horiz <- matrix("solid", 3, 2)
  horiz[2, 1] <- "double"
  expect_equivalent(cbs$vert, vert)
  expect_equivalent(cbs$horiz, horiz)
})


test_that("align, position and caption_pos change \"centre\" to \"center\"", {
  ht <- hux(1)
  align(ht) <- "centre"
  expect_equivalent(align(ht), matrix("center", 1, 1))

  position(ht) <- "centre"
  expect_equivalent(position(ht), "center")
  caption_pos(ht) <- "topcentre"
  expect_equivalent(caption_pos(ht), "topcenter")
  caption_pos(ht) <- "bottomcentre"
  expect_equivalent(caption_pos(ht), "bottomcenter")
})


test_that("rowspan and colspan error when overlapping", {
  ht <- hux(1:4, 1:4)
  rowspan(ht)[1, 2] <- 3
  expect_error(rowspan(ht)[2, 2] <- 2)
  expect_error(rowspan(ht)[3, 2] <- 2)
  expect_error(colspan(ht)[1, 1] <- 2)
  expect_error(colspan(ht)[1, 2] <- 2)
})
