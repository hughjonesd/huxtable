local_edition(3)


setup({
  oo_lm <<- options(huxtable.long_minus = FALSE)
})


teardown({
  options(oo_lm)
})


test_that("Can combine numbers and characters in number_format", {
  ht <- huxtable(a = c(1.11111, 1.11111, 1.11111, 1.11111), autoformat = FALSE, add_colnames = FALSE)
  number_format(ht)[1, ] <- "%3.3f"
  number_format(ht)[2, ] <- 1
  number_format(ht)[3, ] <- list(function(x) ifelse(x > 0, "+", "-"))
  number_format(ht)[4, ] <- NA
  expect_equal(huxtable:::clean_contents(ht, "latex")[1, 1], "1.111", check.attributes = FALSE)
  expect_equal(huxtable:::clean_contents(ht, "latex")[2, 1], "1.1", check.attributes = FALSE)
  expect_equal(huxtable:::clean_contents(ht, "latex")[3, 1], "+", check.attributes = FALSE)
  expect_equal(huxtable:::clean_contents(ht, "latex")[4, 1], "1.11111", check.attributes = FALSE)
})


test_that("number_format works on cells with multiple numbers", {
  ht <- huxtable(a = "1 2.3556, some text; -33 -44.8908", add_colnames = FALSE)
  number_format(ht)[1, 1] <- 1
  expect_equal(huxtable:::clean_contents(ht, "latex")[1, 1], "1.0 2.4, some text; -33.0 -44.9", check.attributes = FALSE)
  number_format(ht)[1, 1] <- "%3.3f"
  expect_equal(huxtable:::clean_contents(ht, "latex")[1, 1], "1.000 2.356, some text; -33.000 -44.891", check.attributes = FALSE)
  number_format(ht)[1, 1] <- list(function(x) ifelse(x > 0, "+", "-"))
  expect_equal(huxtable:::clean_contents(ht, "latex")[1, 1], "+ +, some text; - -", check.attributes = FALSE)
})


test_that("number_format treats scientific notation equivalently to sprintf", {
  ht <- huxtable(c("1.12e3", "1.12E3", "1.12e7", "1.12e-3", "1.12A3", "1.12e3 4.8 and 5.6"), add_colnames = FALSE)
  number_format(ht) <- 4
  expect_equal(huxtable:::clean_contents(ht, "latex")[1, 1], "1120.0000", check.attributes = FALSE)
  expect_equal(huxtable:::clean_contents(ht, "latex")[2, 1], "1120.0000", check.attributes = FALSE)
  expect_equal(
    huxtable:::clean_contents(ht, "latex")[3, 1],
    "11200000.0000",
    check.attributes = FALSE
  )
  expect_equal(huxtable:::clean_contents(ht, "latex")[4, 1], "0.0011", check.attributes = FALSE)
  # the next is not scientific notation so both numbers should be affected
  expect_equal(huxtable:::clean_contents(ht, "latex")[5, 1], "1.1200A3.0000", check.attributes = FALSE)
  expect_equal(huxtable:::clean_contents(ht, "latex")[6, 1], "1120.0000 4.8000 and 5.6000", check.attributes = FALSE)
})


test_that("number_format works with various interesting cases", {
  expect_equal(
    huxtable:::format_numbers("1.1234", "%.3f"),
    "1.123",
    check.attributes = FALSE
  )
  expect_equal(
    huxtable:::format_numbers("1", "%.3f"),
    "1.000",
    check.attributes = FALSE
  )
  expect_equal(
    huxtable:::format_numbers("1 2 3", "%.3f"),
    "1.000 2.000 3.000",
    check.attributes = FALSE
  )
  expect_equal(
    huxtable:::format_numbers("1 -2 -3.1 -.4 .5", "%.3f"),
    "1.000 -2.000 -3.100 -0.400 0.500",
    check.attributes = FALSE
  )
  expect_equal(
    huxtable:::format_numbers("1.1234-1.1234", "%.3f"),
    "1.123-1.123",
    check.attributes = FALSE
  )
  expect_equal(
    huxtable:::format_numbers("1.1234e-2", "%.3f"),
    "0.011",
    check.attributes = FALSE
  )
  expect_equal(
    huxtable:::format_numbers("1.1234e-12", "%.3f"),
    "0.000",
    check.attributes = FALSE
  )
  expect_equal(
    huxtable:::format_numbers("1.1234e12", "%.3f"),
    "1123400000000.000",
    check.attributes = FALSE
  )
  # Make sure user can actually request scientific notation if desired
  # ("e" format) or get them as needed ("g" format)
  expect_equal(
    huxtable:::format_numbers("1.1234e12", "%.3g"),
    "1.12e+12",
    check.attributes = FALSE
  )
  expect_equal(
    huxtable:::format_numbers("1.1234e8 3", "%.3f"),
    "112340000.000 3.000",
    check.attributes = FALSE
  )
  expect_equal(
    huxtable:::format_numbers("1.1234e8 3", "%.3g"),
    "1.12e+08 3",
    check.attributes = FALSE
  )
  expect_equal(
    huxtable:::format_numbers("1.1234e8 3", "%.1e"),
    "1.1e+08 3.0e+00",
    check.attributes = FALSE
  )
  # this is pretty brutal:
  expect_equal(
    huxtable:::format_numbers("-1.1e3-1.2e3", "%.3f"),
    "-1100.000-1200.000",
    check.attributes = FALSE
  )
  expect_equal(
    huxtable:::format_numbers("-1.1e-3-1.2e3", "%.3f"),
    "-0.001-1200.000",
    check.attributes = FALSE
  )
  # Signed zeroes
  expect_equal(
    huxtable:::format_numbers("-1.1e-3", "%.1f"),
    "-0.0",
    check.attributes = FALSE
  )
  expect_equal(
    huxtable:::format_numbers("-1.1e-3", "%.1g"),
    "-0.001",
    check.attributes = FALSE
  )
  expect_equal(
    huxtable:::format_numbers("-1.1e-3", 1),
    "-0.0",
    check.attributes = FALSE
  )
})


test_that("long_minus", {
  oo_lm_local <- options(huxtable.long_minus = TRUE)
  on.exit(options(oo_lm_local))

  ht <- huxtable("-3.1415")
  expect_match(to_screen(ht), "\u22123.1415")

  options(huxtable.long_minus = FALSE)
})


test_that("Decimal padding works", {
  expect_identical(
    huxtable:::handle_decimal_alignment(
      c("do not pad.", "1.00532", "33", "33.6 *"),
      c(NA, rep(".", 3)),
      output_type = "screen"
    ),
    c("do not pad.", "1.00532", "33\u00a0\u00a0\u00a0\u00a0\u00a0\u00a0", "33.6 *\u00a0\u00a0")
  )
  # the characters are non-breaking spaces
})


test_that("Number formatting with non-standard decimal point", {
  skip("Just too hard to do at the moment.")

  oo_od_local <- options(OutDec = ",")
  on.exit(options(oo_od_local))

  # We set a name to avoid matching "Column names: 1,51"
  ht <- huxtable(a = 1.51)
  number_format(ht) <- "%.2f"
  expect_match(
    to_screen(ht),
    "1,51"
  )
})
