
context('mapping functions')


test_that('by_values', {
  m <- matrix(letters[1:4], 2, 2)
  ct <- matrix(NA, 2, 2)

  f <- by_values(a = 1, b = 2)
  expect_equivalent(f(m, 1:2, 1:2, ct), matrix(c(1, 2, NA, NA), 2, 2))
  expect_equivalent(f(m, 1, 1:2, ct[1, 1:2, drop = FALSE]), matrix(c(1, NA), 1, 2))
  expect_equivalent(f(m, 1:2, 1, ct[1:2, 1, drop = FALSE]), matrix(c(1, 2), 2, 1))
  expect_equivalent(f(m, 1, 1, ct[1, 1, drop = FALSE]), matrix(1, 1, 1))

  f <- by_values(a = 1, b = 2, 3)
  expect_equivalent(f(m, 1:2, 1:2, ct), matrix(c(1, 2, 3, 3), 2, 2))

  expect_error(by_values(a = 1, b = 2, 3, 4), 'unnamed')
})


test_that('by_rows/by_cols', {
  m <- matrix(NA, 2, 2)
  ct <- matrix(NA, 2, 2)

  f <- by_rows(1:2)
  expect_equivalent(f(m, 1:2, 1:2, ct), matrix(1:2, 2, 2))
  f <- by_rows(1:2, from = 2)
  expect_equivalent(f(m, 1:2, 1:2, ct), matrix(c(NA, 1, NA, 1), 2, 2))

  f <- by_cols(1:2)
  expect_equivalent(f(m, 1:2, 1:2, ct), matrix(1:2, 2, 2, byrow = TRUE))
  f <- by_cols(1:2, from = 2)
  expect_equivalent(f(m, 1:2, 1:2, ct), matrix(c(NA, 1, NA, 1), 2, 2, byrow = TRUE))
})

test_that('by_ranges', {
  m <- matrix(c(1, 3, 5, 7), 2, 2)
  ct <- matrix(NA, 2, 2)

  f <- by_ranges(breaks = c(2, 6), values = c('low', 'middle', 'high'))
  expect_equivalent(f(m, 1:2, 1:2, ct), matrix(c('low', 'middle', 'middle', 'high'), 2, 2))

  f <- by_ranges(breaks = c(2, 6), values = 'middle', extend = FALSE)
  expect_equivalent(f(m, 1:2, 1:2, ct), matrix(c(NA, 'middle', 'middle', NA), 2, 2))

  expect_error(by_ranges(breaks = c(2, 6), values = c('middle', 'extra'), extend = FALSE),
        'length')
  expect_error(by_ranges(breaks = c(2, 6), values = c('middle', 'notenough'), extend = TRUE),
        'length')

  f <- by_ranges(breaks = 3, values = c('low', 'high'), right = TRUE)
  expect_equivalent(f(m, 1:2, 1:2, ct), matrix(c('low', 'low', 'high', 'high'), 2, 2))
  f <- by_ranges(breaks = 3, values = c('low', 'high'), right = FALSE)
  expect_equivalent(f(m, 1:2, 1:2, ct), matrix(c('low', 'high', 'high', 'high'), 2, 2))
})


test_that('by_quantiles', {
  m <- matrix(1:4, 2, 2)
  ct <- matrix(NA, 2, 2)

  f <- by_quantiles(1:3/4, c('1st', '2nd', '3rd', '4th'))
  expect_equivalent(f(m, 1:2, 1:2, ct), matrix(c('1st', '2nd', '3rd', '4th'), 2, 2))

  f <- by_quantiles(1:3/4, c('2nd', '3rd'), extend = FALSE)
  expect_equivalent(f(m, 1:2, 1:2, ct), matrix(c(NA, '2nd', '3rd', NA), 2, 2))

  expect_error(by_quantiles(c(.5, .25, .75), rep('foo', 4)))
  expect_error(by_quantiles(-1, rep('foo', 2)))
  expect_error(by_quantiles(2, rep('foo', 2)))

  f <- by_equal_groups(4, c('1st', '2nd', '3rd', '4th'))
  expect_equivalent(f(m, 1:2, 1:2, ct), matrix(c('1st', '2nd', '3rd', '4th'), 2, 2))
})


test_that('by_regex', {
  m <- matrix(c('the cat', 'sat', 'on', 'THE MAT'), 2, 2)
  ct <- matrix(NA, 2, 2)

  f <- by_regex('at' = 1)
  expect_equivalent(f(m, 1:2, 1:2, ct), matrix(c(1, 1, NA, NA), 2, 2))

  f <- by_regex('at' = 1, .grepl_args = list(ignore.case = TRUE))
  expect_equivalent(f(m, 1:2, 1:2, ct), matrix(c(1, 1, NA, 1), 2, 2))

  f <- by_regex('at' = 1, .grepl_args = list(fixed = TRUE))
  expect_equivalent(f(m, 1:2, 1:2, ct), matrix(c(1, 1, NA, NA), 2, 2))

  f <- by_regex('t.*\\s.*at' = 1)
  expect_equivalent(f(m, 1:2, 1:2, ct), matrix(c(1, NA, NA, NA), 2, 2))
})


test_that('by_colorspace', {
  skip_if_not_installed('scales')

  m <- matrix(c(0, 0.3, 0.6, 1), 2, 2)
  ct <- matrix(NA, 2, 2)

  f <- by_colorspace('red', 'blue')
  myscale <- scales::col_numeric(c('red', 'blue'), domain = NULL)
  expected <- matrix(myscale(c(0, 0.3, 0.6, 1)), 2, 2)
  expect_equivalent(f(m, 1:2, 1:2, ct), expected)
})


test_that('by_function', {
  m <- matrix(1:4/4, 2, 2)
  ct <- matrix(NA, 2, 2)

  f <- by_function(grey)
  expect_equivalent(f(m, 1:2, 1:2, ct), matrix(grey(1:4/4), 2, 2))
})


test_that('by_cases', {
  skip_if_not_installed('dplyr')

  m <- matrix(1:6, 3, 2)
  ct <- matrix("default", 3, 2)

  f <- by_cases(. < 1.5 ~ "small", . == 2 ~ "two", . %in% 3:4 ~ "middle")
  expect_equivalent(f(m, 1:3, 1:2, ct), matrix(c("small", "two", "middle", "middle", "default",
        "default"), 3, 2))

  f <- by_cases(. < 1.5 ~ "small", . == 2 ~ "two", . %in% 3:4 ~ "middle", skip_na = FALSE)
  expect_equivalent(f(m, 1:3, 1:2, ct), matrix(c("small", "two", "middle", "middle", NA, NA), 3, 2))
})
