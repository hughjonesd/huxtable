
test_that("fmt_percent", {
  ht <- hux(c(0.25, 0.75))
  expect_silent(number_format(ht) <- fmt_percent(1))
  expect_match(to_screen(ht), "25.0%")
  expect_silent(number_format(ht)[2, ] <- fmt_percent(2))
  expect_match(to_screen(ht), "25.0%")
  expect_match(to_screen(ht), "75.00%")
})


test_that("fmt_pretty", {
  ht <- hux(1:3 * 1e5)
  expect_silent(number_format(ht) <- fmt_pretty())
  expect_match(to_screen(ht), "100,000")
  expect_silent(number_format(ht)[2,] <- fmt_pretty(big.mark = "_"))
  expect_match(to_screen(ht), "100,000")
  expect_match(to_screen(ht), "200_000")
  expect_silent(number_format(ht)[3,] <- 2)
  expect_match(to_screen(ht), "100,000")
  expect_match(to_screen(ht), "200_000")
  expect_match(to_screen(ht), "300000.00")
})

