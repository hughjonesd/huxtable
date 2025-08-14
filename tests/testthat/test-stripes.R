local_edition(3)


test_that("stripe_rows", {
  jams <- stripe_rows(jams)
  expect_equal(background_color(jams)[1, 1], "white", ignore_attr = TRUE)
  expect_equal(background_color(jams)[2, 1], "grey90", ignore_attr = TRUE)
  jams <- stripe_rows(jams, "red", "blue")
  expect_equal(background_color(jams)[1, 1], "red", ignore_attr = TRUE)
  expect_equal(background_color(jams)[2, 1], "blue", ignore_attr = TRUE)
})


test_that("stripe_columns", {
  jams <- stripe_columns(jams)
  expect_equal(background_color(jams)[1, 1], "white", ignore_attr = TRUE)
  expect_equal(background_color(jams)[1, 2], "grey90", ignore_attr = TRUE)
  jams <- stripe_columns(jams, "red", "blue")
  expect_equal(background_color(jams)[1, 1], "red", ignore_attr = TRUE)
  expect_equal(background_color(jams)[1, 2], "blue", ignore_attr = TRUE)
})
