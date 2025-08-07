local_edition(2)


test_that("stripe_rows", {
  jams <- stripe_rows(jams)
  expect_equivalent(background_color(jams)[1, 1], "white")
  expect_equivalent(background_color(jams)[2, 1], "grey90")
  jams <- stripe_rows(jams, "red", "blue")
  expect_equivalent(background_color(jams)[1, 1], "red")
  expect_equivalent(background_color(jams)[2, 1], "blue")
})


test_that("stripe_columns", {
  jams <- stripe_columns(jams)
  expect_equivalent(background_color(jams)[1, 1], "white")
  expect_equivalent(background_color(jams)[1, 2], "grey90")
  jams <- stripe_columns(jams, "red", "blue")
  expect_equivalent(background_color(jams)[1, 1], "red")
  expect_equivalent(background_color(jams)[1, 2], "blue")
})
