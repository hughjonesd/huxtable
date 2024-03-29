
local_edition(2)

test_that("column_to_header works", {
  expect_silent(column_to_header(jams, "Price"))
  expect_silent(column_to_header(jams, "Type"))
  expect_silent(column_to_header(jams, 1))
})


test_that("column_to_header arguments work", {
  expect_silent(jams2 <- column_to_header(jams, 1, italic = TRUE))
  expect_equivalent(italic(jams2), matrix(c(F, T, F, T, F, T, F), ncol = 1))
  expect_equivalent(header_rows(jams2), c(T, T, F, T, F, T, F))

  expect_silent(jams3 <- column_to_header(jams, "Type", glue = "Type: {value}"))
  expect_equivalent(contents(jams3)[[2, 1]], "Type: Strawberry")

  expect_silent(jams4 <- column_to_header(jams, 1, set_headers = FALSE))
  expect_equivalent(header_rows(jams4), c(TRUE, rep(FALSE, 6)))

  expect_silent(jams5 <- column_to_header(jams, 1, ignore_headers = FALSE))
  expect_equivalent(contents(jams5)[1, 1], "Type")

  iris_hux <- as_hux(iris, add_colnames = FALSE)[c(1:3, 51:53, 101:103),]
  expect_silent(iris_hux <- column_to_header(iris_hux, "Species", start_col = 3))
  expect_equivalent(contents(iris_hux)[1, 3], "setosa")
  expect_equivalent(colspan(iris_hux)[1, 3], 2)
})
