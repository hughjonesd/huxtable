test_that("header rows wrapped in thead when at top", {
  ht <- hux(matrix(c("a", "b", "c", "d"), 2, 2), add_colnames = FALSE)
  header_rows(ht)[1] <- TRUE
  html <- to_html(ht)
  expect_true(grepl("<thead>", html, fixed = TRUE))
  expect_true(grepl("<tbody>", html, fixed = TRUE))
  expect_true(grepl("<th", html))
})

test_that("thead/tbody omitted when header rows not at top", {
  ht <- hux(matrix(1:6, 3, 2), add_colnames = FALSE)
  header_rows(ht)[c(1, 3)] <- TRUE
  html <- to_html(ht)
  expect_false(grepl("<thead>", html, fixed = TRUE))
  expect_false(grepl("<tbody>", html, fixed = TRUE))
  expect_true(grepl("<th", html))
})
