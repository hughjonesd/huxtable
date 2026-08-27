test_that("as_html returns htmltools tag", {
  ht <- hux(a = 1:2, b = 3:4)
  tag <- as_html(ht)
  expect_s3_class(tag, "html")
  expect_identical(as.character(tag), to_html(ht, dependencies = FALSE))
  expect_s3_class(htmltools::htmlDependencies(tag)[[1]], "html_dependency")
})
