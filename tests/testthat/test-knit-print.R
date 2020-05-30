

skip_if_not_installed("knitr")


test_that("Can turn on and off printing of data frames", {
  # NB this all may fail if rmarkdown is loaded; rmarkdown defines knit_print.data.frame :-(
  expect_huxish <- function (dfr, bool) {
    oo <- options(huxtable.knitr_output_format = "screen")
    tmp <- capture.output(knitr::knit_print(dfr))
    expect_identical(any(grepl("Column names", tmp)), bool)
    options(oo)
  }
  # should be on by default, this tests that...
  dfr <- data.frame(a = 1:3, b = 1:3)
  expect_huxish(dfr, TRUE)
  oo <- options(huxtable.knit_print_df = FALSE)
  expect_huxish(dfr, FALSE)
  options(huxtable.knit_print_df = TRUE)
  expect_huxish(dfr, TRUE)
  options(oo)
})

test_that("Can theme data frames", {
  mytheme <- function (ht) {
    number_format(ht) <- 5
    ht
  }

  oo <- options(
        huxtable.knitr_output_format = "screen",
        huxtable.knit_print_df = TRUE,
        huxtable.knit_print_df_theme = mytheme)
  dfr <- data.frame(a = 1:2, b = 1:2)
  tmp <- knitr::knit_print(dfr)
  expect_true(any(grepl("\\.00000", tmp)))

  options(oo)
})


test_that("knitr_output_format overrides default output format in knit_print", {
  oo <- options(huxtable.knitr_output_format = "html")
  ht <- hux(a = 1)
  expect_match(knitr::knit_print(ht), "<table")
  options(huxtable.knitr_output_format = "latex")
  expect_match(knitr::knit_print(ht), "tabular", fixed = TRUE)
  options(huxtable.knitr_output_format = "md")
  expect_match(knitr::knit_print(ht), "---", fixed = TRUE)

  options(oo)
})


test_that("knit_print warns if output format is weird", {
  oo <- options(huxtable.knitr_output_format = "weirdness")
  ht <- hux(a = 1)
  expect_warning(knitr::knit_print(ht), "output format")

  options(oo)
})
