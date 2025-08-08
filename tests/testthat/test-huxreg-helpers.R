local_edition(2)

if (!requireNamespace("broom", quietly = TRUE)) {
  testthat::skip("broom package is required for huxreg helper tests")
}

test_that("tidy_models adds confidence intervals", {
  lm1 <- lm(Sepal.Width ~ Sepal.Length, iris)
  tm <- huxtable:::tidy_models(list(lm1))
  expect_false("conf.low" %in% names(tm[[1]]))
  tm_ci <- huxtable:::tidy_models(list(lm1), ci_level = 0.95)
  expect_true(all(c("conf.low", "conf.high") %in% names(tm_ci[[1]])))
})

test_that("select_coefs filters coefficients", {
  lm1 <- lm(Sepal.Width ~ Sepal.Length, iris)
  tidied <- huxtable:::tidy_models(list(lm1))
  sel <- huxtable:::select_coefs(tidied, coefs = "Sepal.Length")
  expect_equal(sel$coef_names, "Sepal.Length")
  expect_equal(nrow(sel$tidied[[1]]), 1)
})

test_that("add_stars appends symbols", {
  lm1 <- lm(Sepal.Width ~ Sepal.Length, iris)
  tidied <- huxtable:::tidy_models(list(lm1))
  sel <- huxtable:::select_coefs(tidied)
  starred <- huxtable:::add_stars(sel$tidied, c("*" = 0.05))
  expect_match(starred[[1]]$estimate_star[1], "\\*")
})

test_that("aggregate_statistics collects nobs", {
  lm1 <- lm(Sepal.Width ~ Sepal.Length, iris)
  stats <- huxtable:::aggregate_statistics(list(lm1), statistics = c("nobs"))
  expect_equal(stats$stat_names, "nobs")
  expect_equal(as.numeric(stats$sumstats[1, 1]), nobs(lm1))
})
