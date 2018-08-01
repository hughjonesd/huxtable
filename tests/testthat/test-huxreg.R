

context('huxreg')
skip_if_not_installed('broom')


test_that('huxreg examples unchanged', {
  test_ex_same('huxreg')
})


test_that('has_builtin_ci works', {
  skip_if_not_installed('nlme')

  lm1 <- lm(Sepal.Width ~ Sepal.Length, iris)
  expect_true(huxtable:::has_builtin_ci(lm1))
  library(nlme)
  data(Orthodont, package = 'nlme')
  fm1 <- nlme::lme(distance ~ age + Sex, data = Orthodont, random = ~ 1, method = 'ML')
  expect_false(huxtable:::has_builtin_ci(fm1))
})


test_that('huxreg copes with different models', {
  set.seed(27101975)
  dfr <- data.frame(a = rnorm(100), b = rnorm(100))
  dfr$y <- dfr$a + rnorm(100)
  lm1 <- lm(y ~ a, dfr)
  lm2 <- lm(y ~ a + b, dfr)
  glm1 <- glm(I(y > 0) ~ a, dfr, family = binomial)
  expect_silent(hr <- huxreg(lm1, lm2, glm1))
})


test_that('huxreg confidence intervals work', {
  skip_if_not_installed('nnet')

  set.seed(27101975)
  dfr <- data.frame(a = rnorm(100), b = rnorm(100))
  dfr$y <- dfr$a + rnorm(100)
  lm1 <- lm(y ~ a, dfr)
  lm2 <- lm(y ~ a + b, dfr)
  glm1 <- glm(I(y > 0) ~ a, dfr, family = binomial)
  library(nnet)
  mn <- nnet::multinom(I(y > 0) ~ a, dfr, trace = FALSE)
  expect_silent(huxreg(lm1, lm2, glm1, mn, error_format = "{conf.low}-{conf.high}",
        statistics = c('r.squared'), ci_level = 0.95))
})

test_that('huxreg confidence intervals work when tidy c.i.s not available', {
  skip_if_not_installed('nlme')
  if (packageVersion('broom') >= '0.7.0') skip_if_not_installed('broom.mixed')

  set.seed(27101975)
  data(Orthodont, package = 'nlme')
  # method ML avoids a warning in broom::glance
  fm1 <- nlme::lme(distance ~ age + Sex, data = Orthodont, random = ~ 1, method = 'ML')
  expect_silent(
          huxreg(fm1, tidy_args = list(effects = 'fixed'), statistics = 'nobs', ci_level = 0.95,
          error_format = '({conf.low}-{conf.high})')
        )

})


test_that('huxreg error_style usage', {
  lm1 <- lm(Sepal.Width ~ Sepal.Length, iris)
  expect_warning(hr <- huxreg(lm1, error_style = 'stderr'), '`error_style` is deprecated')
  hr2 <- huxreg(lm1, error_format = '({std.error})')
  expect_identical(hr, hr2)
})


test_that('huxreg works with single coefficient', {
  set.seed(27101975)
  dfr <- data.frame(a = rnorm(100), b = rnorm(100))
  dfr$y <- dfr$a + rnorm(100)
  lm1 <- lm(y ~ a, dfr)
  lm2 <- lm(y ~ a + b, dfr)
  expect_error(huxreg(lm1, lm2, coefs = 'a'), regexp = NA)
})


test_that('huxreg merges coefficients with same names', {
  set.seed(27101975)
  dfr <- data.frame(y = rnorm(100), a = rnorm(100), b = rnorm(100), d = rnorm(100))
  lm1 <- lm(y ~ a, dfr)
  lm2 <- lm(y ~ b, dfr)
  ht <- huxreg(lm1, lm2, coefs = c('name' = 'a', 'name' = 'b'))
  expect_equal(sum(ht[[1]] == 'name'), 1)
  lm3 <- lm(y ~ a + d, dfr)
  lm4 <- lm(y ~ b + d, dfr)
  ht2 <- huxreg(lm3, lm4, coefs = c('name' = 'a', 'name' = 'b', 'd'))
  expect_equal(sum(ht2[[1]] == 'name'), 1)
})


test_that('huxreg bold_signif works', {
  lm1 <- lm(Petal.Length ~ Sepal.Length, iris)
  expect_silent(hr1 <- huxreg(lm1, bold_signif = 0.05))
  expect_identical(unname(bold(hr1)), matrix(c(rep(FALSE, 11), rep(TRUE, 4), rep(FALSE, 5)), 10, 2))
})


test_that('huxreg error_pos works', {
  lm1 <- lm(Petal.Length ~ Sepal.Length, iris)
  lm2 <- lm(Sepal.Width ~ Sepal.Length, iris)
  expect_silent(hr1 <- huxreg(lm1, lm2, error_pos = 'right'))
  expect_equal(ncol(hr1), 5)
})


test_that('huxreg number_format works correctly', {
  set.seed(27101975)
  dfr <- data.frame(y = rnorm(100), a = rnorm(100), b = rnorm(100), d = rnorm(100))
  dfr$y <- dfr$y + dfr$a
  lm1 <- lm(y ~ a, dfr)
  lm2 <- lm(y ~ b, dfr)
  hr <- huxreg(lm1, lm2, number_format = 4)
  expect_equal(number_format(hr)[4, 2], list(4))
  expect_equal(number_format(hr)[9, 2], list(4))
  expect_match(to_screen(hr), paste0('\\D', round(coef(lm1)[2], 4), '\\D'))
  hr2 <- huxreg(lm1, lm2, number_format = '%5.3f')
  expect_equal(number_format(hr2)[4, 2], list('%5.3f'))
  expect_equal(number_format(hr2)[9, 2], list('%5.3f'))
  expect_match(to_screen(hr2), paste0('\\D', sprintf('%5.3f', coef(lm1)[2]), '\\D'))
})


test_that('huxreg borders argument works', {
  set.seed(27101975)
  dfr <- data.frame(y = rnorm(100), a = rnorm(100), b = rnorm(100), d = rnorm(100))
  dfr$y <- dfr$y + dfr$a
  lm1 <- lm(y ~ a, dfr)
  lm2 <- lm(y ~ b, dfr)
  hr <- huxreg(lm1, lm2, borders = .7, outer_borders = .8)
  expect_equivalent(unname(bottom_border(hr)[, 2]), c(.7, rep(0, 5), .7, rep(0, nrow(hr) - 9), .8, 0))
  expect_equivalent(unname(top_border(hr)[, 2]), c(.8, rep(0, 11)))
  hr2 <- huxreg(lm1, lm2, borders = 0, outer_borders = 0)
  expect_equivalent(unname(bottom_border(hr2)), matrix(0, nrow(hr2), ncol(hr2)))
  expect_equivalent(unname(top_border(hr2)), matrix(0, nrow(hr2), ncol(hr2)))
})


test_that('huxreg statistics names shown in output', {
  m <- lm(Sepal.Width ~ Sepal.Length, data = iris)
  expect_match(to_screen(huxreg(m, statistics = c(foo = 'nobs'))), 'foo')
})


test_that('huxreg stars printed correctly', {
  set.seed(27101975)
  dfr <- data.frame(y = rnorm(20), a = rnorm(20))
  dfr$y <- dfr$y + dfr$a + rnorm(20, 0, 4)
  dfr$z <- dfr$a + rnorm(20, 0, 1)
  lm1 <- lm(y ~ a, dfr)
  lm2 <- lm(z ~ a, dfr)
  number_regex <- '\\s*(\\d|\\.)+\\s*'
  expect_match(huxreg(lm1)[[4, 2]], number_regex)
  expect_match(huxreg(lm1, stars = c('@' = 0.1))[[4, 2]], paste0(number_regex, '@\\s*'))
  expect_match(huxreg(lm1, stars = c('@' = 0.1, 'wrong' = 0.05))[[4, 2]], paste0(number_regex, '@\\s*'))
  expect_match(huxreg(lm1, stars = c('wrong' = 0.05, '@' = 0.1))[[4, 2]], paste0(number_regex, '@\\s*'))
  expect_match(huxreg(lm2)[[4, 2]], paste0(number_regex, '\\*\\*\\*\\s*'))
})


test_that('huxreg works for models without tidy p values', {
  skip_if_not_installed('lme4')

  expect_warning(huxreg(lme4::lmer(Sepal.Width ~ Sepal.Length + (1 | Species), data = iris),
        statistics = 'nobs'), 'p values')
})


test_that('huxreg works when nobs not available', {
  skip_if_not_installed('lmtest')

  m <- lm(Sepal.Width ~ Sepal.Length, data = iris)
  ct <- lmtest::coeftest(m)
  expect_warning(huxreg(ct, statistics = NULL), 'No `glance` method')
})


test_that('huxreg column names are legitimate', {
  hr1 <- huxreg(lm(Sepal.Width ~ Sepal.Length, data = iris), lm(Sepal.Width ~ Sepal.Length, data = iris))
  cn <- colnames(hr1)

  expect_identical(cn, make.names(cn))
})


test_that('can pass broom::tidy arguments to huxreg', {
  lm1 <-  lm(Sepal.Width ~ Sepal.Length, data = iris)
  glm1 <- glm(I(Sepal.Width > 3) ~ Sepal.Length, data = iris, family = binomial)
  expect_silent(huxreg(glm1, tidy_args = list(exponentiate = TRUE), statistics = "nobs"))
  expect_silent(huxreg(lm1, glm1, tidy_args = list(list(), list(exponentiate = TRUE)), statistics = "nobs"))
  expect_silent(huxreg(lm1, glm1, tidy_args = list(exponentiate = FALSE), statistics = "nobs"))
})
