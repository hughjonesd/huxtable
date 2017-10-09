

context('huxreg')

test_that('huxreg examples unchanged', {
  test_ex_same('huxreg')
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
  set.seed(27101975)
  dfr <- data.frame(a = rnorm(100), b = rnorm(100))
  dfr$y <- dfr$a + rnorm(100)
  lm1 <- lm(y ~ a, dfr)
  lm2 <- lm(y ~ a + b, dfr)
  glm1 <- glm(I(y > 0) ~ a, dfr, family = binomial)
  library(nnet)
  mn <- nnet::multinom(I(y > 0) ~ a, dfr, trace = FALSE)
  expect_silent(huxreg(lm1, lm2, glm1, mn, error_format = "{conf.low}-{conf.high}", statistics = c('r.squared'),
        ci_level = 0.95))
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
  hr <- huxreg(lm1, lm2, borders = .7)
  expect_equivalent(bottom_border(hr)[, 2], c(.7, rep(0, 5), .7, rep(0, nrow(hr) - 9), .7, 0))
  hr2 <- huxreg(lm1, lm2, borders = FALSE)
  expect_equivalent(bottom_border(hr2)[, 2], rep(0, nrow(hr2)))
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
  expect_warning(huxreg(lme4::lmer(Sepal.Width ~ Sepal.Length + (1 | Species), data = iris), statistics = 'nobs'),
        'p values')
})


test_that('huxreg column names are legitimate', {
  hr1 <- huxreg(lm(Sepal.Width ~ Sepal.Length, data = iris), lm(Sepal.Width ~ Sepal.Length, data = iris))
  cn <- colnames(hr1)

  expect_identical(cn, make.names(cn))
})
