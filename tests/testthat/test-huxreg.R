

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
  expect_silent(hr <- huxreg(lm1, lm2, glm1, mn, error_style = "ci", statistics = c('r.squared')))
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
