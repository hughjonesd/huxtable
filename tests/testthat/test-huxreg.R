

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
