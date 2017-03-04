library(testthat)
library(huxtable)

expect_examples_unchanged <- function(fname) {
  extest <- example(fname, give.lines = TRUE, character.only = TRUE, run.dontrun = FALSE, run.donttest = FALSE)
  excode <- parse(text = extest)
  exenv <- new.env()
  exres <- list()
  i <- 1
  for (expr in excode) {
    res <- eval(expr, envir = exenv)
    rds_name <- paste0(fname, '-example-output-', i, '.rds')
    fp <- file.path(test_path(), rds_name)
    expect_equal_to_reference(res, fp)
    i <- i + 1
  }
}

test_check("huxtable")
