
# runs example and checks it hasn't changed
test_ex_same <- function(fname) {
  path <- devtools::package_file('man', paste0(fname, '.Rd'))
  expath <- tempfile('test-example')
  tools::Rd2ex(path, expath, commentDontrun = TRUE, commentDonttest = TRUE)
  excode <- parse(expath)
  exenv <- new.env()
  exres <- list()
  i <- 1
  for (expr in excode) {
    capture.output(res <- eval(expr, envir = exenv))
    rds_name <- paste0(fname, '-example-output-', i, '.rds')
    fp <- file.path(test_path(), rds_name)
    expect_equal_to_reference(res, fp, info = paste0('Changed expression: ', deparse(expr)))
    i <- i + 1
  }
}
