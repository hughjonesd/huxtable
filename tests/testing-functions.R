
# runs example and checks it hasn't changed
test_ex_same <- function(fname) {
  path <- devtools::find_topic(fname)
  if (is.null(path)) stop('No help exists for ', fname)
  expath <- tempfile('test-example')
  tools::Rd2ex(path, expath, commentDontrun = TRUE, commentDonttest = TRUE)
  if (! file.exists(expath)) stop('Could not make example for ', fname)
  excode <- parse(expath)
  exenv <- new.env()
  exres <- list()
  i <- 1
  for (expr in excode) {
    out <- capture.output(res <- eval(expr, envir = exenv))
    rds_name <- paste0(fname, '-example-value-', i, '.rds')
    fp <- file.path(test_path(), rds_name)
    expect_equal_to_reference(res, fp, info = paste0('Expression changed value: ', deparse(expr)))
    rds_name <- paste0(fname, '-example-output-', i, '.rds')
    fp <- file.path(test_path(), rds_name)
    expect_equal_to_reference(out, fp, info = paste0('Expression changed output: ', deparse(expr)))

    i <- i + 1
  }
}
