
context('huxtable creation')


source('functions.R')


test_that('Object creation examples unchanged', {
  test_ex_same('huxtable')
})


test_that('create huxtable using hux[table]()', {
  expect_silent(ht <- huxtable(a = 1:3, b = 1:3))
  expect_silent(ht2 <- hux(a = 1:3, b = 1:3))
  expect_is(ht, 'huxtable')
  expect_equal(ncol(ht), 2)
  expect_equal(nrow(ht), 3)
  expect_identical(ht, ht2)
})


test_that('create huxtable from data frame', {
  dfr <- data.frame(a = 1:3, b = 1:3)
  expect_silent(ht <- as_hux(dfr))
  expect_silent(ht2 <- as_huxtable(dfr))
  expect_is(ht, 'huxtable')
  expect_identical(ht, huxtable(a = 1:3, b = 1:3))
  expect_identical(ht, ht2)
})


test_that('autoformat', {
  dfr <- data.frame(
          character = letters[1:3],
          integer   = 1:3,
          numeric   = c(1.5, 2.5, 3.5),
          complex   = 1:3 + 1i,
          Date      = as.Date(rep("2001/01/01", 3)),
          POSIXct   = as.POSIXct(Sys.time()),
          POSIXlt   = as.POSIXlt(Sys.time())
        )
  ht <- as_hux(dfr, autoformat = TRUE, add_colnames = TRUE)

  for (x in c("character", "Date", "POSIXct", "POSIXlt")) {
    expect_equivalent(number_format(ht)[2, x], NA)
  }
  for (x in c("numeric", "complex")) {
    expect_equivalent(number_format(ht)[2, x], "%.3g")
  }
  expect_equivalent(number_format(ht)[2, "integer"], 0)

  expect_equivalent(number_format(ht)[1, ], rep(NA, ncol(dfr)))

  for (x in c("integer", "Date", "POSIXct", "POSIXlt")) {
    # column heading alignment same:
    expect_equivalent(align(ht)[1:2, x], rep("right", 2))
  }
  for (x in c("numeric", "complex")) {
    expect_equivalent(align(ht)[2, x], getOption("OutDec"))
    # headings right aligned:
    expect_equivalent(align(ht)[1, x], "right")
  }
  expect_equivalent(align(ht)[1:2, "character"], rep("left", 2))
})


test_that('create huxtable from matrix', {
  m <- matrix(1:8, 4, 2)
  expect_silent(ht <- as_hux(m))
})


test_that('create huxtable from vector', {
  v <- letters[1:5]
  expect_silent(ht <- as_hux(v))
  expect_equal(nrow(ht), length(v))
  nv <- 1:5
  expect_silent(ht <- as_hux(nv))
  expect_equal(nrow(ht), length(nv))
})


test_that('create huxtable from table', {
  tbl <- table(mtcars$gear, mtcars$cyl)
  expect_silent(ht <- as_hux(tbl))
  expect_is(ht, 'huxtable')
  expect_equivalent(ht[[1]][ -1 ], rownames(tbl))
  expect_equivalent(unlist(ht[1, -1]), colnames(tbl))
  expect_equivalent(ht[[1, 1]], "") # check no "rownames" in top left
})


test_that('as_hux.table does not use number_format on rownames', {
  tbl <- table(c(3.5, 3.5, 4.5, 4.5), c(1.5, 1.5, 2.5, 2.5))
  ht <- as_hux(tbl)
  expect_match(to_screen(ht, colnames = FALSE), "3\\.5")
  expect_match(to_screen(ht, colnames = FALSE), "1\\.5")
})


test_that('create huxtable from ftable', {
  ft <- ftable(mtcars[c("cyl", "vs", "gear")])
  expect_silent(ht <- as_hux(ft))
  # the below tests current implementation, where we have separate rows/columns for column and
  # row name names ("dnn")
  expect_equal(nrow(ht), nrow(ft) + length(attr(ft, "col.vars")) + 1)
  expect_equal(ncol(ht), ncol(ft) + length(attr(ft, "row.vars")) + 1)
})
