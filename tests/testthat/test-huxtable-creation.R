local_edition(3)


test_that("create huxtable using hux[table]()", {
  expect_silent(ht <- huxtable(a = 1:3, b = 1:3))
  expect_silent(ht2 <- hux(a = 1:3, b = 1:3))
  expect_s3_class(ht, "huxtable")
  expect_equal(ncol(ht), 2)
  expect_equal(nrow(ht), 4)  # Now includes header row
  expect_identical(ht, ht2)
})


test_that("add_colnames", {
  expect_silent(ht <- huxtable(a = 1:3, b = 1:3, add_colnames = TRUE))
  expect_equal(nrow(ht), 4)
  expect_silent(ht <- huxtable(a = 1:3, b = 1:3, add_colnames = FALSE))
  expect_equal(nrow(ht), 3)
})


test_that("add_rownames", {
  expect_silent(ht <- huxtable(a = 1:3, b = 1:3, add_rownames = TRUE))
  expect_equal(ncol(ht), 3)
  expect_equal(colnames(ht), c("rownames", "a", "b"))
  expect_silent(ht <- huxtable(a = 1:3, b = 1:3, add_rownames = FALSE))
  expect_equal(ncol(ht), 2)
  expect_silent(ht <- huxtable(a = 1:3, b = 1:3, add_rownames = "foo"))
  expect_equal(ncol(ht), 3)
  expect_equal(colnames(ht), c("foo", "a", "b"))
})


test_that("add_colnames with as_huxtable.matrix", {
  mat <- matrix(1:4, 2, 2, dimnames = list(letters[1:2], LETTERS[1:2]))
  ht <- as_hux(mat, add_colnames = TRUE, add_rownames = TRUE)
  expect_equal(as.character(ht[1, 2:3]), colnames(mat))
  expect_equal(ht$rownames[2:3], rownames(mat), ignore_attr = TRUE)
})


test_that("create huxtable using tribble_hux()", {
  skip_if_not_installed("tibble")
  for (addc in c(TRUE, FALSE)) {
    expect_silent(ht <- tribble_hux(
      ~a, ~b,
      1, "a",
      2, "b",
      3, "c",
      add_colnames = addc
    ))
    expect_s3_class(ht, "huxtable")
    expect_equal(nrow(ht), 3 + addc)
    expect_equal(ncol(ht), 2)
    expect_equal(colnames(ht), c("a", "b"))
  }
})


test_that("create huxtable from data frame", {
  dfr <- data.frame(a = 1:3, b = 1:3)
  expect_silent(ht <- as_hux(dfr))
  expect_silent(ht2 <- as_huxtable(dfr))
  expect_s3_class(ht, "huxtable")
  expect_identical(ht, huxtable(a = 1:3, b = 1:3))
  expect_identical(ht, ht2)
})


test_that("autoformat", {
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
    expect_equal(number_format(ht)[[2, x]], NA)
  }
  for (x in c("numeric", "complex")) {
    expect_equal(number_format(ht)[[2, x]], "%.3g")
  }
  expect_equal(number_format(ht)[[2, "integer"]], 0)

  expect_equal(as.vector(unlist(number_format(ht)[1, ])), rep(NA, ncol(dfr)))

  for (x in c("integer", "Date", "POSIXct", "POSIXlt")) {
    # column heading alignment same:
    expect_equal(as.vector(align(ht)[1:2, x]), rep("right", 2))
  }
  for (x in c("numeric", "complex")) {
    expect_equal(align(ht)[2, x], getOption("OutDec"), ignore_attr = TRUE)
    # headings right aligned:
    expect_equal(align(ht)[1, x], "right", ignore_attr = TRUE)
  }
  expect_equal(as.vector(align(ht)[1:2, "character"]), rep("left", 2))
})


test_that("create huxtable from matrix", {
  m <- matrix(1:8, 4, 2)
  expect_silent(ht <- as_hux(m))
})


test_that("create huxtable from vector", {
  v <- letters[1:5]
  expect_silent(ht <- as_hux(v, add_colnames = FALSE))
  expect_equal(nrow(ht), length(v))
  nv <- 1:5
  expect_silent(ht <- as_hux(nv, add_colnames = FALSE))
  expect_equal(nrow(ht), length(nv))
})


test_that("create huxtable from table", {
  tbl <- table(mtcars$gear, mtcars$cyl)
  expect_silent(ht <- as_hux(tbl))
  expect_s3_class(ht, "huxtable")
  expect_equal(ht[[1]][-1], rownames(tbl))
  expect_equal(as.character(unlist(ht[1, -1])), colnames(tbl))
  expect_equal(ht[[1, 1]], "") # check no "rownames" in top left
})


test_that("as_hux.table does not use number_format on rownames", {
  tbl <- table(c(3.5, 3.5, 4.5, 4.5), c(1.5, 1.5, 2.5, 2.5))
  ht <- as_hux(tbl)
  expect_match(to_screen(ht, colnames = FALSE), "3\\.5")
  expect_match(to_screen(ht, colnames = FALSE), "1\\.5")
})


test_that("create huxtable from ftable", {
  ft <- ftable(mtcars[c("cyl", "vs", "gear")])
  expect_silent(ht <- as_hux(ft))
  # the below tests current implementation, where we have separate rows/columns for column and
  # row name names ("dnn")
  expect_equal(nrow(ht), nrow(ft) + length(attr(ft, "col.vars")) + 1)
  expect_equal(ncol(ht), ncol(ft) + length(attr(ft, "row.vars")) + 1)
})


test_that("create huxtable from grouped_df", {
  skip_if_not_installed("dplyr")

  iris_grp <- dplyr::group_by(iris[c(1:3, 51:53, 101:103), ], Species)
  expect_silent(iris_hux <- as_hux(iris_grp))

  iris_hux2 <- as_hux(iris_grp, groups_to_headers = TRUE, add_colnames = TRUE)
  expect_equal(contents(iris_hux2)[[2, 1]], "Species: setosa")
})
