test_that("Non-op arrange_spans", {
  new_ht <- huxtable(a = 1:5, b = letters[1:5], d = 1:5)
  old_ht <- huxtable(a = 1:5, b = letters[1:5], d = 1:5)

  expect_identical(arrange_spans(new_ht, old_ht), new_ht)
})

test_that("Copy spans for merged columns", {
  new_ht <- huxtable(a = 1:5, b = letters[1:5], d = 1:5)
  old_ht <- huxtable(a = 1:5, b = letters[1:5], d = 1:5)
  old_ht <- merge_cells(old_ht, row = 2)
  new_ht <- arrange_spans(new_ht, old_ht)

  expect_identical(colspan(new_ht), colspan(old_ht))
})

test_that("Copy spans for merged rows", {
  new_ht <- huxtable(a = 1:5, b = letters[1:5], d = 1:5)
  old_ht <- huxtable(a = 1:5, b = letters[1:5], d = 1:5)
  old_ht <- merge_cells(old_ht, row = 2:5, col = 2)
  new_ht <- arrange_spans(new_ht, old_ht)

  expect_identical(rowspan(new_ht), rowspan(old_ht))
})

test_that("Copy spans for merged rows and columns", {
  new_ht <- huxtable(a = 1:5, b = letters[1:5], d = 1:5)
  old_ht <- huxtable(a = 1:5, b = letters[1:5], d = 1:5)
  old_ht <- merge_cells(old_ht, row = 2:5, col = 2:3)
  new_ht <- arrange_spans(new_ht, old_ht)

  expect_identical(rowspan(new_ht), rowspan(old_ht))

})
