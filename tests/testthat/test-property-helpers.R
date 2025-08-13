local_edition(2)

test_that("prop_get works", {
  ht <- huxtable(a = 1:3, b = 4:6)
  
  expect_equal(prop_get(ht, "align"), align(ht))
  expect_equal(prop_get(ht, "bold"), bold(ht))
  expect_equal(prop_get(ht, "width"), width(ht))
})

test_that("validate_prop works", {
  # Basic validation
  expect_equal(validate_prop(c("left", "right"), "align"), c("left", "right"))
  
  # Check function validation
  expect_error(validate_prop(c(1, 2), "align", check_fun = is.character))
  expect_silent(validate_prop(c("left", "right"), "align", check_fun = is.character))
  
  # Check values validation
  expect_error(validate_prop(c("invalid"), "align", check_values = c("left", "center", "right")))
  expect_silent(validate_prop(c("left"), "align", check_values = c("left", "center", "right")))
  
  # NA reset
  expect_equal(validate_prop(c(NA, "left"), "align"), c("left", "left"))
  expect_equal(validate_prop(c(NA, "left"), "align", reset_na = FALSE), c(NA, "left"))
})

test_that("prop_set replaces entire property when row/col missing", {
  ht <- huxtable(a = 1:3, b = 4:6)
  
  # Replace entire property (old prop_replace behavior)
  ht2 <- prop_set(ht, value = "center", prop = "align")
  expect_true(all(align(ht2) == "center"))
  
  # With validation
  expect_error(prop_set(ht, value = 123, prop = "align", check_fun = is.character))
  
  # With check_values
  expect_error(prop_set(ht, value = "invalid", prop = "align", check_values = c("left", "center", "right")))
})

test_that("prop_set works", {
  ht <- huxtable(a = 1:3, b = 4:6)
  
  # Set specific cells
  ht2 <- prop_set(ht, 1, 1, "center", "align")
  expect_equal(align(ht2)[1, 1], "center")
  expect_equal(align(ht2)[2, 2], "left")  # unchanged
  
  # Set entire table using two-argument form
  ht3 <- prop_set(ht, "center", prop = "align")
  expect_true(all(align(ht3) == "center"))
  
  # With validation
  expect_error(prop_set(ht, 1, 1, 123, "align", check_fun = is.character))
})

test_that("prop_map works", {
  ht <- huxtable(a = 1:3, b = 4:6)
  
  # Simple mapping function
  map_fn <- function(ht, row, col, current) {
    ifelse(current == "left", "right", "left")
  }
  
  ht2 <- prop_map(ht, 1, 1, map_fn, "align")
  expect_equal(align(ht2)[1, 1], "right")
  expect_equal(align(ht2)[2, 2], "left")  # unchanged
  
  # Map entire table using two-argument form  
  ht3 <- prop_map(ht, map_fn, prop = "align")
  expect_true(all(align(ht3) == "right"))
})

test_that("prop_set_row works", {
  ht <- huxtable(a = 1:3, b = 4:6, add_colnames = FALSE)
  
  # Set specific rows
  ht2 <- prop_set_row(ht, 1, 0.5, "row_height")
  expect_equal(row_height(ht2)[1], 0.5)
  expect_true(is.na(row_height(ht2)[2]))  # unchanged
  
  # Set all rows using single argument form
  ht3 <- prop_set_row(ht, 0.3, prop = "row_height")
  expect_true(all(row_height(ht3) == 0.3))
})

test_that("prop_set_col works", {
  ht <- huxtable(a = 1:3, b = 4:6, add_colnames = FALSE)
  
  # Set specific columns
  ht2 <- prop_set_col(ht, 1, 0.5, "col_width")
  expect_equal(col_width(ht2)[1], 0.5)
  expect_true(is.na(col_width(ht2)[2]))  # unchanged
  
  # Set all columns using single argument form
  ht3 <- prop_set_col(ht, 0.4, prop = "col_width")
  expect_true(all(col_width(ht3) == 0.4))
})

test_that("prop_set_table works", {
  ht <- huxtable(a = 1:3, b = 4:6)
  
  # Set table property
  ht2 <- prop_set_table(ht, 0.8, "width")
  expect_equal(width(ht2), 0.8)
  
  # With validation
  expect_error(prop_set_table(ht, "invalid", "width", check_fun = is.numeric))
})