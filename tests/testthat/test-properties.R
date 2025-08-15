local_edition(3)


# =============================================================================
# PROPERTY INTERFACE TESTS
# Tests for user-facing property getters/setters and their behavior
# =============================================================================

test_that("Basic property assignment works", {
  ht <- hux(1:2, 3:4)
  
  # Test basic assignment and retrieval
  align(ht) <- "left"
  expect_equal(align(ht), matrix("left", 2, 2), ignore_attr = TRUE)
  
  bold(ht) <- TRUE
  expect_equal(bold(ht), matrix(TRUE, 2, 2), ignore_attr = TRUE)
  
  # Test individual cell assignment
  align(ht)[1, 1] <- "right"
  expect_equal(align(ht)[1, 1], "right")
  expect_equal(align(ht)[1, 2], "left")  # unchanged
  
  # Test table properties
  width(ht) <- 0.8
  expect_equal(width(ht), 0.8)
  
  # Test row/column properties
  col_width(ht) <- c(0.3, 0.7)
  expect_equal(col_width(ht), c(0.3, 0.7), ignore_attr = TRUE)
  
  row_height(ht) <- c(0.5, 0.5)
  expect_equal(row_height(ht), c(0.5, 0.5), ignore_attr = TRUE)
})


test_that("Can refer to properties by colnames", {
  ht <- huxtable(a = 1:5, b = letters[1:5], d = 1:5)
  number_format(ht)[1, 1] <- 3
  col_width(ht) <- c(.2, .6, .2)
  row_height(ht) <- rep(.2, 6)
  expect_equal(number_format(ht)[1, "a"], list(3))
  expect_equal(col_width(ht)["a"], .2, ignore_attr = TRUE)
})


test_that("Assignment to properties preserves colnames", {
  ht <- huxtable(a = 1:5, b = letters[1:5], d = 1:5)
  cn <- colnames(align(ht))
  align(ht) <- "right"
  expect_equal(cn, colnames(align(ht)))
  align(ht)[1, 1] <- "left"
  expect_equal(cn, colnames(align(ht)))
})


test_that("Can assign numeric to width, col_width etc. after assigning character", {
  ht <- huxtable(letters[1:3])
  width(ht) <- "300pt"
  width(ht) <- 0.5
  expect_type(width(ht), "double")
  row_height(ht) <- paste0(1:3, "em")
  row_height(ht) <- rep(1 / 3, 3)
  expect_type(row_height(ht), "double")
  number_format(ht) <- "%.3f"
  number_format(ht) <- 2L
  nf <- number_format(ht)
  expect_equal(mode(nf[1, 1][[1]]), "numeric")
  expect_equal(dim(nf), dim(ht))
})


test_that("Can set properties to NA", {
  ht <- hux(1:3, 4:6)
  # expect no error
  expect_error(caption(ht) <- NA, regexp = NA)
  expect_error(font(ht) <- NA, regexp = NA)
  expect_error(col_width(ht) <- NA, regexp = NA)
})


test_that("align, position and caption_pos change \"centre\" to \"center\"", {
  ht <- hux(1)
  align(ht) <- "centre"
  expect_equal(align(ht), matrix("center", 1, 1), ignore_attr = TRUE)

  position(ht) <- "centre"
  expect_equal(position(ht), "center")
  caption_pos(ht) <- "topcentre"
  expect_equal(caption_pos(ht), "topcenter")
  caption_pos(ht) <- "bottomcentre"
  expect_equal(caption_pos(ht), "bottomcenter")
})


# =============================================================================
# SPAN PROPERTY TESTS  
# Tests for rowspan/colspan validation and behavior
# =============================================================================

test_that("rowspan and colspan error when overlapping", {
  ht <- hux(1:4, 1:4)
  rowspan(ht)[1, 2] <- 3
  expect_error(rowspan(ht)[2, 2] <- 2)
  expect_error(rowspan(ht)[3, 2] <- 2)
  expect_error(colspan(ht)[1, 1] <- 2)
  expect_error(colspan(ht)[1, 2] <- 2)
})


test_that("rowspan and colspan overwrite shadowed cell contents when set", {
  ht <- hux(1:2)
  rowspan(ht)[1, 1] <- 2
  expect_equal(ht[[2, 1]], 1)

  ht <- hux(1, 2)
  colspan(ht)[1, 1] <- 2
  expect_equal(ht[[1, 2]], 1)
})


# =============================================================================
# CONTENT PROCESSING TESTS
# Tests for content formatting and processing
# =============================================================================

test_that("na_string works", {
  ht <- hux(c(1, 2, NA), c(NA, 1, 2))
  na_string(ht) <- "foo"
  na_string(ht)[3, 1] <- "bar"
  expect_silent(cc <- huxtable:::clean_contents(ht))

  expect_match(cc[1, 2], "foo")
  expect_match(cc[3, 1], "bar")
})


test_that("Can pad with align", {
  ht <- hux(c("1.5", "2.5"))
  ht2 <- ht
  expect_silent(align(ht) <- ".")
  expect_identical(huxtable:::clean_contents(ht), huxtable:::clean_contents(ht2))
})


# =============================================================================
# DEFAULT PROPERTIES TESTS
# Tests for default property management
# =============================================================================

test_that("set_default_properties", {
  old <- set_default_properties(bold = TRUE)
  expect_equal(bold(hux(1)), matrix(TRUE, 1, 1), ignore_attr = TRUE)
  set_default_properties(old)
  expect_equal(bold(hux(1)), matrix(FALSE, 1, 1), ignore_attr = TRUE)

  expect_error(set_default_properties(unknown = 1))
})


test_that("get_default_properties", {
  expect_equal(get_default_properties("bold"), list(bold = FALSE))
  expect_error(get_default_properties("unknown"))
})


# =============================================================================
# BORDER PROCESSING TESTS  
# Tests for internal border processing functions
# =============================================================================

test_that("collapsed_border_colors works", {
  ht <- hux(1:2, 3:4)
  left_border_color(ht)[1, 2] <- "pink"
  top_border_color(ht)[2, 1] <- "green"
  cbc <- huxtable:::collapsed_border_colors(ht)
  expect_type(cbc, "list")
  expect_equal(cbc$vert, matrix(c(NA, "pink", NA, NA, NA, NA), 2, 3, byrow = TRUE), ignore_attr = TRUE)
  expect_equal(cbc$horiz, matrix(c(NA, NA, "green", NA, NA, NA), 3, 2, byrow = TRUE), ignore_attr = TRUE)
  right_border_color(ht)[1, 1] <- "blue" # overrides
  bottom_border_color(ht)[1, 1] <- "purple"
  cbc <- huxtable:::collapsed_border_colors(ht)
  expect_equal(cbc$vert, matrix(c(NA, "blue", NA, NA, NA, NA), 2, 3, byrow = TRUE), ignore_attr = TRUE)
  expect_equal(cbc$horiz, matrix(c(NA, NA, "purple", NA, NA, NA), 3, 2, byrow = TRUE), ignore_attr = TRUE)
})


test_that("collapsed_border_styles works", {
  ht <- hux(1:2, 3:4)
  left_border_style(ht)[1, 2] <- "dashed"
  top_border_style(ht)[2, 1] <- "double"
  cbs <- huxtable:::collapsed_border_styles(ht)
  expect_type(cbs, "list")
  vert <- matrix("solid", 2, 3)
  vert[1, 2] <- "dashed"
  horiz <- matrix("solid", 3, 2)
  horiz[2, 1] <- "double"
  expect_equal(cbs$vert, vert, ignore_attr = TRUE)
  expect_equal(cbs$horiz, horiz, ignore_attr = TRUE)
})


# =============================================================================
# PROPERTY HELPER FUNCTION TESTS
# Tests for internal property manipulation helpers
# =============================================================================

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
  ht <- huxtable(a = letters[1:3], b = letters[4:6])  # Use characters so default is "left"
  
  # Set specific cells
  ht2 <- prop_set(ht, "align", 1, 1, value = "center")
  expect_equal(align(ht2)[1, 1], "center")
  expect_equal(align(ht2)[2, 2], "left")  # unchanged
  
  # Set entire table using two-argument form
  ht3 <- prop_set(ht, "align", value = "center")
  expect_true(all(align(ht3) == "center"))
  
  # With validation
  expect_error(prop_set(ht, "align", 1, 1, value = 123, check_fun = is.character))
})


test_that("prop_map works", {
  ht <- huxtable(a = letters[1:3], b = letters[4:6])  # Use characters so default is "left"
  
  # Simple mapping function
  map_fn <- function(ht, row, col, current) {
    ifelse(current == "left", "right", "left")
  }
  
  ht2 <- prop_set(ht, "align", 1, 1, fn = map_fn)
  expect_equal(align(ht2)[1, 1], "right")
  expect_equal(align(ht2)[2, 2], "left")  # unchanged
  
  # Map entire table using two-argument form  
  ht3 <- prop_set(ht, "align", fn = map_fn)
  expect_true(all(align(ht3) == "right"))
})


test_that("prop_set_row works", {
  ht <- huxtable(a = 1:3, b = 4:6, add_colnames = FALSE)
  
  # Set specific rows
  ht2 <- prop_set_row(ht, 1, 0.5, "row_height")
  expect_equal(as.numeric(row_height(ht2)[1]), 0.5)  # Convert to remove names
  expect_true(is.na(row_height(ht2)[2]))  # unchanged
  
  # Set all rows using single argument form
  ht3 <- prop_set_row(ht, 0.3, prop = "row_height")
  expect_true(all(row_height(ht3) == 0.3))
})


test_that("prop_set_col works", {
  ht <- huxtable(a = 1:3, b = 4:6, add_colnames = FALSE)
  
  # Set specific columns
  ht2 <- prop_set_col(ht, 1, 0.5, "col_width")
  expect_equal(as.numeric(col_width(ht2)[1]), 0.5)  # Convert to remove names
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