
skip_on_cran()

skip_without_typst <- function() {
  if (Sys.which("typst") == "") skip("typst CLI not found")
}

make_tables <- function() {
  # Text properties: font, italic, bold, markdown, color
  text_props <- hux(
    "Property" = c("bold", "italic", "font", "markdown", "text_color", "background"),
    "Normal" = c("normal", "normal", "serif", "plain text", "black", "white"),
    "Styled" = c("**BOLD**", "*italic*", "Times New Roman", "*markdown* text", "red text", "grey bg"),
    add_colnames = TRUE
  )
  bold(text_props)[2, 3] <- TRUE
  italic(text_props)[3, 3] <- TRUE
  font(text_props)[4, 3] <- "Times New Roman"
  markdown(text_props)[5, 3] <- TRUE
  text_color(text_props)[6, 3] <- "red"
  background_color(text_props)[7, 3] <- "grey90"
  caption(text_props) <- "Testing text formatting properties: font styles, colors, and markdown"

  # Text positioning: align, valign, padding, rotation, wrap
  text_position <- hux(
    "left" = c("left", "padded", "rotated"),
    "center" = c("center", "normal", "normal"),
    "right" = c("right", "normal", "wrapped long text example"),
    add_colnames = TRUE
  )
  align(text_position)[2, 1] <- "left"
  align(text_position)[2, 2] <- "center"
  align(text_position)[2, 3] <- "right"
  valign(text_position)[2, 1] <- "top"
  valign(text_position)[2, 2] <- "middle"
  valign(text_position)[2, 3] <- "bottom"
  left_padding(text_position)[3, 1] <- 10
  right_padding(text_position)[3, 1] <- 10
  rotation(text_position)[4, 1] <- 90
  wrap(text_position)[4, 3] <- TRUE
  caption(text_position) <- "Testing text positioning: alignment, padding, rotation, and wrapping"

  # Border properties: width, color, style
  borders_table <- hux(
    "Type" = c("thick", "colored", "dashed", "mixed"),
    "Top" = c("thick top", "red top", "dashed top", "normal"),
    "Bottom" = c("normal", "normal", "normal", "thick bottom"),
    add_colnames = TRUE
  )
  top_border(borders_table)[2, ] <- 3
  top_border_color(borders_table)[3, ] <- "red"
  top_border_style(borders_table)[4, ] <- "dashed"
  bottom_border(borders_table)[5, 3] <- 2
  left_border(borders_table)[, 1] <- 1
  right_border(borders_table)[, 3] <- 1
  caption(borders_table) <- "Testing border properties: thickness, colors, and styles"

  # Dimensions: col_width and row_height
  dimensions <- hux(
    "Narrow" = c("20%", "content"),
    "Medium" = c("30%", "content"),
    "Wide" = c("50%", "content"),
    add_colnames = TRUE
  )
  col_width(dimensions) <- c(0.2, 0.3, 0.5)
  row_height(dimensions) <- c(NA, 40)
  caption(dimensions) <- "Testing table dimensions: column widths and row heights"

  # Table properties (smaller tables combined)
  table_pos_left <- hux("Left positioned", add_colnames = FALSE)
  position(table_pos_left) <- "left"
  caption(table_pos_left) <- "Left positioned table"

  table_pos_center <- hux("Center positioned", add_colnames = FALSE)
  position(table_pos_center) <- "center"
  caption(table_pos_center) <- "Center positioned table"

  table_width_50 <- hux("50% width table", add_colnames = FALSE)
  width(table_width_50) <- 0.5
  caption(table_width_50) <- "Table with 50% width"

  table_caption <- hux("Table with caption", add_colnames = FALSE)
  caption(table_caption) <- "This table tests caption positioning and formatting"

  # Content formatting: number_format, na_string, escape
  content_format <- hux(
    "Type" = c("number", "decimal", "NA value", "escaped"),
    "Raw" = c(1234.5678, 3.14159, NA, "<b>HTML</b>"),
    "Formatted" = c(1234.5678, 3.14159, NA, "<b>HTML</b>"),
    add_colnames = TRUE
  )
  number_format(content_format)[2, 3] <- 0  # integer format
  number_format(content_format)[3, 3] <- "%.2f"  # 2 decimal places
  na_string(content_format)[4, 3] <- "missing"
  escape_contents(content_format)[5, 3] <- FALSE
  caption(content_format) <- "Testing content formatting: numbers, NA values, and HTML escaping"

  list(
    text_properties = text_props,
    text_positioning = text_position,
    borders = borders_table,
    dimensions = dimensions,
    table_position_left = table_pos_left,
    table_position_center = table_pos_center,
    table_width = table_width_50,
    table_caption = table_caption,
    content_formatting = content_format
  )
}


test_that("pdf snapshots", {
  tables <- make_tables()
  for (nm in names(tables)) {
    f <- tempfile(pattern = nm, fileext = ".pdf")
    quick_pdf(tables[[nm]], file = f, open = FALSE)
    expect_snapshot_file(f, paste0(nm, ".pdf"))
  }
})


test_that("typst pdf snapshots", {
  skip_without_typst()
  tables <- make_tables()
  for (nm in names(tables)) {
    f <- tempfile(pattern = nm, fileext = ".pdf")
    quick_typst_pdf(tables[[nm]], file = f, open = FALSE)
    expect_snapshot_file(f, paste0(nm, "-typst.pdf"))
  }
})


test_that("docx snapshots", {
  skip_if_not_installed("officer")
  skip_if_not_installed("flextable")
  tables <- make_tables()
  for (nm in names(tables)) {
    f <- tempfile(pattern = nm, fileext = ".docx")
    quick_docx(tables[[nm]], file = f, open = FALSE)
    expect_snapshot_file(f, paste0(nm, ".docx"))
  }
})


test_that("html snapshots", {
  tables <- make_tables()
  for (nm in names(tables)) {
    f <- tempfile(pattern = nm, fileext = ".html")
    quick_html(tables[[nm]], file = f, open = FALSE)
    expect_snapshot_file(f, paste0(nm, ".html"))
  }
})
