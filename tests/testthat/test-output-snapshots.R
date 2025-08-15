
skip_on_cran()

skip_without_typst <- function() {
  if (Sys.which("typst") == "" && Sys.which("quarto") == "") skip("typst CLI not found")
}

make_tables <- function() {
  # Text properties: font, italic, bold, markdown, color, font_size
  text_props <- hux(
    "Property" = c("bold", "italic", "font", "font_size", "markdown", "text_color", "background"),
    "Normal" = c("normal text", "normal text", "serif text", "12pt text", "plain text", "black text", "white bg"),
    "Styled" = c("bold=TRUE", "italic=TRUE", "Times New Roman", "font_size=16", "*markdown*=TRUE", "red text", "grey90 bg"),
    add_colnames = TRUE
  )
  text_props <- set_all_borders(text_props)
  bold(text_props)[2, 3] <- TRUE
  italic(text_props)[3, 3] <- TRUE
  font(text_props)[4, 3] <- "Times New Roman"
  font_size(text_props)[5, 3] <- 16
  markdown(text_props)[6, 3] <- TRUE
  text_color(text_props)[7, 3] <- "red"
  background_color(text_props)[8, 3] <- "grey90"
  caption(text_props) <- "Text formatting: bold, italic, font, font_size, markdown, text_color, background_color"

  # Text positioning: align, valign, padding, rotation, wrap
  text_position <- hux(
    "Align" = c("left+top", "center+middle", "right+bottom"),
    "Padding" = c("normal", "left_padding=10", "normal"),
    "Effects" = c("normal", "normal", "rotation=90"),
    add_colnames = TRUE
  )
  text_position <- set_all_borders(text_position)
  # Set alignments - row 2 (first data row)
  align(text_position)[2, 1] <- "left"
  align(text_position)[2, 2] <- "center"
  align(text_position)[2, 3] <- "right"
  valign(text_position)[2, 1] <- "top"
  valign(text_position)[2, 2] <- "middle"
  valign(text_position)[2, 3] <- "bottom"
  # Set padding - row 3 (second data row), column 2
  left_padding(text_position)[3, 2] <- 10
  right_padding(text_position)[3, 2] <- 10
  # Set rotation - row 4 (third data row), column 3
  rotation(text_position)[4, 3] <- 90
  # Add wrap to show another effect
  text_position[4, 1] <- "wrap=TRUE very long text that should wrap around"
  wrap(text_position)[4, 1] <- TRUE
  row_height(text_position) <- c(NA, 40, 40, 60)
  width(text_position) <- 0.7  # Set width so wrap works in LaTeX
  caption(text_position) <- "Text positioning: align, valign, padding, rotation, wrap"

  # Border properties: width, color, style
  borders_table <- hux(
    "Description" = c("thickness=3", "color=red", "style=dashed", "style=double", "mixed styles"),
    "Top border" = c("top=3", "top=red", "top=dashed", "top=double", "normal"),
    "Bottom border" = c("normal", "normal", "normal", "normal", "bottom=2"),
    add_colnames = TRUE
  )
  borders_table <- set_all_borders(borders_table)
  top_border(borders_table)[2, ] <- 3
  top_border_color(borders_table)[3, ] <- "red"
  top_border_style(borders_table)[4, ] <- "dashed"
  top_border_style(borders_table)[5, ] <- "double"
  bottom_border(borders_table)[6, 3] <- 2
  caption(borders_table) <- "Border properties: top_border thickness=3, color=red, style=dashed/double"

  # Dimensions: col_width and row_height
  dimensions <- hux(
    "col_width=0.2" = c("Narrow 20%", "normal height", "tall height"),
    "col_width=0.3" = c("Medium 30%", "row_height=40", "row_height=60"),
    "col_width=0.5" = c("Wide 50%", "row_height=40", "row_height=60"),
    "Row Heights" = c("header", "40pt", "60pt"),
    add_colnames = TRUE
  )
  dimensions <- set_all_borders(dimensions)
  col_width(dimensions) <- c(0.2, 0.3, 0.5, 0.15)  # Add width for new column
  row_height(dimensions) <- c(NA, 40, 60, 60)  # Different heights for rows 2 and 3
  caption(dimensions) <- "Dimensions: col_width=(0.2, 0.3, 0.5, 0.15), row_height=(NA, 40, 60)"

  # Table caption properties - multiple tables in one document
  table_caption_tests <- list(
    # Caption at top (default)
    {
      ht <- hux("caption_pos=top", add_colnames = FALSE)
      ht <- set_all_borders(ht)
      caption(ht) <- "Table 1: caption_pos=\"top\" (default)"
      caption_pos(ht) <- "top"
      ht
    },
    # Caption at bottom
    {
      ht <- hux("caption_pos=bottom", add_colnames = FALSE)
      ht <- set_all_borders(ht)
      caption(ht) <- "Table 2: caption_pos=\"bottom\""
      caption_pos(ht) <- "bottom"
      ht
    },
    # Caption width 0.5
    {
      ht <- hux("caption_width=0.5", add_colnames = FALSE)
      ht <- set_all_borders(ht)
      caption(ht) <- "Table 3: caption_width=0.5 - This is a longer caption to demonstrate width constraint"
      caption_width(ht) <- 0.5
      ht
    },
    # Caption width 0.8
    {
      ht <- hux("caption_width=0.8", add_colnames = FALSE)
      ht <- set_all_borders(ht)
      caption(ht) <- "Table 4: caption_width=0.8 - This is a longer caption to demonstrate a wider width constraint"
      caption_width(ht) <- 0.8
      ht
    }
  )

  # Table position properties - multiple tables in one document
  table_position_tests <- list(
    # Position left
    {
      ht <- hux("position=left", add_colnames = FALSE)
      ht <- set_all_borders(ht)
      position(ht) <- "left"
      caption(ht) <- "Table 1: position=\"left\""
      ht
    },
    # Position center
    {
      ht <- hux("position=center", add_colnames = FALSE)
      ht <- set_all_borders(ht)
      position(ht) <- "center"
      caption(ht) <- "Table 2: position=\"center\""
      ht
    },
    # Position right
    {
      ht <- hux("position=right", add_colnames = FALSE)
      ht <- set_all_borders(ht)
      position(ht) <- "right"
      caption(ht) <- "Table 3: position=\"right\""
      ht
    }
  )

  # Table width properties - multiple tables in one document
  table_width_tests <- list(
    # Width 30%
    {
      ht <- hux("width=0.3", add_colnames = FALSE)
      ht <- set_all_borders(ht)
      width(ht) <- 0.3
      caption(ht) <- "Table 1: width=0.3 (30%)"
      ht
    },
    # Width 60%
    {
      ht <- hux("width=0.6", add_colnames = FALSE)
      ht <- set_all_borders(ht)
      width(ht) <- 0.6
      caption(ht) <- "Table 2: width=0.6 (60%)"
      ht
    },
    # Width 90%
    {
      ht <- hux("width=0.9", add_colnames = FALSE)
      ht <- set_all_borders(ht)
      width(ht) <- 0.9
      caption(ht) <- "Table 3: width=0.9 (90%)"
      ht
    }
  )

  # Content formatting: number_format, na_string, escape
  content_format <- hux(
    "Property" = c("number_format=0", "number_format=%.2f", "na_string=missing", "escape_contents=FALSE"),
    "Raw value" = c(1234.5678, 3.14159, NA, "<b>HTML</b>"),
    "Formatted" = c(1234.5678, 3.14159, NA, "<b>HTML</b>"),
    add_colnames = TRUE
  )
  content_format <- set_all_borders(content_format)
  number_format(content_format)[2, 3] <- 0  # integer format
  number_format(content_format)[3, 3] <- "%.2f"  # 2 decimal places
  na_string(content_format)[4, 3] <- "missing"
  escape_contents(content_format)[5, 3] <- FALSE
  caption(content_format) <- "Content formatting: number_format, na_string, escape_contents"

  list(
    text_properties = text_props,
    text_positioning = text_position,
    borders = borders_table,
    dimensions = dimensions,
    table_caption_tests = table_caption_tests,
    table_position_tests = table_position_tests,
    table_width_tests = table_width_tests,
    content_formatting = content_format
  )
}


# Helper function to test output snapshots for different formats
test_output_format <- function(quick_func, file_ext, snapshot_suffix = "") {
  tables <- make_tables()
  multi_table_names <- c("table_caption_tests", "table_position_tests", "table_width_tests")

  for (nm in names(tables)) {
    # Handle different file extension patterns
    if (file_ext == "" && grepl("typst_(png|svg)", deparse(substitute(quick_func)))) {
      f <- tempfile(pattern = nm, fileext = "")
    } else {
      f <- tempfile(pattern = nm, fileext = file_ext)
    }

    # Generate output
    if (nm %in% multi_table_names) {
      do.call(quick_func, c(tables[[nm]], file = f, open = FALSE))
    } else {
      quick_func(tables[[nm]], file = f, open = FALSE)
    }

    # Handle file checking for different formats
    if (grepl("typst_(png|svg)", deparse(substitute(quick_func)))) {
      # For PNG/SVG, find files with suffixes
      file_ext_actual <- ifelse(grepl("png", deparse(substitute(quick_func))), "png", "svg")
      file_pattern <- paste0("^", basename(f), ".*\\.", file_ext_actual, "$")
      output_files <- list.files(dirname(f), pattern = file_pattern, full.names = TRUE)
      if (length(output_files) > 0) {
        if (nm %in% multi_table_names && length(output_files) > 1) {
          # For multi-table tests, save all files with numbered suffixes
          for (i in seq_along(output_files)) {
            expect_snapshot_file(output_files[i], paste0(nm, "-", i, snapshot_suffix))
          }
        } else {
          # Single table test
          expect_snapshot_file(output_files[1], paste0(nm, snapshot_suffix))
        }
      }
    } else if (file_ext == "" && !grepl("typst_(png|svg)", deparse(substitute(quick_func)))) {
      # Single file with added extension
      output_file <- paste0(f, gsub("^\\.", "", snapshot_suffix))
      if (file.exists(output_file)) {
        expect_snapshot_file(output_file, paste0(nm, snapshot_suffix))
      }
    } else {
      # Standard file output
      expect_snapshot_file(f, paste0(nm, snapshot_suffix))
    }
  }
}

test_that("pdf snapshots", {
  test_output_format(quick_pdf, ".pdf", ".pdf")
})

test_that("typst pdf snapshots", {
  skip_without_typst()
  test_output_format(quick_typst_pdf, ".pdf", "-typst.pdf")
})

test_that("docx snapshots", {
  skip_if_not_installed("officer")
  skip_if_not_installed("flextable")
  test_output_format(quick_docx, ".docx", ".docx")
})

test_that("html snapshots", {
  test_output_format(quick_html, ".html", ".html")
})

test_that("typst png snapshots", {
  skip_without_typst()
  test_output_format(quick_typst_png, "", ".png")
})

test_that("typst svg snapshots", {
  skip_without_typst()
  test_output_format(quick_typst_svg, "", ".svg")
})
