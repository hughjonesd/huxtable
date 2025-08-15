
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
  text_props <- set_all_borders(text_props, value = 1)
  bold(text_props)[2, 3] <- TRUE
  italic(text_props)[3, 3] <- TRUE
  font(text_props)[4, 3] <- "Times New Roman"
  font_size(text_props)[5, 3] <- 16
  markdown(text_props)[6, 3] <- TRUE
  text_color(text_props)[7, 3] <- "red"
  background_color(text_props)[8, 3] <- "grey90"
  caption(text_props) <- "Text formatting: bold, italic, font, font_size, markdown, text_color, background_color"

  # Text alignment: align and valign combinations
  text_alignment <- hux(
    "Left" = c("left+top", "left+middle", "left+bottom"),
    "Center" = c("center+top", "center+middle", "center+bottom"),
    "Right" = c("right+top", "right+middle", "right+bottom"),
    add_colnames = TRUE
  )
  text_alignment <- set_all_borders(text_alignment, value = 1)
  # Set column widths (absolute values)
  col_width(text_alignment) <- c("3cm", "3cm", "3cm")
  # Set row heights to 2em
  row_height(text_alignment) <- rep("2em", 4)
  # Apply alignments systematically
  for (row in 2:4) {
    for (col in 1:3) {
      if (col == 1) align(text_alignment)[row, col] <- "left"
      if (col == 2) align(text_alignment)[row, col] <- "center"
      if (col == 3) align(text_alignment)[row, col] <- "right"

      if (row == 2) valign(text_alignment)[row, col] <- "top"
      if (row == 3) valign(text_alignment)[row, col] <- "middle"
      if (row == 4) valign(text_alignment)[row, col] <- "bottom"
    }
  }
  caption(text_alignment) <- "Text alignment: align=(left, center, right) × valign=(top, middle, bottom)"

  # Text effects: wrap, rotation, padding
  text_effects <- hux(
    "Wrap test" = c("wrap=TRUE", "wrap=FALSE"),
    "Rotation" = c("normal", "rotation=90"),
    "Padding" = c("normal", "padding=10px"),
    add_colnames = TRUE
  )
  text_effects <- set_all_borders(text_effects, value = 1)
  # Set absolute column widths
  col_width(text_effects) <- c(0.25, 0.25, 0.5)
  width(text_effects) <- 0.6
  row_height(text_effects) <- c(1/3, 1/3, 1/3)

  # Add long text for wrap test - same text with different wrap settings
  long_text <- "This is a very long text that should demonstrate text wrapping behavior when wrap is enabled versus disabled"
  text_effects[2, 1] <- long_text
  text_effects[3, 1] <- long_text
  wrap(text_effects)[2, 1] <- TRUE
  wrap(text_effects)[3, 1] <- FALSE

  # Set rotation
  rotation(text_effects)[3, 2] <- 90

  # Add long text for padding test
  padding_text <- "This text demonstrates padding effects with longer content to show the spacing difference"
  text_effects[2, 3] <- padding_text
  text_effects[3, 3] <- padding_text

  # Set padding
  left_padding(text_effects)[2, 3] <- 10
  right_padding(text_effects)[2, 3] <- 10
  top_padding(text_effects)[2, 3] <- 10
  bottom_padding(text_effects)[2, 3] <- 10

  caption(text_effects) <- "Text effects: wrap (TRUE vs FALSE), rotation=90, padding=10px"

  # Border properties: width, color, style
  borders_table <- hux(
    "Description" = c("thickness=3", "color=red", "style=dashed", "style=double", "mixed styles"),
    "Top border" = c("top=3", "top=red", "top=dashed", "top=double", "normal"),
    "Bottom border" = c("normal", "normal", "normal", "normal", "bottom=2"),
    add_colnames = TRUE
  )
  borders_table <- set_all_borders(borders_table, value = 1)
  top_border(borders_table)[2, ] <- 3
  top_border_color(borders_table)[3, ] <- "red"
  top_border_style(borders_table)[4, ] <- "dashed"
  top_border_style(borders_table)[5, ] <- "double"
  top_border(borders_table)[5, ] <- 3
  bottom_border(borders_table)[6, 3] <- 2
  caption(borders_table) <- "Border properties: top_border thickness=3, color=red, style=dashed/double"

  # Dimensions: col_width and row_height
  dimensions <- hux(
    "col_width=0.2" = c("Narrow 20%", "text", "text", "text"),
    "col_width=0.3" = c("Medium 30%", "text", "text", "text"),
    "col_width=0.35" = c("Wide 35%", "text", "text", "text"),
    "Row Heights" = c("0.15", "0.15", "015", "0.4"),
    add_colnames = TRUE
  )
  dimensions <- set_all_borders(dimensions, value = 1)
  col_width(dimensions) <- c(0.2, 0.3, 0.35, 0.15)  # Total = 1.0
  row_height(dimensions) <- c(0.15, 0.15, 0.15, 0.15, 0.4)  # Different heights for all rows including header
  caption(dimensions) <- "Dimensions: col_width=(0.2, 0.3, 0.35, 0.15)=1.0, row_height=(0.15, 0.15, 0.15, 0.15, 0.4)"

  # Table caption properties - multiple tables in one document
  table_caption_tests <- list(
    # Caption at top (default)
    {
      ht <- hux("caption_pos=top", add_colnames = FALSE)
      ht <- set_all_borders(ht, value = 1)
      caption(ht) <- "Table 1: caption_pos=\"top\" (default)"
      caption_pos(ht) <- "top"
      ht
    },
    # Caption at bottom
    {
      ht <- hux("caption_pos=bottom", add_colnames = FALSE)
      ht <- set_all_borders(ht, value = 1)
      caption(ht) <- "Table 2: caption_pos=\"bottom\""
      caption_pos(ht) <- "bottom"
      ht
    },
    # Caption width 0.5
    {
      ht <- hux("caption_width=0.5", add_colnames = FALSE)
      ht <- set_all_borders(ht, value = 1)
      caption(ht) <- "Table 3: caption_width=0.5 - This is a longer caption to demonstrate width constraint"
      caption_width(ht) <- 0.5
      ht
    },
    # Caption width 0.8
    {
      ht <- hux("caption_width=0.8", add_colnames = FALSE)
      ht <- set_all_borders(ht, value = 1)
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
      ht <- set_all_borders(ht, value = 1)
      position(ht) <- "left"
      caption(ht) <- "Table 1: position=\"left\""
      ht
    },
    # Position center
    {
      ht <- hux("position=center", add_colnames = FALSE)
      ht <- set_all_borders(ht, value = 1)
      position(ht) <- "center"
      caption(ht) <- "Table 2: position=\"center\""
      ht
    },
    # Position right
    {
      ht <- hux("position=right", add_colnames = FALSE)
      ht <- set_all_borders(ht, value = 1)
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
      ht <- set_all_borders(ht, value = 1)
      width(ht) <- 0.3
      caption(ht) <- "Table 1: width=0.3 (30%)"
      ht
    },
    # Width 60%
    {
      ht <- hux("width=0.6", add_colnames = FALSE)
      ht <- set_all_borders(ht, value = 1)
      width(ht) <- 0.6
      caption(ht) <- "Table 2: width=0.6 (60%)"
      ht
    },
    # Width 90%
    {
      ht <- hux("width=0.9", add_colnames = FALSE)
      ht <- set_all_borders(ht, value = 1)
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
  content_format <- set_all_borders(content_format, value = 1)
  number_format(content_format)[2, 3] <- 0  # integer format
  number_format(content_format)[3, 3] <- "%.2f"  # 2 decimal places
  na_string(content_format)[4, 3] <- "missing"
  escape_contents(content_format)[5, 3] <- FALSE
  caption(content_format) <- "Content formatting: number_format, na_string, escape_contents"

  # Cell spanning: colspan and rowspan
  spanning <- hux(
    "Description" = c("normal cell", "colspan=2", "", "rowspan=2", "normal", "rowspan+colspan"),
    "Test" = c("single cell", "spans 2 cols", "", "spans 2 rows", "normal", "spans both"),
    "Column 3" = c("normal", "part of span", "", "normal", "normal", "part of span"),
    "Column 4" = c("normal", "normal", "normal", "normal", "normal", "part of span"),
    add_colnames = TRUE
  )
  spanning <- set_all_borders(spanning, value = 1)
  colspan(spanning)[3, 2] <- 2  # Row 3, span columns 2-3
  rowspan(spanning)[4, 1] <- 2  # Row 4, span rows 4-5
  colspan(spanning)[6, 2] <- 2  # Row 6, span columns 2-3
  rowspan(spanning)[6, 2] <- 2  # Row 6, span rows 6-7 (if exists)
  caption(spanning) <- "Cell spanning: colspan=2, rowspan=2, combined colspan+rowspan"

  list(
    text_properties = text_props,
    text_alignment = text_alignment,
    text_effects = text_effects,
    borders = borders_table,
    dimensions = dimensions,
    table_caption_tests = table_caption_tests,
    table_position_tests = table_position_tests,
    table_width_tests = table_width_tests,
    content_formatting = content_format,
    cell_spanning = spanning
  )
}


# Helper function to test output snapshots for different formats
test_output_format <- function(quick_func, file_ext, snapshot_suffix = "") {
  # Set fixed timestamp for deterministic Typst PDF output
  if (grepl("typst", deparse(substitute(quick_func)))) {
    Sys.setenv(SOURCE_DATE_EPOCH = "1704110400")  # 2024-01-01 12:00:00 UTC
  }

  tables <- make_tables()
  multi_table_names <- c("table_caption_tests", "table_position_tests", "table_width_tests")

  for (nm in names(tables)) {
    # Use fixed path to avoid any randomness in filenames
    if (file_ext == "" && grepl("typst_(png|svg)", deparse(substitute(quick_func)))) {
      f <- file.path("/tmp", nm)
    } else {
      f <- file.path("/tmp", paste0(nm, file_ext))
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

test_that("latex snapshots", {
  test_output_format(quick_latex, ".tex", ".tex")
})

test_that("typst pdf snapshots", {
  skip_without_typst()
  test_output_format(quick_typst_pdf, ".pdf", "-typst.pdf")
})

test_that("rtf snapshots", {
  test_output_format(quick_rtf, ".rtf", ".rtf")
})

test_that("docx-as-rtf snapshots", {
  skip_if_not_installed("officer")
  skip_if_not_installed("flextable")
  if (Sys.which("pandoc") == "") skip("pandoc not found")

  # Test DOCX conversion pathway but output as RTF for determinism
  test_docx_as_rtf <- function(tables, file_prefix) {
    multi_table_names <- c("table_caption_tests", "table_position_tests", "table_width_tests")

    for (nm in names(tables)) {
      if (nm %in% multi_table_names) {
        # Handle multiple tables
        for (i in seq_along(tables[[nm]])) {
          ft <- huxtable::as_flextable(tables[[nm]][[i]])
          rtf_file <- file.path("/tmp", paste0(nm, "-", i, ".rtf"))
          flextable::save_as_rtf(ft, path = rtf_file)
          expect_snapshot_file(rtf_file, paste0(nm, "-", i, "-docx.rtf"))
        }
      } else {
        # Single table
        ft <- huxtable::as_flextable(tables[[nm]])
        rtf_file <- file.path("/tmp", paste0(nm, ".rtf"))
        flextable::save_as_rtf(ft, path = rtf_file)
        expect_snapshot_file(rtf_file, paste0(nm, "-docx.rtf"))
      }
    }
  }

  tables <- make_tables()
  test_docx_as_rtf(tables, "docx")
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
