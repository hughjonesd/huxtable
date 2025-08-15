
skip_on_cran()

skip_without_typst <- function() {
  if (Sys.which("typst") == "") skip("typst CLI not found")
}

make_tables <- function() {
  # Text properties: font, italic, bold, markdown, color
  text_props <- hux(
    "Property" = c("bold", "italic", "font", "markdown", "text_color", "background"),
    "Normal" = c("normal text", "normal text", "serif text", "plain text", "black text", "white bg"),
    "Styled" = c("bold=TRUE", "italic=TRUE", "Times New Roman", "*markdown*=TRUE", "red text", "grey90 bg"),
    add_colnames = TRUE
  )
  text_props <- set_all_borders(text_props)
  bold(text_props)[2, 3] <- TRUE
  italic(text_props)[3, 3] <- TRUE
  font(text_props)[4, 3] <- "Times New Roman"
  markdown(text_props)[5, 3] <- TRUE
  text_color(text_props)[6, 3] <- "red"
  background_color(text_props)[7, 3] <- "grey90"
  caption(text_props) <- "Text formatting: bold, italic, font, markdown, text_color, background_color"

  # Text positioning: align, valign, padding, rotation, wrap
  text_position <- hux(
    "Align" = c("left+top", "center+middle", "right+bottom"),
    "Padding" = c("left_padding=10", "normal", "normal"),
    "Effects" = c("rotation=90", "normal", "wrap=TRUE long text"),
    add_colnames = TRUE
  )
  text_position <- set_all_borders(text_position)
  # Set alignments
  align(text_position)[2, 1] <- "left"
  align(text_position)[2, 2] <- "center"
  align(text_position)[2, 3] <- "right"
  valign(text_position)[2, 1] <- "top"
  valign(text_position)[2, 2] <- "middle"
  valign(text_position)[2, 3] <- "bottom"
  # Set padding
  left_padding(text_position)[3, 1] <- 10
  right_padding(text_position)[3, 1] <- 10
  # Set rotation and wrap
  rotation(text_position)[4, 1] <- 90
  wrap(text_position)[4, 3] <- TRUE
  row_height(text_position) <- c(NA, 40, 40, 60)
  caption(text_position) <- "Text positioning: align, valign, padding, rotation, wrap"

  # Border properties: width, color, style
  borders_table <- hux(
    "Description" = c("thickness=3", "color=red", "style=dashed", "mixed styles"),
    "Top border" = c("top=3", "top=red", "top=dashed", "normal"),
    "Bottom border" = c("normal", "normal", "normal", "bottom=2"),
    add_colnames = TRUE
  )
  borders_table <- set_all_borders(borders_table)
  top_border(borders_table)[2, ] <- 3
  top_border_color(borders_table)[3, ] <- "red"
  top_border_style(borders_table)[4, ] <- "dashed"
  bottom_border(borders_table)[5, 3] <- 2
  caption(borders_table) <- "Border properties: top_border thickness=3, color=red, style=dashed"

  # Dimensions: col_width and row_height
  dimensions <- hux(
    "col_width=0.2" = c("Narrow 20%", "normal height"),
    "col_width=0.3" = c("Medium 30%", "normal height"),
    "col_width=0.5" = c("Wide 50%", "row_height=40"),
    add_colnames = TRUE
  )
  dimensions <- set_all_borders(dimensions)
  col_width(dimensions) <- c(0.2, 0.3, 0.5)
  row_height(dimensions) <- c(NA, 40)
  caption(dimensions) <- "Dimensions: col_width=(0.2, 0.3, 0.5), row_height=40"

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


test_that("pdf snapshots", {
  tables <- make_tables()
  for (nm in names(tables)) {
    f <- tempfile(pattern = nm, fileext = ".pdf")
    if (nm %in% c("table_caption_tests", "table_position_tests", "table_width_tests")) {
      # Pass the list of tables as separate arguments
      do.call(quick_pdf, c(tables[[nm]], file = f, open = FALSE))
    } else {
      quick_pdf(tables[[nm]], file = f, open = FALSE)
    }
    expect_snapshot_file(f, paste0(nm, ".pdf"))
  }
})


test_that("typst pdf snapshots", {
  skip_without_typst()
  tables <- make_tables()
  for (nm in names(tables)) {
    f <- tempfile(pattern = nm, fileext = ".pdf")
    if (nm %in% c("table_caption_tests", "table_position_tests", "table_width_tests")) {
      do.call(quick_typst_pdf, c(tables[[nm]], file = f, open = FALSE))
    } else {
      quick_typst_pdf(tables[[nm]], file = f, open = FALSE)
    }
    expect_snapshot_file(f, paste0(nm, "-typst.pdf"))
  }
})


test_that("docx snapshots", {
  skip_if_not_installed("officer")
  skip_if_not_installed("flextable")
  tables <- make_tables()
  for (nm in names(tables)) {
    f <- tempfile(pattern = nm, fileext = ".docx")
    if (nm %in% c("table_caption_tests", "table_position_tests", "table_width_tests")) {
      do.call(quick_docx, c(tables[[nm]], file = f, open = FALSE))
    } else {
      quick_docx(tables[[nm]], file = f, open = FALSE)
    }
    expect_snapshot_file(f, paste0(nm, ".docx"))
  }
})


test_that("html snapshots", {
  tables <- make_tables()
  for (nm in names(tables)) {
    f <- tempfile(pattern = nm, fileext = ".html")
    if (nm %in% c("table_caption_tests", "table_position_tests", "table_width_tests")) {
      do.call(quick_html, c(tables[[nm]], file = f, open = FALSE))
    } else {
      quick_html(tables[[nm]], file = f, open = FALSE)
    }
    expect_snapshot_file(f, paste0(nm, ".html"))
  }
})


test_that("typst png snapshots", {
  skip_without_typst()
  tables <- make_tables()
  for (nm in names(tables)) {
    f <- tempfile(pattern = nm, fileext = "")  # No extension for PNG prefix
    if (nm %in% c("table_caption_tests", "table_position_tests", "table_width_tests")) {
      do.call(quick_typst_png, c(tables[[nm]], file = f, open = FALSE))
      # PNG creates files with -1, -2 etc. suffixes, find the first one
      png_files <- list.files(dirname(f), pattern = paste0("^", basename(f), ".*\\.png$"), full.names = TRUE)
      if (length(png_files) > 0) {
        expect_snapshot_file(png_files[1], paste0(nm, ".png"))
      }
    } else {
      quick_typst_png(tables[[nm]], file = f, open = FALSE)
      png_file <- paste0(f, ".png")
      if (file.exists(png_file)) {
        expect_snapshot_file(png_file, paste0(nm, ".png"))
      }
    }
  }
})


test_that("typst svg snapshots", {
  skip_without_typst()
  tables <- make_tables()
  for (nm in names(tables)) {
    f <- tempfile(pattern = nm, fileext = "")  # No extension for SVG prefix
    if (nm %in% c("table_caption_tests", "table_position_tests", "table_width_tests")) {
      do.call(quick_typst_svg, c(tables[[nm]], file = f, open = FALSE))
      # SVG creates files with -1, -2 etc. suffixes, find the first one
      svg_files <- list.files(dirname(f), pattern = paste0("^", basename(f), ".*\\.svg$"), full.names = TRUE)
      if (length(svg_files) > 0) {
        expect_snapshot_file(svg_files[1], paste0(nm, ".svg"))
      }
    } else {
      quick_typst_svg(tables[[nm]], file = f, open = FALSE)
      svg_file <- paste0(f, ".svg")
      if (file.exists(svg_file)) {
        expect_snapshot_file(svg_file, paste0(nm, ".svg"))
      }
    }
  }
})
