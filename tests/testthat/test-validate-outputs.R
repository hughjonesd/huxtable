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
  col_width(text_alignment) <- rep(0.33, 3)
  # Set row heights to 2em
  row_height(text_alignment) <- c(0.1, 0.3, 0.3, 0.3)
  # Set absolute width and height for the table
  width(text_alignment) <- 1
  height(text_alignment) <- 0.5
  # Apply alignments systematically
  align(text_alignment)[2:4, 1] <- "left"
  align(text_alignment)[2:4, 2] <- "center"
  align(text_alignment)[2:4, 3] <- "right"

  valign(text_alignment)[2, 1:3] <- "top"
  valign(text_alignment)[3, 1:3] <- "middle"
  valign(text_alignment)[4, 1:3] <- "bottom"
  caption(text_alignment) <- "Text alignment: align=(left, center, right) × valign=(top, middle, bottom)"

  # Text effects: wrap, rotation, padding
  text_effects <- hux(
    "Wrap 1" = c("wrap=TRUE", ""),
    "Wrap 2" = c("", "wrap=FALSE"),
    "Rotation" = c("normal", "rotation=90"),
    "Padding" = c("normal", "padding=10px"),
    add_colnames = TRUE
  )
  text_effects <- set_all_borders(text_effects, value = 1)
  col_width(text_effects) <- c(0.25, 0.25, 0.2, 0.3)
  width(text_effects) <- 0.8
  row_height(text_effects) <- c(0.2, 0.4, 0.4)

  long_text <- "This is a very long text that should demonstrate text wrapping behavior when wrap is enabled versus disabled"
  text_effects[2, 1] <- long_text
  text_effects[3, 2] <- long_text
  wrap(text_effects)[2, 1] <- TRUE
  wrap(text_effects)[3, 2] <- FALSE

  rotation(text_effects)[3, 3] <- 90

  padding_text <- "This text demonstrates padding effects with longer content to show the spacing difference"
  text_effects[2, 4] <- padding_text
  text_effects[3, 4] <- padding_text

  text_effects <- set_all_padding(text_effects, 2, 4, 10)

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
  col_width(dimensions) <- c(0.2, 0.3, 0.35, 0.15) # Total = 1.0
  row_height(dimensions) <- c(0.15, 0.15, 0.15, 0.15, 0.4) # Different heights for all rows including header
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
      caption(ht) <- "Table 4: caption_width=0.8 - This is a longer caption to demonstrate width constraint"
      caption_width(ht) <- 0.8
      ht
    }
  )

  # Table position properties - multiple tables in one document
  table_pos_tests <- list(
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
    "Property" = c("number_format=0", "number_format=%.2f", "fmt_percent()", "na_string=missing", "escape_contents=FALSE"),
    "Raw value" = c(1234.5678, 3.14159, 0.2345, NA, "HTML &copy;"),
    "Formatted" = c(1234.5678, 3.14159, 0.2345, NA, "HTML &copy;"),
    "Decimal align" = c(3, 3.15, -42.3, 123.456, 7),
    add_colnames = TRUE
  )
  content_format <- set_all_borders(content_format, value = 1)
  number_format(content_format)[2, 3] <- 0 # integer format
  number_format(content_format)[3, 3] <- "%.2f" # 2 decimal places
  number_format(content_format)[4, 3] <- fmt_percent(digits = 1)
  na_string(content_format)[5, 3] <- "missing"
  escape_contents(content_format)[6, 3] <- FALSE
  # Set decimal alignment for the fourth column
  align(content_format)[, 4] <- "."
  caption(content_format) <- "Content formatting: number_format, na_string, escape_contents, decimal align"

  # Cell spanning: colspan and rowspan
  spanning <- hux(
    c("normal cell", "normal", "rowspan=2", "INVISIBLE!", "normal"),
    c("normal cell", "colspan=2", "normal", "spans both ways", "INVISIBLE!"),
    c("normal", "INVISIBLE!", "normal", "INVISIBLE!", "normal"),
    c("normal", "normal", "normal", "normal", "normal")
  )
  spanning <- set_all_borders(spanning, value = 1)
  colspan(spanning)[2, 2] <- 2
  rowspan(spanning)[3, 1] <- 2
  colspan(spanning)[4, 2] <- 2
  rowspan(spanning)[4, 2] <- 2
  caption(spanning) <- "Cell spanning: colspan=2, rowspan=2, combined colspan+rowspan"

  list(
    text_properties = text_props,
    text_alignment = text_alignment,
    text_effects = text_effects,
    borders = borders_table,
    dimensions = dimensions,
    table_caption_tests = table_caption_tests,
    table_pos_tests = table_pos_tests,
    table_width_tests = table_width_tests,
    content_formatting = content_format,
    cell_spanning = spanning
  )
}

tables <- make_tables()

#' Determine base file path for output files
#'
#' Generates the base path where output files should be created, handling
#' special cases for Typst image outputs that don't use file extensions.
#'
#' @param table_name Name of the table being tested
#' @param file_ext File extension (e.g., ".html", ".tex")
#' @param is_typst_image Is this a typst image format?
#' @return Character string with base file path
#' @noRd
determine_base_path <- function(table_name, file_ext, is_typst_image) {
  if (file_ext == "" && is_typst_image) {
    file.path(tempdir(), table_name)
  } else {
    file.path(tempdir(), paste0(table_name, file_ext))
  }
}

#' Generate output files using quick functions
#'
#' Calls the appropriate quick function to generate output files, handling both
#' single tables and multi-table lists uniformly using do.call.
#'
#' @param table_data Huxtable or list of huxtables to output
#' @param quick_func Quick function to use (e.g., quick_html, quick_latex)
#' @param base_path Base file path for output
#' @param table_name Name of table being processed
#' @noRd
generate_output_files <- function(table_data, quick_func, base_path, table_name) {
  multi_table_names <- c("table_caption_tests", "table_pos_tests", "table_width_tests")
  # Wrap single huxtables in a list so we can always use do.call
  if (!table_name %in% multi_table_names) {
    table_data <- list(table_data)
  }

  do.call(quick_func, c(table_data, file = base_path, open = FALSE))
}

#' Discover actual output files created by quick functions
#'
#' Finds the files that were actually created, handling special cases like
#' Typst image outputs that create multiple numbered files.
#'
#' @param base_path Base file path that was used for generation
#' @param is_typst_image Flag indicating if this is a typst image function
#' @param file_ext File extension that was used
#' @param snapshot_suffix Suffix for snapshot files
#' @return Character vector of actual output file paths
#' @noRd
discover_output_files <- function(base_path, is_typst_image, file_ext, snapshot_suffix) {
  if (is_typst_image) {
    # For PNG/SVG, find files with suffixes
    # the last backslashes here escape the "." of the snapshot_suffix
    file_pattern <- paste0("^", basename(base_path), "(-\\d+)?\\", snapshot_suffix, "$")
    output_files <- list.files(path = dirname(base_path), pattern = file_pattern, full.names = TRUE)
  } else {
    output_files <- base_path
  }
  output_files <- Filter(file.exists, output_files)
  return(output_files)
}

#' Test output snapshots for different formats
#'
#' Main function to test huxtable output generation across different formats.
#' Generates output files, discovers actual files created, and creates snapshots.
#'
#' @param quick_func Quick function to test (e.g., quick_html, quick_latex)
#' @param file_ext File extension for outputs (e.g., ".html", ".tex").
#'   This is "" for image files because `quick_typst_svg/png` just takes a prefix
#' @param snapshot_suffix Suffix to add to snapshot file names
#' @noRd
test_output_format <- function(quick_func, file_ext, snapshot_suffix = "") {

  if (grepl("typst|docx", deparse(substitute(quick_func)))) {
    # reproducibility
    Sys.setenv(SOURCE_DATE_EPOCH = "1704110400") # 2024-01-01 12:00:00 UTC
  }

  # Determine if this is a typst image function
  is_typst_image <- grepl("typst_(png|svg)", deparse(substitute(quick_func)))

  platform <- utils::sessionInfo()$platform

  for (table_name in names(tables)) {
    base_path <- determine_base_path(table_name = table_name, file_ext = file_ext,
                                     is_typst_image = is_typst_image)

    generate_output_files(table_data = tables[[table_name]], quick_func = quick_func,
                         base_path = base_path, table_name = table_name)

    output_files <- discover_output_files(base_path = base_path, is_typst_image = is_typst_image,
                                         file_ext = file_ext, snapshot_suffix = snapshot_suffix)

    # Use lapply to create snapshots for all files
    lapply(output_files, function(file) {
      snapshot_file <- basename(file)
      file_pattern <- paste0("\\", file_ext, "$") # "\\" escapes the leading dot
      snapshot_file <- sub(file_pattern, snapshot_suffix, snapshot_file)
      expect_snapshot_file(path = file, name = snapshot_file, variant = platform)
    })
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

test_that("docx snapshots", {
  # No way to make reproducible, you just have to manually compare them
  # if you want to
  skip_on_ci()
  skip("Can't make docx reproducible")
  test_output_format(quick_docx, ".docx", ".docx")
})

validate_html_file <- function(file_path) {
  html_content <- paste(readLines(file_path, warn = FALSE), collapse = "\n")

  # Use filterpattern to exclude known false positives at the validator source
  # The Nu validator incorrectly flags <style> in <body> as an error,
  # but this is allowed per WHATWG HTML specification
  filter_pattern <- ".*Element.*style.*not allowed.*"
  validator_url <- paste0("https://validator.w3.org/nu/?out=json&filterpattern=",
                         utils::URLencode(filter_pattern, reserved = TRUE))

  response <- tryCatch({
    httr::POST(
      validator_url,
      body = html_content,
      httr::content_type("text/html; charset=utf-8")
    )
  }, error = function(e) {
    skip(paste("Validation request failed for", basename(file_path), ":", conditionMessage(e)))
  })

  if (httr::status_code(response) != 200) {
    skip(paste("Validator returned status", httr::status_code(response), "for", basename(file_path)))
  }

  result <- httr::content(response, as = "parsed", type = "application/json")
  messages <- result$messages
  messages <- data.frame(
    type = vapply(messages, `[[`, "type", FUN.VALUE = character(1L)),
    message = vapply(messages, `[[`, "message", FUN.VALUE = character(1L))
  )

  return(messages)
}

test_that("html snapshots", {
  # Enhanced HTML quick function that includes W3C validation
  quick_html_with_validation <- function(..., file, open = "ignored") {
    # Generate HTML file using standard quick_html
    quick_html(..., file = file, open = open)

    # Validate the generated HTML file
    skip_if_not_installed("httr")
    skip_if_not_installed("jsonlite")

    # Test validator accessibility
    validator_accessible <- tryCatch({
      response <- httr::HEAD("https://validator.w3.org/nu/")
      httr::status_code(response) < 400
    }, error = function(e) FALSE)

    if (validator_accessible) {
      messages <- validate_html_file(file)

      if (nrow(messages) > 0) {
        # Separate errors from warnings/info
        errors <- messages[messages$type == "error", ]
        warnings <- messages[messages$type %in% c("warning", "info"), ]

        # Report warnings but don't fail
        if (nrow(warnings) > 0) {
          warning_msgs <- paste0("Line ", warnings$lastLine, ": ", warnings$message)
          message("HTML validation warnings in ", basename(file), ": ",
                  paste(warning_msgs, collapse = "; "))
        }

        # Fail on errors (false positives already filtered at source)
        if (nrow(errors) > 0) {
          error_msgs <- paste0("Line ", errors$lastLine, ": ", errors$message)
          fail(paste("HTML validation errors in", basename(file), ":",
                     paste(error_msgs, collapse = "; ")))
        }
      }
    }
  }

  test_output_format(quick_html_with_validation, ".html", ".html")
})

test_that("typst png snapshots", {
  skip_without_typst()
  test_output_format(quick_typst_png, "", ".png")
})

test_that("typst svg snapshots", {
  skip_without_typst()
  test_output_format(quick_typst_svg, "", ".svg")
})

test_that("screen snapshots", {
  # Helper function to create screen output files
  quick_screen <- function(..., file, open = "ignored") {
    local_reproducible_output(crayon = TRUE) # lets crayon work in to_screen
    args <- list(...)

    if (file.exists(file)) file.remove(file)
    for (obj in args) {
      cat(to_screen(obj, min_width = 20, max_width = 80, color = TRUE),
        file = file, append = TRUE
      )
      cat("\n\n", file = file, append = TRUE)
    }
  }

  test_output_format(quick_screen, ".txt", ".txt")
})

