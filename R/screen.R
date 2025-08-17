#' @import assertthat
#' @importFrom stats na.omit
NULL

#' @export
#' @rdname to_screen
print_screen <- function(ht, ...) cat(to_screen(ht, ...))


#' Print a huxtable on screen
#'
#' @param ht A huxtable.
#' @param ... Passed on to `to_screen`.
#' @param min_width Minimum width in on-screen characters of the result.
#' @param max_width Maximum width in on-screen characters of the result. Overrides `min_width`.
#' @param compact Logical. To save space, don't print lines for empty horizontal borders.
#' @param colnames Logical. Whether or not to print colum names.
#' @param color Logical. Whether to print the huxtable in color (requires the `crayon` package).
#'
#' @return `to_screen` returns a string. `print_screen` prints the string and returns `NULL`.
#'
#' @details
#' Screen display shows the following features:
#'
#' * Table and caption positioning
#' * Merged cells
#' * Cell alignment
#' * Borders
#' * Cell background and border color (if the "crayon" package is installed)
#' * Text color, bold and italic (if the "crayon" package is installed)
#'
#' Cell padding, widths and heights are not shown.
#'
#' @export
#' @family printing functions
#' @examples
#' bottom_border(jams)[1, 1:2] <- 1
#' bold(jams)[1, 1:2] <- TRUE
#' jams <- map_text_color(
#'   jams,
#'   by_regex("berry" = "red")
#' )
#'
#' print_screen(jams)
to_screen <- function(ht,
                      min_width = ceiling(getOption("width") / 6),
                      max_width = getOption("width", Inf),
                      compact = TRUE,
                      colnames = TRUE,
                      color = getOption("huxtable.color_screen", default = TRUE),
                      ...) {
  
  # Validate parameters and setup color support
  config <- validate_screen_config(min_width, max_width, compact, colnames, color)
  
  # Handle empty tables
  if (ncol(ht) == 0 || nrow(ht) == 0) {
    return(format_empty_table(ht))
  }
  
  # Generate the core table display
  table_result <- generate_table_display(ht, config$min_width, config$max_width, config$color, compact)
  
  # Add caption if present
  result <- add_caption_if_present(table_result$content, ht, config$max_width, table_result$char_matrix_ncol)
  
  # Add column names if requested
  result <- add_column_names_if_requested(result, ht, config$colnames, config$max_width, table_result$last_col, ncol(ht))
  
  result
}


# Validate parameters and check color support
validate_screen_config <- function(min_width, max_width, compact, colnames, color) {
  assert_that(is.number(min_width), is.number(max_width), is.flag(compact), is.flag(colnames), is.flag(color))
  
  if (color && !requireNamespace("crayon", quietly = TRUE)) {
    warning(
      "On-screen color requires the `crayon` package. Run:\n",
      "install.packages(\"crayon\")\n",
      "or set `options(huxtable.color_screen = FALSE)`."
    )
    color <- FALSE
  }
  
  list(min_width = min_width, max_width = max_width, compact = compact, colnames = colnames, color = color)
}


# Handle empty tables
format_empty_table <- function(ht) {
  glue::glue("<huxtable with {nrow(ht)} rows and {ncol(ht)} columns>\n")
}


# Generate the main table display with borders and colors
generate_table_display <- function(ht, min_width, max_width, color, compact) {
  # Generate character matrix representation
  charmat_data <- create_character_matrix(ht, min_width, max_width, color)
  
  # Add borders to the character matrix
  charmat_with_borders <- add_borders_to_matrix(ht, charmat_data, color)
  
  # Remove empty border rows if compact mode
  final_charmat <- apply_compact_formatting(charmat_with_borders, compact, charmat_data$border_rows)
  
  # Convert matrix to positioned string
  result <- format_matrix_to_string(final_charmat, ht, max_width, charmat_data$width_mat)
  
  list(
    content = result,
    char_matrix_ncol = ncol(final_charmat),
    last_col = charmat_data$last_ht_col
  )
}


# Create the basic character matrix without borders
create_character_matrix <- function(ht, min_width, max_width, color) {
  character_matrix(
    ht = ht,
    inner_border_h = 3, 
    inner_border_v = 1, 
    outer_border_h = 2, 
    outer_border_v = 1,
    min_width = min_width, 
    max_width = max_width, 
    color = color, 
    markdown = FALSE
  )
}


# Add border characters to the matrix
add_borders_to_matrix <- function(ht, charmat_data, color) {
  charmat <- charmat_data$charmat
  border_rows <- charmat_data$border_rows
  border_cols <- charmat_data$border_cols
  last_ht_col <- charmat_data$last_ht_col
  
  # Subset huxtable to displayed columns
  ht <- ht[, seq_len(last_ht_col)]
  border_cols[-1] <- border_cols[-1] + 1
  
  # Create border matrices
  border_info <- create_border_matrices(ht, charmat, border_rows, border_cols)
  
  # Apply border characters
  charmat <- apply_border_characters(charmat, border_info$border_mat, border_info$vstyles, border_info$hstyles)
  
  # Apply border colors if enabled
  if (color) {
    charmat <- apply_border_colors(charmat, ht, border_rows, border_cols)
  }
  
  charmat
}


# Create matrices tracking border presence and styles
create_border_matrices <- function(ht, charmat, border_rows, border_cols) {
  borders <- get_visible_borders(ht)
  bstyles <- collapsed_border_styles(ht)
  border_mat <- matrix(1L, nrow = nrow(charmat), ncol = ncol(charmat))
  vstyles <- matrix(NA_character_, nrow = nrow(charmat), ncol = ncol(charmat))
  hstyles <- matrix(NA_character_, nrow = nrow(charmat), ncol = ncol(charmat))
  
  # Map huxtable rows/cols to character matrix regions
  index_rows <- lapply(seq_len(nrow(ht)), function(x) seq(border_rows[x], border_rows[x + 1] - 1))
  index_cols <- lapply(seq_len(ncol(ht)), function(x) seq(border_cols[x], border_cols[x + 1] - 1))
  
  # Process vertical and horizontal borders
  for (i in seq_len(nrow(ht) + 1)) {
    for (j in seq_len(ncol(ht) + 1)) {
      if (i <= nrow(ht) && borders$vert[i, j] > 0) {
        ir <- index_rows[[i]]
        border_mat[ir, border_cols[j]] <- border_mat[ir, border_cols[j]] + 1L
        border_mat[ir + 1, border_cols[j]] <- border_mat[ir + 1, border_cols[j]] + 2L
        vstyles[ir, border_cols[j]] <- bstyles$vert[i, j]
        vstyles[ir + 1, border_cols[j]] <- bstyles$vert[i, j]
      }
      if (j <= ncol(ht) && borders$horiz[i, j] > 0) {
        ic <- index_cols[[j]]
        border_mat[border_rows[i], ic] <- border_mat[border_rows[i], ic] + 4L
        border_mat[border_rows[i], ic + 1] <- border_mat[border_rows[i], ic + 1] + 8L
        hstyles[border_rows[i], ic] <- bstyles$horiz[i, j]
        hstyles[border_rows[i], ic + 1] <- bstyles$horiz[i, j]
      }
    }
  }
  
  list(border_mat = border_mat, vstyles = vstyles, hstyles = hstyles)
}


# Apply Unicode border characters based on border codes
apply_border_characters <- function(charmat, border_mat, vstyles, hstyles) {
  pipe_chars <- get_unicode_border_chars()
  pipe_mat <- matrix(NA_character_, nrow = nrow(border_mat), ncol = ncol(border_mat))
  
  for (r in seq_len(nrow(border_mat))) {
    for (c in seq_len(ncol(border_mat))) {
      code <- border_mat[r, c]
      if (code > 1) {
        style <- determine_border_style(vstyles[r, c], hstyles[r, c])
        pc <- pipe_chars[[style]][code]
        if (is.na(pc)) pc <- pipe_chars$solid[code]
        pipe_mat[r, c] <- pc
      }
    }
  }
  
  charmat[!is.na(pipe_mat)] <- pipe_mat[!is.na(pipe_mat)]
  charmat
}


# Get Unicode characters for different border styles
get_unicode_border_chars <- function() {
  list(
    solid = c(
      NA,
      "\u2502", "\u2502", "\u2502", "\u2500",
      "\u250c", "\u2514", "\u251c", "\u2500",
      "\u2510", "\u2518", "\u2524", "\u2500",
      "\u252c", "\u2534", "\u253c"
    ),
    double = c(
      NA,
      "\u2551", "\u2551", "\u2551", "\u2550",
      "\u2554", "\u255a", "\u2560", "\u2550",
      "\u2557", "\u255d", "\u2563", "\u2550",
      "\u2566", "\u2569", "\u256c"
    ),
    dashed = c(
      NA,
      "\u2506", "\u2506", "\u2506", "\u2504",
      "\u250c", "\u2514", "\u251c", "\u2504",
      "\u2510", "\u2518", "\u2524", "\u2504",
      "\u252c", "\u2534", "\u253c"
    ),
    dotted = c(
      NA,
      "\u250a", "\u250a", "\u250a", "\u2508",
      "\u250c", "\u2514", "\u251c", "\u2508",
      "\u2510", "\u2518", "\u2524", "\u2508",
      "\u252c", "\u2534", "\u253c"
    )
  )
}


# Determine border style from vertical and horizontal styles
determine_border_style <- function(vstyle, hstyle) {
  if (!is.na(vstyle) && (is.na(hstyle) || vstyle == hstyle)) {
    vstyle
  } else if (!is.na(hstyle) && is.na(vstyle)) {
    hstyle
  } else if (!is.na(vstyle) && !is.na(hstyle) && vstyle == hstyle) {
    vstyle
  } else {
    "solid"
  }
}


# Apply colors to border characters
apply_border_colors <- function(charmat, ht, border_rows, border_cols) {
  bcolors <- collapsed_border_colors(ht)
  unique_cols <- unique(na.omit(unlist(bcolors)))
  col_funs <- lapply(unique_cols, crayon::make_style)
  names(col_funs) <- unique_cols
  
  index_rows <- lapply(seq_len(nrow(ht)), function(x) seq(border_rows[x], border_rows[x + 1] - 1))
  index_cols <- lapply(seq_len(ncol(ht)), function(x) seq(border_cols[x], border_cols[x + 1] - 1))
  
  for (i in seq_len(nrow(ht) + 1)) {
    for (j in seq_len(ncol(ht) + 1)) {
      if (i <= nrow(ht) && !is.na(bcolors$vert[i, j])) {
        ir <- index_rows[[i]]
        color_fun <- col_funs[[bcolors$vert[i, j]]]
        charmat[ir, border_cols[j]] <- color_fun(charmat[ir, border_cols[j]])
      }
      if (j <= ncol(ht) && !is.na(bcolors$horiz[i, j])) {
        ic <- c(index_cols[[j]], max(index_cols[[j]]) + 1)
        color_fun <- col_funs[[bcolors$horiz[i, j]]]
        charmat[border_rows[i], ic] <- color_fun(charmat[border_rows[i], ic])
      }
    }
  }
  
  charmat
}


# Remove empty horizontal border rows in compact mode
apply_compact_formatting <- function(charmat, compact, border_rows) {
  if (!compact) return(charmat)
  
  empty_borders <- apply(charmat, 1, function(x) {
    all(grepl(" ", x, fixed = TRUE) | grepl("[\u2502\u2551\u2506\u250a]", x))
  })
  empty_borders <- intersect(border_rows, which(empty_borders))
  
  if (length(empty_borders) > 0) {
    charmat <- charmat[-empty_borders, , drop = FALSE]
  }
  
  charmat
}


# Convert character matrix to positioned string output
format_matrix_to_string <- function(charmat, ht, max_width, width_mat) {
  result <- apply(charmat, 1, paste0, collapse = "")
  
  # Calculate padding for table positioning
  width_mat <- pmax(width_mat, 1)
  width_mat[charmat == ""] <- 0
  row_char_widths <- rowSums(width_mat)
  pad_width <- min(max_width, getOption("width", 80)) - max(row_char_widths)
  
  pad_width <- switch(position_no_wrap(ht),
    "left"   = 0,
    "right"  = pad_width,
    "center" = floor(pad_width / 2)
  )
  
  result <- paste0(strrep(" ", max(pad_width, 0)), result)
  paste0(paste(result, collapse = "\n"), "\n")
}


# Add caption to output if present
add_caption_if_present <- function(result, ht, max_width, charmat_ncol) {
  cap <- caption(ht)
  if (is.na(cap)) return(result)
  
  # Determine horizontal position for caption
  poss_pos <- c("left", "center", "right")
  hpos <- if (any(found <- sapply(poss_pos, grepl, x = caption_pos(ht)))) {
    poss_pos[found]
  } else {
    position_no_wrap(ht)
  }
  
  # Wrap caption if too wide
  if (ncharw(cap) > max_width) cap <- strwrap(cap, max_width)
  
  # Format caption with appropriate padding
  cap <- format_caption_text(cap, hpos, charmat_ncol)
  cap <- paste0(pad_position(cap, position_no_wrap(ht), max_width), collapse = "\n")
  cap <- paste0(cap, "\n")
  
  # Position caption above or below table
  if (grepl("top", caption_pos(ht))) {
    paste0(cap, result)
  } else {
    paste0(result, cap)
  }
}


# Format caption text with appropriate alignment
format_caption_text <- function(cap, hpos, charmat_ncol) {
  stringr_side <- switch(hpos,
    left = "right",
    right = "left",
    center = "both"
  )
  cap <- stringr::str_pad(cap, charmat_ncol - 4, stringr_side)
  stringr::str_pad(cap, charmat_ncol, "both")
}


# Add column names to output if requested
add_column_names_if_requested <- function(result, ht, show_colnames, max_width, last_col, orig_ncol) {
  all_colnames <- colnames(ht)
  
  if (show_colnames && any(nzchar(all_colnames))) {
    colnames_text <- paste0("Column names: ", paste(all_colnames, collapse = ", "))
    colnames_text <- strwrap(colnames_text, max_width)
    colnames_text <- paste0(colnames_text, collapse = "\n")
    result <- paste0(result, "\n", colnames_text, "\n")
  }
  
  if (last_col < orig_ncol) {
    result <- glue::glue("{result}\n{last_col}/{orig_ncol} columns shown.\n")
  }
  
  result
}


# Improved character_matrix function with better structure
character_matrix <- function(ht,
                             inner_border_h,
                             inner_border_v,
                             outer_border_h,
                             outer_border_v,
                             min_width,
                             max_width = Inf,
                             color = FALSE,
                             markdown = FALSE) {
  if (ncol(ht) == 0) stop("Couldn't display any columns in less than max_width characters.")
  
  # Validate inputs
  assert_that(is.flag(markdown), is.flag(color))

  # Prepare cell data and calculate initial dimensions
  cell_data <- prepare_cell_display_data(ht, markdown)
  column_widths <- calculate_column_widths(ht, cell_data, min_width, max_width, inner_border_h, outer_border_h, markdown)
  
  # Check if we need to drop columns due to width constraints
  if (is.null(column_widths)) {
    return(character_matrix(ht[, -ncol(ht)], inner_border_h, inner_border_v, outer_border_h,
      outer_border_v, min_width, max_width, color = color, markdown = markdown))
  }
  
  # Format cell contents to fit in calculated widths
  formatted_cells <- format_cell_contents(cell_data, column_widths, ht, markdown)
  
  # Calculate row heights based on formatted content
  row_heights <- calculate_row_heights(formatted_cells, ht, inner_border_v)
  
  # Build the final character matrix
  build_final_character_matrix(formatted_cells, column_widths, row_heights, 
                              inner_border_h, inner_border_v, outer_border_h, outer_border_v, 
                              ht, color)
}


# Prepare display cell data with content
prepare_cell_display_data <- function(ht, markdown) {
  dc <- display_cells(ht, all = FALSE)
  dc <- dc[order(dc$colspan), ]
  contents <- clean_contents(ht, output_type = if (markdown) "markdown" else "screen")
  drow_mat <- as.matrix(dc[, c("display_row", "display_col")])
  dc$contents <- contents[drow_mat]
  dc
}


# Calculate optimal column widths
calculate_column_widths <- function(ht, dc, min_width, max_width, inner_border_h, outer_border_h, markdown = FALSE) {
  cw <- col_width(ht)
  if (!is.numeric(cw) || anyNA(cw)) cw <- rep(1, ncol(ht))
  cw <- cw / sum(cw)
  
  min_widths <- ceiling(min_width * cw)
  widths <- min_widths
  
  # Calculate content requirements
  content_widths <- ncharw(dc$contents)
  max_word_widths <- sapply(
    lapply(strsplit(dc$contents, "(\t|\n|\r|\v )"), ncharw),
    function(x) max(c(0, x))
  )
  
  # Expand widths to accommodate content
  for (r in seq_len(nrow(dc))) {
    width <- if (wrap(ht)[dc$display_row[r], dc$display_col[r]]) {
      max_word_widths[r]
    } else {
      content_widths[r]
    }
    
    # Add space for markdown formatting
    if (markdown) {
      if (bold(ht)[dc$display_row[r], dc$display_col[r]]) width <- width + 4
      if (italic(ht)[dc$display_row[r], dc$display_col[r]]) width <- width + 2
    }
    
    cols <- seq(dc$display_col[r], dc$end_col[r])
    if (sum(widths[cols]) + inner_border_h * (dc$colspan[r] - 1) < width) {
      widths[cols] <- pmax(widths[cols], ceiling(width / dc$colspan[r]))
    }
  }
  
  # Constrain to maximum width
  max_widths <- floor(cw * (max_width - 2 * outer_border_h - (ncol(ht) - 1) * inner_border_h))
  if (any(max_widths < 8)) return(NULL)  # Signal that we need fewer columns
  
  pmin(widths, max_widths)
}


# Format cell contents to fit within calculated widths
format_cell_contents <- function(dc, widths, ht, markdown) {
  dc$strings <- vector(nrow(dc), mode = "list")
  
  for (r in seq_len(nrow(dc))) {
    dcell <- dc[r, ]
    col <- dcell$display_col
    end_col <- dcell$end_col
    width <- sum(widths[col:end_col])
    
    # Handle markdown formatting requirements
    md_bold <- isTRUE(markdown && bold(ht)[dcell$display_row, dcell$display_col])
    md_italic <- isTRUE(markdown && italic(ht)[dcell$display_row, dcell$display_col])
    eff_width <- width
    if (md_bold) eff_width <- eff_width - 4
    if (md_italic) eff_width <- eff_width - 2
    
    # Process content for proper wrapping
    content <- process_content_for_wrapping(dcell$contents)
    strings <- fansi::strwrap2_ctl(content, width = eff_width + 1, wrap.always = TRUE)
    strings <- strings[strings != ""]
    if (length(strings) == 0) strings <- ""
    
    # Apply markdown formatting
    if (md_bold) strings[ncharw(strings) > 0] <- paste0("**", strings[ncharw(strings) > 0], "**")
    if (md_italic) strings[ncharw(strings) > 0] <- paste0("*", strings[ncharw(strings) > 0], "*")
    
    # Apply text alignment
    align <- real_align(ht)[dcell$display_row, dcell$display_col]
    stringr_align <- switch(align, "left" = "right", "right" = "left", "center" = "both")
    strings <- col_aware_strpad(strings, width, stringr_align)
    
    dc$strings[[r]] <- strings
  }
  
  # Calculate text dimensions
  dc$text_height <- sapply(dc$strings, length)
  dc$text_width <- sapply(dc$strings, function(x) max(ncharw(x, type = "chars")))
  
  dc
}


# Process content for proper text wrapping
process_content_for_wrapping <- function(content) {
  # Convert trailing spaces to non-breaking spaces for decimal padding
  while (!identical(new <- gsub("( |\u00a0)$", "\u00a0", content), content)) {
    content <- new
  }
  # Handle paragraph breaks
  gsub("\n", "\n\n", content, fixed = TRUE)
}


# Calculate row heights based on formatted content
calculate_row_heights <- function(dc, ht, inner_border_v) {
  dc <- dc[order(dc$rowspan), ]
  heights <- rep(1, nrow(ht))
  
  for (r in seq_len(nrow(dc))) {
    dcell <- dc[r, ]
    rows <- seq(dcell$display_row, dcell$end_row)
    if (sum(heights[rows]) + inner_border_v * (dcell$rowspan - 1) < dcell$text_height) {
      heights[rows] <- pmax(heights[rows], ceiling(dcell$text_height / dcell$rowspan))
    }
  }
  
  heights
}


# Build the final character matrix with all content positioned
build_final_character_matrix <- function(dc, widths, heights, inner_border_h, inner_border_v, 
                                       outer_border_h, outer_border_v, ht, color) {
  # Calculate matrix dimensions and positions
  border_widths <- c(outer_border_h, rep(inner_border_h, ncol(ht) - 1), outer_border_h)
  all_widths <- border_widths + c(0, widths)
  starting_cols <- cumsum(all_widths[seq_len(ncol(ht))]) + 1
  border_cols <- c(starting_cols, sum(all_widths) + 1) - border_widths
  
  border_heights <- c(outer_border_v, rep(inner_border_v, nrow(ht) - 1), outer_border_v)
  all_heights <- border_heights + c(0, heights)
  starting_rows <- cumsum(all_heights[seq_len(nrow(ht))]) + 1
  border_rows <- c(starting_rows, sum(all_heights) + 1) - border_heights
  
  # Initialize matrices
  charmat <- matrix(" ", sum(all_heights), sum(all_widths))
  width_mat <- matrix(0, sum(all_heights), sum(all_widths))
  
  # Fill in cell content
  for (r in seq_len(nrow(dc))) {
    dcell <- dc[r, ]
    string_letters <- unlist(col_aware_strsplit(dcell$strings[[1]], ""))
    drow <- dcell$display_row
    dcol <- dcell$display_col
    
    style <- if (color) make_cell_style(ht, drow, dcol) else identity
    rows <- seq(starting_rows[drow], starting_rows[drow] + dcell$text_height - 1)
    cols <- seq(starting_cols[dcol], starting_cols[dcol] + dcell$text_width - 1)
    
    charmat[rows, cols] <- matrix(style(string_letters), length(rows), length(cols), byrow = TRUE)
    width_mat[rows, cols] <- matrix(ncharw(string_letters), length(rows), length(cols), byrow = TRUE)
  }
  
  list(
    charmat     = charmat,
    width_mat   = width_mat,
    border_rows = border_rows,
    border_cols = border_cols,
    last_ht_col = ncol(ht)
  )
}


# Helper functions remain the same
col_aware_strsplit <- function(...) {
  if (requireNamespace("crayon", quietly = TRUE)) {
    crayon::col_strsplit(...)
  } else {
    strsplit(...)
  }
}


col_aware_strpad <- function(string, width, side) {
  if (requireNamespace("crayon", quietly = TRUE)) {
    clean <- crayon::strip_style(string)
    padded <- stringi::stri_pad(clean, width, side = side, use_length = TRUE)
    pads <- stringr::str_match(padded, "^( *).*?( *)$")
    paste0(pads[, 2], string, pads[, 3])
  } else {
    stringi::stri_pad(string, width, side = side, use_length = TRUE)
  }
}


pad_position <- function(string, position, max_width) {
  width <- min(max_width, getOption("width", 80))
  assert_that(position %in% c("left", "center", "right"))
  if (position == "left") return(string)
  
  side <- if (position == "center") "both" else "left"
  stringr::str_pad(string, width, side)
}


make_cell_style <- function(ht, row, col) {
  tc <- text_color(ht)[row, col]
  bgc <- background_color(ht)[row, col]
  bold <- bold(ht)[row, col]
  italic <- italic(ht)[row, col]

  maybe_combine_style <- function(style, style2) {
    if (is.null(style)) style2 else crayon::combine_styles(style, style2)
  }
  style <- NULL
  if (bold) style <- crayon::bold
  if (italic) style <- maybe_combine_style(style, crayon::italic)
  if (!is.na(tc)) style <- maybe_combine_style(style, crayon::make_style(tc))
  if (!is.na(bgc)) style <- maybe_combine_style(style, crayon::make_style(bgc, bg = TRUE))
  style <- style %||% identity

  style
}