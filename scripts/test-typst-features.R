#!/usr/bin/env Rscript

# Test script for Typst features in huxtable
# Tests various properties systematically and outputs PDFs for visual inspection

library(huxtable)

# Create output directory
output_dir <- "scripts/test-typst-pdfs"
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

cat("Testing Typst features systematically...\n")
cat("Output directory:", output_dir, "\n\n")

# Test 1: Comprehensive feature demonstration
cat("Creating comprehensive feature test...\n")
ht_comprehensive <- iris[1:6, 1:4] |> as_hux()

# Add column names
ht_comprehensive <- add_colnames(ht_comprehensive)

# Text formatting
bold(ht_comprehensive)[1, ] <- TRUE                    # Header row bold
italic(ht_comprehensive)[2, 1] <- TRUE                 # First data cell italic
font_size(ht_comprehensive)[3, 2] <- 14               # Larger font
font(ht_comprehensive)[4, 3] <- "Courier"             # Different font family
text_color(ht_comprehensive)[5, 4] <- "red"           # Red text

# Cell alignment
align(ht_comprehensive)[1, ] <- "center"              # Center headers
align(ht_comprehensive)[2:7, 1] <- "left"             # Left align first column
align(ht_comprehensive)[2:7, 2:4] <- "right"          # Right align numeric columns
valign(ht_comprehensive)[3, ] <- "middle"             # Middle vertical alignment
valign(ht_comprehensive)[4, ] <- "bottom"             # Bottom vertical alignment

# Backgrounds and colors
background_color(ht_comprehensive)[1, ] <- "lightblue"   # Header background
background_color(ht_comprehensive)[3, 2] <- "yellow"     # Highlight one cell
background_color(ht_comprehensive)[5, 1] <- "lightgreen" # Another highlight

# Borders
top_border(ht_comprehensive)[1, ] <- brdr(2, "solid", "black")    # Header top border
bottom_border(ht_comprehensive)[1, ] <- brdr(2, "solid", "black") # Header bottom border
left_border(ht_comprehensive)[, 1] <- brdr(1, "dashed", "gray")   # Left column dashed border
right_border(ht_comprehensive)[, 4] <- brdr(1, "dotted", "blue")  # Right column dotted border

# Padding
left_padding(ht_comprehensive)[2, 1] <- 15           # Extra left padding
right_padding(ht_comprehensive)[3, 4] <- 20          # Extra right padding
top_padding(ht_comprehensive)[4, 2] <- 10            # Extra top padding
bottom_padding(ht_comprehensive)[5, 3] <- 12         # Extra bottom padding

# Cell merging
colspan(ht_comprehensive)[2, 2] <- 2                 # Merge horizontally
rowspan(ht_comprehensive)[4, 1] <- 2                 # Merge vertically

# Rotation (if supported)
rotation(ht_comprehensive)[6, 4] <- 45               # Rotate text

# Table properties
caption(ht_comprehensive) <- "Comprehensive Typst Feature Test"
label(ht_comprehensive) <- "tab:comprehensive"
width(ht_comprehensive) <- 0.8
position(ht_comprehensive) <- "center"

# Generate PDF
quick_typst_pdf(ht_comprehensive, file = file.path(output_dir, "01_comprehensive_test.pdf"))

# Test 2: Border styles focus
cat("Creating border styles test...\n")
ht_borders <- matrix(c("Solid", "Dashed", "Dotted", "None",
                       "Thick", "Medium", "Thin", "Mixed"),
                     nrow = 2, ncol = 4, byrow = TRUE) |> as_hux()

# Different border styles
top_border(ht_borders)[1, 1] <- brdr(2, "solid", "black")
top_border(ht_borders)[1, 2] <- brdr(2, "dashed", "red")
top_border(ht_borders)[1, 3] <- brdr(2, "dotted", "blue")
top_border(ht_borders)[1, 4] <- brdr(0, "solid", "black")

bottom_border(ht_borders)[2, 1] <- brdr(3, "solid", "green")
bottom_border(ht_borders)[2, 2] <- brdr(1, "solid", "orange")
bottom_border(ht_borders)[2, 3] <- brdr(0.5, "solid", "purple")
bottom_border(ht_borders)[2, 4] <- brdr(1, "dashed", "black")

left_border(ht_borders)[, 1] <- brdr(1.5, "dotted", "gray")
right_border(ht_borders)[, 4] <- brdr(2.5, "solid", "navy")

caption(ht_borders) <- "Border Styles Test"
quick_typst_pdf(ht_borders, file = file.path(output_dir, "02_border_styles.pdf"))

# Test 3: Text formatting focus
cat("Creating text formatting test...\n")
ht_text <- matrix(c("Normal", "Bold", "Italic", "Both",
                    "Large", "Small", "Colored", "Font",
                    "Left", "Center", "Right", "Rotated"),
                  nrow = 3, ncol = 4, byrow = TRUE) |> as_hux()

bold(ht_text)[1, 2] <- TRUE
italic(ht_text)[1, 3] <- TRUE
bold(ht_text)[1, 4] <- TRUE
italic(ht_text)[1, 4] <- TRUE

font_size(ht_text)[2, 1] <- 16
font_size(ht_text)[2, 2] <- 8
text_color(ht_text)[2, 3] <- "red"
font(ht_text)[2, 4] <- "Times"

align(ht_text)[3, 1] <- "left"
align(ht_text)[3, 2] <- "center"
align(ht_text)[3, 3] <- "right"
rotation(ht_text)[3, 4] <- 90

caption(ht_text) <- "Text Formatting Test"
quick_typst_pdf(ht_text, file = file.path(output_dir, "03_text_formatting.pdf"))

# Test 4: Cell merging focus
cat("Creating cell merging test...\n")
ht_merge <- matrix(1:16, nrow = 4, ncol = 4) |> as_hux()

# Simple horizontal merge
colspan(ht_merge)[1, 1] <- 2
ht_merge[1, 1] <- "Merged Horizontal"

# Simple vertical merge (different area)
rowspan(ht_merge)[3, 3] <- 2
ht_merge[3, 3] <- "Merged Vertical"

background_color(ht_merge)[1, 1] <- "lightblue"
background_color(ht_merge)[3, 3] <- "lightgreen"

caption(ht_merge) <- "Cell Merging Test"
quick_typst_pdf(ht_merge, file = file.path(output_dir, "04_cell_merging.pdf"))

# Test 5: Alignment focus
cat("Creating alignment test...\n")
ht_align <- matrix(c("Top Left", "Top Center", "Top Right",
                     "Middle Left", "Middle Center", "Middle Right",
                     "Bottom Left", "Bottom Center", "Bottom Right"),
                   nrow = 3, ncol = 3, byrow = TRUE) |> as_hux()

# Horizontal alignment
align(ht_align)[, 1] <- "left"
align(ht_align)[, 2] <- "center"
align(ht_align)[, 3] <- "right"

# Vertical alignment
valign(ht_align)[1, ] <- "top"
valign(ht_align)[2, ] <- "middle"
valign(ht_align)[3, ] <- "bottom"

# Make cells taller to see vertical alignment
row_height(ht_align) <- rep("30pt", 3)

# Add borders to see alignment clearly
ht_align <- set_all_borders(ht_align, brdr(1, "solid", "gray"))

caption(ht_align) <- "Alignment Test"
quick_typst_pdf(ht_align, file = file.path(output_dir, "05_alignment.pdf"))

# Test 6: Padding and spacing
cat("Creating padding test...\n")
ht_padding <- matrix(c("Default", "Left+", "Right+", "Top+",
                       "Bottom+", "All+", "Mixed", "None"),
                     nrow = 2, ncol = 4, byrow = TRUE) |> as_hux()

left_padding(ht_padding)[1, 2] <- 20
right_padding(ht_padding)[1, 3] <- 20
top_padding(ht_padding)[1, 4] <- 20
bottom_padding(ht_padding)[2, 1] <- 20

# All padding
left_padding(ht_padding)[2, 2] <- 15
right_padding(ht_padding)[2, 2] <- 15
top_padding(ht_padding)[2, 2] <- 15
bottom_padding(ht_padding)[2, 2] <- 15

# Mixed padding
left_padding(ht_padding)[2, 3] <- 5
right_padding(ht_padding)[2, 3] <- 25
top_padding(ht_padding)[2, 3] <- 10
bottom_padding(ht_padding)[2, 3] <- 30

# Add borders to see padding effect
ht_padding <- set_all_borders(ht_padding, brdr(1, "solid", "black"))

caption(ht_padding) <- "Padding Test"
quick_typst_pdf(ht_padding, file = file.path(output_dir, "06_padding.pdf"))

# Test 7: Color and background test
cat("Creating color test...\n")
ht_color <- matrix(c("Black", "Red", "Blue", "Green",
                     "White", "Yellow", "Orange", "Purple"),
                   nrow = 2, ncol = 4, byrow = TRUE) |> as_hux()

text_color(ht_color)[1, 1] <- "black"
text_color(ht_color)[1, 2] <- "red"
text_color(ht_color)[1, 3] <- "blue"
text_color(ht_color)[1, 4] <- "green"

background_color(ht_color)[2, 1] <- "black"
text_color(ht_color)[2, 1] <- "white"
background_color(ht_color)[2, 2] <- "yellow"
background_color(ht_color)[2, 3] <- "orange"
background_color(ht_color)[2, 4] <- "purple"
text_color(ht_color)[2, 4] <- "white"

caption(ht_color) <- "Color Test"
quick_typst_pdf(ht_color, file = file.path(output_dir, "07_colors.pdf"))

# Test 8: Table-level properties
cat("Creating table properties test...\n")
ht_table <- iris[1:4, 1:3] |> as_hux(add_colnames = TRUE)

# Various table widths and positions
ht_narrow <- ht_table
width(ht_narrow) <- 0.5
position(ht_narrow) <- "left"
caption(ht_narrow) <- "Narrow Table (50% width, left aligned)"

ht_wide <- ht_table
width(ht_wide) <- 0.9
position(ht_wide) <- "right"
caption(ht_wide) <- "Wide Table (90% width, right aligned)"

quick_typst_pdf(ht_narrow, file = file.path(output_dir, "08a_table_narrow.pdf"))
quick_typst_pdf(ht_wide, file = file.path(output_dir, "08b_table_wide.pdf"))

cat("\nAll test PDFs generated successfully!\n")
cat("Check the following files in", output_dir, ":\n")
list.files(output_dir, pattern = "*.pdf", full.names = FALSE) |>
  paste0("  - ", x = _) |>
  cat(sep = "\n")

cat("\nPlease visually inspect each PDF to identify any rendering issues.\n")
