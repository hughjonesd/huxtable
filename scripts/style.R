#!/usr/bin/env Rscript

if (!requireNamespace("styler", quietly = TRUE)) {
  stop("styler package is required. Install it via install.packages('styler')")
}

styler::style_pkg()
