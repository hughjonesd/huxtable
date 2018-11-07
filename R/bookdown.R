
# Functions relating to bookdown


in_bookdown <- function () {
  if (! is.null(book_opt <- getOption("huxtable.bookdown", NULL))) return(book_opt)

  if (! requireNamespace("knitr")) return(FALSE)
  if (! requireNamespace("rmarkdown")) return(FALSE)
  input_path <- knitr::current_input(dir = TRUE)
  if (is.null(input_path)) return(FALSE)
  rmd_of <- rmarkdown::default_output_format(input_path)$name

  return(grepl("bookdown", rmd_of))
}

make_caption <- function(ht, format = c("html", "latex", "md")) {
  format <- match.arg(format)

  raw_cap <- caption(ht)
  label <- label(ht)

  if (is.na(label) || label == "") {
    return(raw_cap)
  }
  if (! in_bookdown()) {
    return(raw_cap)
  }

  if (! grepl("^tab:", label)) label <- paste0("tab:", label)

  # even if there's no caption, we make one if we need it for the label:
  if (is.na(raw_cap)) raw_cap <- ""
  cap_with_label <- if (format == "latex") {
          sprintf("(\\#%s) %s", label, raw_cap)
        } else {
          sprintf("(#%s) %s", label, raw_cap)
        }

  return(cap_with_label)
}
