
context('Fuzzy tests')


expect_outputs_unchanged <- function (hx, idx) {
  info <- paste0("Index i = ", idx)
  file <- file.path(test_path(), "example-rds", paste0("various-outputs-", idx))
  # setting the width avoids problems that command line and RStudio tests have different
  # options(width)
  expect_known_value(to_screen(hx, min_width = 20, max_width = 80),
        file = paste0(file, "-screen.rds"), info = info)
  expect_known_value(to_md(hx, min_width = 20, max_width = 80), file = paste0(file, "-md.rds"),
        info = info)
  expect_known_value(to_html(hx),   file = paste0(file, "-html.rds"),   info = info)
  expect_known_value(to_latex(hx),  file = paste0(file, "-latex.rds"),  info = info)
}

variations <- expand.grid(
  align             = c('left', 'right', 'centre', '.'),
  background_color  = c('red', grey(.6), NA),
  bold              = c(TRUE, FALSE),
  escape_contents   = c(TRUE, FALSE),
  font_size         = c(8, 10),
  italic            = c(TRUE, FALSE),
  left_border       = c(0, 1, 2),
  left_border_color = c('red', grey(.6)),
  left_padding      = c(0, 4),
  number_format     = c("%3.1g", NA),
  rotation          = c(0, 90),
  text_color        = c('red', grey(.6)),
  valign            = c('top', 'middle', 'bottom'),
  wrap              = c(TRUE, FALSE),
  stringsAsFactors  = FALSE,
  KEEP.OUT.ATTRS   = FALSE
)


hx_raw <- hux(
  int  = 1:3,
  real = 1:3 + 0.005,
  char = letters[1:3],
  date = as.Date(1:3, origin = "1970-01-01"),
  fact = factor(letters[4:6])
)

add_props <- function(hx, row) {
  props <- as.list(row)
  props$ht <- hx
  hx_set <- do.call("set_cell_properties", props)

  return(hx_set)
}

test_that('various outputs unchanged', {
  skip_on_R_CMD_check()

  RNGversion("3.3.0")
  set.seed(271075L) # expect_unchanged is useless if we always pick new variations
  for (i in sample(nrow(variations), 300)) {
    hx_set <- add_props(hx_raw, variations[i,])
    expect_outputs_unchanged(hx_set, i)
  }
})


test_that('Some random TeX outputs compile', {
  skip_on_R_CMD_check()

  on.exit({
    try(file.remove(pdf_out), silent = TRUE)
  })
  pdf_out <- character(0)
  for (i in sample(nrow(variations), 10)) {
    hx_set <- add_props(hx_raw, variations[i,])
    pdf_out[as.character(i)] <- pdfo <- sprintf('tex-check-%d.pdf', i)
    expect_error(quick_pdf(hx_set, file = pdfo, open = FALSE),
          regexp = NA,
          info = list(index = i))
    expect_true(file.exists(pdfo), info = list(index = i))
  }
})


test_that('Some random HTML outputs are validated by W3C', {
  skip_on_R_CMD_check()
  skip_if_not_installed('httr')
  library(httr)

  # here we do randomize
  for (i in sample(nrow(variations), 10)) {
    hx_set <- add_props(hx_raw, variations[i,])
    webpage <- paste0("<!DOCTYPE html><html lang=\"en\">",
      "<head><meta charset=\"utf-8\"><title>huxtable table validation</title></head>",
      "<body>\n", to_html(hx_set), "\n</body></html>")
    response <- httr::POST("http://validator.w3.org/nu/?out=json", body = webpage,
          httr::content_type("text/html"))
    response <- httr::content(response, "parsed")
    errors   <- Filter(function (x) x$type == 'error', response$messages)
    warnings <- Filter(function (x) x$type == 'warnings', response$messages)
    valid <- length(errors) == 0
    expect_true(valid, info = list(index = i, errors = errors, warnings = warnings))
  }
})
