
context('Fuzzy tests')


expect_outputs_unchanged <- function (hx, idx) {
  info <- paste0("Index i = ", idx)
  file <- file.path(test_path(), "example-rds", paste0("various-outputs-", idx))
  expect_known_value(to_screen(hx), file = paste0(file, "-screen.rds"), info = info)
  expect_known_value(to_md(hx),     file = paste0(file, "-md.rds"),     info = info)
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

test_that('various outputs unchanged', {
  skip_on_R_CMD_check()
  hx <- hux(
    int  = 1:3,
    real = 1:3 + 0.005,
    char = letters[1:3],
    date = as.Date(1:3, origin = "1970-01-01"),
    fact = factor(letters[4:6])
  )

  RNGversion("3.3.0")
  set.seed(271075L) # expect_unchanged is useless if we always pick new variations
  for (i in sample(nrow(variations), 300)) {
    props <- as.list(variations[i,])
    props$ht <- hx
    hx_set <- do.call("set_cell_properties", props)
    expect_outputs_unchanged(hx_set, i)
  }
})


test_that('Some random outputs are validated by W3C', {
  skip_on_R_CMD_check()
  skip_if_not_installed('httr')
  library(httr)

  hx <- hux(
    int  = 1:3,
    real = 1:3 + 0.005,
    char = letters[1:3],
    date = as.Date(1:3, origin = "1970-01-01"),
    fact = factor(letters[4:6])
  )
  # here we do randomize
  for (i in sample(nrow(variations), 20)) {
    props <- as.list(variations[i,])
    props$ht <- hx
    hx_set <- do.call("set_cell_properties", props)
    webpage <- paste0("<!DOCTYPE html><html lang=\"en\">",
      "<head><meta charset=\"utf-8\"><title>huxtable table validation</title></head>",
      "<body>\n", to_html(hx_set), "\n</body></html>")
    response <- httr::POST("http://validator.w3.org/nu/?out=json", body = webpage,
          content_type("text/html"))
    response <- content(response, "parsed")
    errors   <- Filter(function (x) x$type == 'error', response$messages)
    warnings <- Filter(function (x) x$type == 'warnings', response$messages)
    valid <- length(errors) == 0
    expect_true(valid, info = list(index = i, errors = errors, warnings = warnings))
  }
})
