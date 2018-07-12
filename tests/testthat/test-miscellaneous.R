
context("Miscellaneous")

test_that('.onLoad works and sets options', {
  old_opts <- options()
  hux_opts <- list(
          # huxtable.add_colnames  = NULL, # want not to set this to anything on package load
          huxtable.print         = NULL,
          huxtable.color_screen  = NULL,
          huxtable.knit_print_df = NULL,
          huxtable.knit_print_df_theme = NULL,
          huxtable.autoformat = NULL,
          huxtable.autoformat_number_format = NULL,
          huxtable.autoformat_align = NULL
        )
  options(hux_opts)
  expect_silent(huxtable:::.onLoad(system.file(package='huxtable'), 'huxtable'))
  for (opt in names(hux_opts)) {
    expect_false( is.null(options(opt)[[1]]), info = paste("Option:", opt))
  }
  options(old_opts)
})
