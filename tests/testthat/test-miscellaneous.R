
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
  expect_silent(huxtable:::.onLoad(system.file(package = 'huxtable'), 'huxtable'))
  for (opt in names(hux_opts)) {
    expect_false( is.null(options(opt)[[1]]), info = paste("Option:", opt))
  }
  options(old_opts)


})


# can be run via devtools::test, but not automatically from RStudio
test_that('.onLoad gets methods registered', {
  tryCatch(unloadNamespace('dplyr'), error = function (e) skip("Couldn't unload dplyr namespace"))

  huxtable:::.onLoad(system.file(package = 'huxtable'), 'huxtable')
  expected_methods <- c('arrange', 'arrange_', 'filter', 'filter_', 'mutate', 'mutate_', 'rename',
    'rename_', 'select', 'select_', 'slice', 'slice_', 'transmute', 'transmute_')
  dplyr_methods <- getNamespaceInfo('dplyr', 'S3methods')  # will load dplyr
  dplyr_methods <- as.data.frame(dplyr_methods[, 1:2])
  names(dplyr_methods) <- c('generic', 'class')
  dplyr_methods <- dplyr::filter(dplyr_methods, class == 'huxtable')
  expect_setequal(unlist(dplyr_methods$generic), expected_methods)

  knitr_methods <- getNamespaceInfo('knitr', 'S3methods')
  knitr_methods <- as.data.frame(knitr_methods[, 1:2])
  names(knitr_methods) <- c('generic', 'class')
  knitr_methods <- dplyr::filter(knitr_methods, generic == 'knit_print')
  expect_true(all(c('huxtable', 'data.frame') %in% unlist(knitr_methods$class)))
})
