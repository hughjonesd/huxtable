
context("Miscellaneous")

test_that(".onLoad works and sets options", {
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
  expect_silent(huxtable:::.onLoad(system.file(package = "huxtable"), "huxtable"))
  for (opt in names(hux_opts)) {
    expect_false( is.null(options(opt)[[1]]), info = paste("Option:", opt))
  }
  options(old_opts)


})


test_dplyr_methods <- function () {
  huxtable:::.onLoad(system.file(package = "huxtable"), "huxtable")
  expected_methods <- c("arrange", "arrange_", "filter", "filter_", "mutate", "mutate_", "rename",
    "rename_", "select", "select_", "slice", "slice_", "transmute", "transmute_")
  dplyr_methods <- getNamespaceInfo("dplyr", "S3methods")  # will load dplyr
  dplyr_methods <- as.data.frame(dplyr_methods[, 1:2])
  names(dplyr_methods) <- c("generic", "class")
  dplyr_methods <- dplyr::filter(dplyr_methods, class == "huxtable")
  expect_setequal(unlist(dplyr_methods$generic), expected_methods)
}


test_knitr_methods <- function () {
  huxtable:::.onLoad(system.file(package = "huxtable"), "huxtable")
  knitr_methods <- getNamespaceInfo("knitr", "S3methods")
  knitr_methods <- as.data.frame(knitr_methods[, 1:2])
  names(knitr_methods) <- c("generic", "class")
  knitr_methods <- dplyr::filter(knitr_methods, generic == "knit_print")
  expect_true(all(c("huxtable", "data.frame") %in% unlist(knitr_methods$class)))
}


# can be run via devtools::test, but not automatically from RStudio
test_that(".onLoad gets methods registered if namespace not loaded", {
  skip_if_not_installed("dplyr")
  skip_if_not_installed("knitr")

  tryCatch(unloadNamespace("dplyr"), error = function (e) skip("Couldn't unload dplyr namespace"))
  test_dplyr_methods()
  tryCatch(unloadNamespace("knitr"), error = function (e) skip("Couldn't unload knitr namespace"))
  test_knitr_methods()
})


test_that(".onLoad gets S3 methods registered if namespace loaded", {
  skip_if_not_installed("dplyr")
  skip_if_not_installed("knitr")

  tryCatch(unloadNamespace("dplyr"), error = function (e) skip("Couldn't unload dplyr namespace"))
  library(dplyr)
  test_dplyr_methods()
  tryCatch(unloadNamespace("knitr"), error = function (e) skip("Couldn't unload knitr namespace"))
  library(knitr)
  test_knitr_methods()
})


test_that("install/report_latex_dependencies run", {
  skip_if_not_installed("tinytex")

  expect_silent(packages <- report_latex_dependencies(quiet = TRUE))
  packages <- vapply(packages, `[[`, character(1), "name")
  packages <- setdiff(packages, c("graphicx", "calc", "array"))
  with_mock(
    `tinytex::tlmgr_install` = function (...) return(0),
    expect_error(x <- install_latex_dependencies(), regexp = NA)
  )
  expect_true(x)
})


test_that("check_latex_dependencies runs correctly", {
  skip_if_not_installed("tinytex")

  with_mock(
    `tinytex::tl_pkgs` = function (...) return(character(0)), {
    expect_false(check_latex_dependencies(quiet = TRUE))
    expect_message(check_latex_dependencies(quiet = FALSE), regexp = "not found")
  })

  ld <- tlmgr_packages()
  with_mock(
    `tinytex::tl_pkgs` = function (...) return(ld), {
      expect_true(check_latex_dependencies(quiet = TRUE))
      expect_message(check_latex_dependencies(quiet = FALSE), regexp = "All LaTeX packages found")
    })
})


test_that("is_a_number", {
  expect_false(is_a_number("foo"))
  expect_true(is_a_number(1.5))
  expect_true(is_a_number("1.5"))
  ht <- hux(a = 1:2, add_colnames = TRUE)
  expect_equivalent(is_a_number(ht), matrix(c(FALSE, TRUE, TRUE), 3, 1))
})

