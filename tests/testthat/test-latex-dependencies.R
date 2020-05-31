

test_that("install/report_latex_dependencies", {
  skip_if_not_installed("tinytex")

  expect_silent(packages <- report_latex_dependencies(quiet = TRUE))
  packages <- vapply(packages, `[[`, character(1), "name")
  packages <- setdiff(packages, c("graphicx", "calc", "array"))
  with_mock(
    `tinytex::tlmgr_install` = function (...) return(0),
    expect_error(x <- install_latex_dependencies(), regexp = NA)
  )
  expect_true(x)

  expect_silent(package_str <- report_latex_dependencies(quiet = TRUE,
    as_string = TRUE))
  expect_match(package_str, "\\\\usepackage\\{array\\}")
})


test_that("check_latex_dependencies checks adjustbox", {
  skip_if_not_installed("tinytex")
  # appveyor doesn't have tlmgr
  skip_on_appveyor()

  with_mock(
    "tinytex::tlmgr" = function (...) "1.0",
    {
      expect_warning(check_adjustbox(quiet = FALSE), "adjustbox")
      expect_equivalent(check_adjustbox(), FALSE)
    }
  )
})


test_that("check_latex_dependencies runs correctly", {
  skip_if_not_installed("tinytex")
  # appveyor doesn't have tlmgr
  skip_on_appveyor()

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
