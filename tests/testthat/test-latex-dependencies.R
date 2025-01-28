
local_edition(2)

test_that("install/report_latex_dependencies", {
  skip_if_not_installed("tinytex")
  skip_on_appveyor()
  skip_on_cran() # no tlmgr, and the code uses it not just in tlmgr_install

  expect_silent(packages <- report_latex_dependencies(quiet = TRUE))
  packages <- vapply(packages, `[[`, character(1), "name")
  packages <- setdiff(packages, c("graphicx", "calc", "array"))
  with_mocked_bindings(
    expect_error(x <- install_latex_dependencies(), regexp = NA),
    tlmgr_install_wrapper = function (...) return(0)
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
  # nor does win-builder
  skip_on_cran()
  skip_on_ci()

  local_mocked_bindings(
    .package = "tinytex",
    tlmgr = function (...) "1.0",
  )
  expect_warning(check_adjustbox(quiet = FALSE), "adjustbox")
  expect_equivalent(check_adjustbox(), FALSE)
})


test_that("check_latex_dependencies runs correctly", {
  skip_if_not_installed("tinytex")
  # appveyor doesn't have tlmgr
  skip_on_appveyor()
  # nor does win-builder
  skip_on_cran()

  ld <- tlmgr_packages()

  local_mocked_bindings(
    .package = "tinytex",
    tl_pkgs = function (...) return(character(0))
  )
  expect_false(check_latex_dependencies(quiet = TRUE))
  expect_message(check_latex_dependencies(quiet = FALSE), regexp = "not found")

  local_mocked_bindings(
    .package = "tinytex",
    tl_pkgs = function (...) return(ld)
  )
  expect_true(check_latex_dependencies(quiet = TRUE))
  expect_message(check_latex_dependencies(quiet = FALSE), regexp = "All LaTeX packages found")
})
