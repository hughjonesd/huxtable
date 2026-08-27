skip_if_not_installed("knitr")


test_that("Bugfix: multiple tables in a chunk get unique labels", {
  old_current <- knitr::opts_current$get()
  on.exit(knitr::opts_current$restore(old_current))
  knitr::opts_current$set(label = "run")
  knitr::opts_current$delete("huxtable.used_labels")

  first <- to_latex(hux(a = 1))
  second <- to_latex(hux(a = 2))

  expect_match(first, "\\label{tab:run}", fixed = TRUE)
  expect_match(second, "\\label{tab:run-2}", fixed = TRUE)
})


test_that("Explicit labels are respected by automatic labels", {
  old_current <- knitr::opts_current$get()
  on.exit(knitr::opts_current$restore(old_current))
  knitr::opts_current$set(label = "run")
  knitr::opts_current$delete("huxtable.used_labels")

  expect_equal(make_label(set_label(hux(a = 1), "tab:run")), "tab:run")
  expect_equal(make_label(hux(a = 2)), "tab:run-2")
})


test_that("Automatic label state resets with knitr chunk state", {
  old_current <- knitr::opts_current$get()
  on.exit(knitr::opts_current$restore(old_current))
  knitr::opts_current$set(label = "run")
  knitr::opts_current$delete("huxtable.used_labels")

  expect_equal(make_label(hux(a = 1)), "tab:run")
  expect_equal(make_label(hux(a = 2)), "tab:run-2")

  knitr::opts_current$restore(old_current)
  knitr::opts_current$set(label = "next-run")
  expect_equal(make_label(hux(a = 3)), "tab:next-run")
})


test_that("Unnamed chunks and disabled autolabeling do not create labels", {
  old_current <- knitr::opts_current$get()
  old_options <- options(huxtable.autolabel = TRUE)
  on.exit({
    knitr::opts_current$restore(old_current)
    options(old_options)
  })

  knitr::opts_current$set(label = "unnamed-chunk-1")
  expect_true(is.na(make_label(hux(a = 1))))

  knitr::opts_current$set(label = "run")
  options(huxtable.autolabel = FALSE)
  expect_true(is.na(make_label(hux(a = 2))))
})


test_that("Bookdown captions include labels in the expected syntax", {
  old_options <- options(huxtable.bookdown = TRUE)
  on.exit(options(old_options))
  ht <- set_caption(hux(a = 1), "A caption")

  html_caption <- make_caption(ht, "tab:run", "html")
  latex_caption <- make_caption(ht, "tab:run", "latex")

  expect_equal(html_caption, "(#tab:run) A caption")
  expect_equal(as.character(latex_caption), "(\\#tab:run) A caption")
  expect_null(attr(html_caption, "has_label"))
  expect_true(isTRUE(attr(latex_caption, "has_label")))

  options(huxtable.bookdown = FALSE)
  expect_equal(make_caption(ht, "tab:run", "html"), "A caption")
})
