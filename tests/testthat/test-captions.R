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

  expect_equal(resolve_caption(set_label(hux(a = 1), "tab:run"), "html")$label, "tab:run")
  expect_equal(resolve_caption(hux(a = 2), "html")$label, "tab:run-2")
})


test_that("Automatic label state resets with knitr chunk state", {
  old_current <- knitr::opts_current$get()
  on.exit(knitr::opts_current$restore(old_current))
  knitr::opts_current$set(label = "run")
  knitr::opts_current$delete("huxtable.used_labels")

  expect_equal(resolve_caption(hux(a = 1), "html")$label, "tab:run")
  expect_equal(resolve_caption(hux(a = 2), "html")$label, "tab:run-2")

  knitr::opts_current$restore(old_current)
  knitr::opts_current$set(label = "next-run")
  expect_equal(resolve_caption(hux(a = 3), "html")$label, "tab:next-run")
})


test_that("Unnamed chunks and disabled autolabeling do not create labels", {
  old_current <- knitr::opts_current$get()
  old_options <- options(huxtable.autolabel = TRUE)
  on.exit({
    knitr::opts_current$restore(old_current)
    options(old_options)
  })

  knitr::opts_current$set(label = "unnamed-chunk-1")
  expect_true(is.na(resolve_caption(hux(a = 1), "html")$label))

  knitr::opts_current$set(label = "run")
  options(huxtable.autolabel = FALSE)
  expect_true(is.na(resolve_caption(hux(a = 2), "html")$label))
})


test_that("Bookdown captions include labels in the expected syntax", {
  old_options <- options(huxtable.bookdown = TRUE)
  on.exit(options(old_options))
  ht <- set_label(set_caption(hux(a = 1), "A caption"), "tab:run")

  html_caption <- resolve_caption(ht, "html")
  latex_caption <- resolve_caption(ht, "latex")

  expect_equal(html_caption$text, "(#tab:run) A caption")
  expect_equal(latex_caption$text, "(\\#tab:run) A caption")
  expect_equal(html_caption$label, "tab:run")
  expect_equal(latex_caption$label, "tab:run")
  expect_true(html_caption$label_in_caption)
  expect_true(latex_caption$label_in_caption)

  label_only <- resolve_caption(set_label(hux(a = 1), "run"), "html")
  expect_equal(label_only$text, "(#tab:run) ")
  expect_equal(label_only$label, "run")
  expect_true(label_only$label_in_caption)

  options(huxtable.bookdown = FALSE)
  plain_caption <- resolve_caption(ht, "html")
  expect_equal(plain_caption$text, "A caption")
  expect_false(plain_caption$label_in_caption)
})


test_that("Caption position helpers share fallback behavior", {
  ht <- set_position(hux(a = 1), "left")

  caption_pos(ht) <- "top"
  expect_equal(get_caption_hpos(ht), "left")
  expect_equal(get_caption_vpos(ht), "top")

  caption_pos(ht) <- "bottomright"
  expect_equal(get_caption_hpos(ht), "right")
  expect_equal(get_caption_vpos(ht), "bottom")
})


test_that("Renderers preserve their native label behavior", {
  old_options <- options(huxtable.bookdown = TRUE)
  on.exit(options(old_options))
  ht <- set_label(set_caption(hux(a = 1), "A caption"), "tab:run")

  latex <- to_latex(ht)
  html <- to_html(ht)
  markdown <- to_md(ht)
  typst <- to_typst(ht)

  expect_match(latex, "(\\#tab:run) A caption", fixed = TRUE)
  expect_false(grepl("\\label{tab:run}", latex, fixed = TRUE))
  expect_match(html, 'id="tab:run"', fixed = TRUE)
  expect_match(html, "(#tab:run) A caption", fixed = TRUE)
  expect_match(markdown, "Table: (#tab:run) A caption", fixed = TRUE)
  expect_match(typst, "(\\#tab:run) A caption", fixed = TRUE)
  expect_match(typst, "<tab:run>", fixed = TRUE)
})
