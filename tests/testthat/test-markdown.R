

md_hux <- hux(x = c(
  "Ordinary text",
  "*Italic*, **bold**",
  "***Bold italic***",
  "~Strikethrough~",
  "Inline `code`",
  "Text with a [link](https://cran.r-project.org)",
  "Text with an image ![bones](bones.png)",
  "Thematic break\n\n----\n",
  "# ATX heading\n",
  "Setext heading\n==============",
  "Para\n\n    code block\nPara",
  "Text\n```\nfenced code\n```\ntext",
  "Text\n- Bullet 1\n- Bullet 2\n",
  "Text\n> Blockquote\n> Blockquote\n\nText"
))

md_hux <- cbind(md_hux, md_hux)
markdown(md_hux)[, 2] <- TRUE


test_that("Output produced for basic types", {
  expect_silent(to_screen(md_hux))
  expect_silent(to_html(md_hux))
  expect_silent(to_latex(md_hux))
  expect_silent(to_md(md_hux))
  expect_silent(to_rtf(md_hux))
})


test_that("Output produced for Word", {
  skip_if_not_installed("ftExtra")
  # currently only basic stuff works
  expect_silent(as_flextable(md_hux[1:7, ]))
  skip("Awaiting ftExtra improvements")
  expect_silent(as_flextable(md_hux[8:14, ]))
})


test_that("Output produced for Excel", {
  skip_if_not_installed("openxlsx")
  expect_silent(as_Workbook(md_hux))
})


test_that("Compile to PDF", {
  on.exit(try(file.remove("quick-markdown.pdf"), silent = TRUE))
  md_hux_w <- set_width(md_hux, 0.5)
  expect_silent(quick_pdf(md_hux_w, file = "quick-markdown.pdf", open = FALSE))
})
