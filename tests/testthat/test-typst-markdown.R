local_edition(2)

test_that("typst markdown translator handles formatting", {
  expect_equal(unname(huxtable:::render_markdown("**bold**", "typst")), "**bold**")
  expect_equal(unname(huxtable:::render_markdown("*italic*", "typst")), "*italic*")
  expect_equal(unname(huxtable:::render_markdown("~~strike~~", "typst")), "#strike[strike]")
  expect_equal(unname(huxtable:::render_markdown("`code`", "typst")), "`code`")
})

test_that("typst markdown translator handles links and images", {
  expect_equal(
    unname(huxtable:::render_markdown("[link](https://example.com)", "typst")),
    '#link("https://example.com")[link]'
  )
  expect_equal(
    unname(huxtable:::render_markdown("![alt](https://example.com/img.png)", "typst")),
    '#image("https://example.com/img.png", alt: "alt")'
  )
})

test_that("typst markdown translator handles headings", {
  expect_equal(
    unname(huxtable:::render_markdown("# Heading", "typst")),
    "= Heading"
  )
  expect_equal(
    unname(huxtable:::render_markdown("### Subheading", "typst")),
    "=== Subheading"
  )
})

test_that("typst markdown translator handles lists", {
  expect_equal(
    unname(huxtable:::render_markdown("- one\n- two", "typst")),
    "- one\n- two\n"
  )
  expect_equal(
    unname(huxtable:::render_markdown("1. one\n2. two", "typst")),
    "+ one\n+ two\n"
  )
})

test_that("typst markdown translator handles block quotes", {
  expect_equal(
    unname(huxtable:::render_markdown("> quote", "typst")),
    "> quote\n"
  )
})

test_that("typst markdown translator handles code blocks", {
  expect_equal(
    unname(huxtable:::render_markdown("```\ncode\n```", "typst")),
    "```\ncode\n```"
  )
})

test_that("typst markdown translator handles horizontal rules", {
  expect_equal(
    unname(huxtable:::render_markdown("---", "typst")),
    "---\n"
  )
})
