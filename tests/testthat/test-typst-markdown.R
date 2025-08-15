local_edition(3)

test_that("typst markdown translator handles formatting", {
  expect_match(unname(huxtable:::render_markdown("**bold**", "typst")), "*bold*", fixed = TRUE)
  expect_match(unname(huxtable:::render_markdown("*italic*", "typst")), "_italic_", fixed = TRUE)
  expect_match(unname(huxtable:::render_markdown("~~strike~~", "typst")), "#strike[strike]", fixed = TRUE)
  expect_match(unname(huxtable:::render_markdown("`code`", "typst")), "`code`", fixed = TRUE)
})

test_that("typst markdown translator handles links and images", {
  expect_match(
    unname(huxtable:::render_markdown("[link](https://example.com)", "typst")),
    '#link("https://example.com")[link]',
    fixed = TRUE
  )
  expect_match(
    unname(huxtable:::render_markdown("![alt](https://example.com/img.png)", "typst")),
    '#image("https://example.com/img.png", alt: "alt")',
    fixed = TRUE
  )
})

test_that("typst markdown translator handles headings", {
  expect_match(
    unname(huxtable:::render_markdown("# Heading", "typst")),
    "= Heading",
    fixed = TRUE
  )
  expect_match(
    unname(huxtable:::render_markdown("### Subheading", "typst")),
    "=== Subheading",
    fixed = TRUE
  )
})

test_that("typst markdown translator handles lists", {
  expect_match(
    unname(huxtable:::render_markdown("- one\n- two", "typst")),
    "- one\n- two\n",
    fixed = TRUE
  )
  expect_match(
    unname(huxtable:::render_markdown("1. one\n2. two", "typst")),
    "+ one\n+ two\n",
    fixed = TRUE
  )
})

test_that("typst markdown translator handles code blocks", {
  expect_equal(
    unname(huxtable:::render_markdown("```\ncode\n```", "typst")),
    "```\ncode\n```"
  )
})

test_that("typst markdown translator handles horizontal rules", {
  expect_match(
    huxtable:::render_markdown("---", "typst"),
    "#line(",
    fixed = TRUE
  )
})
