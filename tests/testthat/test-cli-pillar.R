test_that("cli_pillar_screen works", {
  skip_if_not_installed("pillar")
  skip_if_not_installed("cli")
  ht <- hux(a = 1:2, b = 3:4)
  expect_output(cli_pillar_screen(ht), "a")
})

test_that("cli_pillar_screen handles spans", {
  skip_if_not_installed("pillar")
  skip_if_not_installed("cli")
  ht <- hux(a = 1:2, b = 3:4)
  ht <- merge_cells(ht, 1:2, 1:2)
  out <- capture.output(cli_pillar_screen(ht))
  expect_true(any(grepl("1", out)))
  expect_false(any(grepl("3", out)))
})
