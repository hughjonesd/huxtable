# Package options

Huxtable has several options.

## Details

- `options("huxtable.add_colnames")` sets the default value for
  `add_colnames` in [`huxtable()`](huxtable.md) and
  [`as_huxtable()`](as_huxtable.md). As of version 5.0.0, this defaults
  to `TRUE`.

- `options("huxtable.print")` sets the print method for huxtable
  objects. See [`print.huxtable()`](print.huxtable.md).

- `options("huxtable.knitr_output_format")` overrides the default output
  format when huxtable objects are printed by knitr. Set to "html",
  "latex", "md" or "screen". If `NULL` (the default), huxtable guesses
  the format using
  [`guess_knitr_output_format()`](guess_knitr_output_format.md).

- `options("huxtable.autolabel")`. If `TRUE`, (the default)
  automatically sets [`label()`](label.md) from the knitr chunk label,
  if there is one.

- `options("huxtable.color_screen")`. If `TRUE` and package `crayon` is
  available, huxtables will be printed in color on screen.

- `options("huxtable.bookdown")`. Set to `TRUE` within a bookdown
  document to automatically print bookdown-style labels. If unset,
  huxtable will try to guess if we are in a bookdown document.

- `options("huxtable.knit_print_df")`. If `TRUE`, data frames in knitr
  will be pretty-printed using huxtable. This option defaults to `TRUE`
  only if huxtable is attached to the search path using
  [`library()`](https://rdrr.io/r/base/library.html); not if huxtable is
  merely loaded (e.g. imported by another package).

- `options("huxtable.knit_print_df_theme")`. A function applied to data
  frames before printing in knitr. The function should take one argument
  (a data frame) and return a huxtable. Defaults to
  [`theme_plain()`](themes.md).

- `options("huxtable.autoformat")` sets the default value for
  `autoformat` in [`huxtable()`](huxtable.md) and
  [`as_huxtable()`](as_huxtable.md). It defaults to `TRUE`.

- `options("huxtable.latex_use_fontspec")`. If `TRUE`, use the
  "fontspec" package, which allows you to use the same font names in TeX
  and HTML. This requires the the xetex or xelatex engine, which can be
  set using an .rmd header option. Note that
  [`quick_pdf()`](quick-output.md) may use pdflatex. The default is
  `FALSE`.

- `options("huxtable.long_minus")`. If `TRUE`, prints long minus signs
  for numbers. The default is `FALSE`. In LaTeX output, this option is
  overridden by `options("huxtable.latex_siunitx_align")`.

- `options("huxtable.latex_siunitx_align")`. If `TRUE`, uses the
  `\tablenum` macro from the "siunitx" package to align numbers when
  `align(ht)` is `"."` or similar. See [`align()`](align.md) for
  details. The default is `FALSE`.

  `options("huxtable.quarto_process")`. If `TRUE`, enables quarto
  processing of HTML tables. This overrides some huxtable styles, but
  may allow quarto to do other things, e.g. process citations correctly.
  The default is `FALSE`.

- `options("huxtable.autoformat_number_format")` and
  `options("huxtable.autoformat_align")` are lists. The list names are
  base R classes. [`huxtable()`](huxtable.md) with `autoformat = TRUE`
  will set [`number_format()`](number_format.md) and
  [`align()`](align.md) for data columns according to the corresponding
  list values. For example, to center-align `Date` objects you could set
  `"huxtable.autoformat_align"` to something like
  `list(..., Date = "center", ...)`.

## See also

Useful links:

- <https://hughjonesd.github.io/huxtable/>

- Report bugs at <https://github.com/hughjonesd/huxtable/issues>

## Author

**Maintainer**: David Hugh-Jones <davidhughjones@gmail.com>
