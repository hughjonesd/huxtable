# Changelog

## huxtable (development version)

- Bugfix: [`add_rownames()`](../reference/add_colnames.md) should no
  longer add a 1 in the header row of a data frame.

## huxtable 5.8.0

CRAN release: 2025-11-07

### Other changes

- Redesigned [`hux_logo()`](../reference/hux_logo.md) function to spell
  out “huxtable” with customizable layout, fonts, and colors. Added
  `compact` and `latex` parameters for different output formats.
- Workaround for a bug in stringr 1.6.0.

## huxtable 5.7.0

CRAN release: 2025-08-18

### Breaking changes

- Functions are no longer generic, so you can’t subclass a huxtable
  object. AFAIK nobody has ever done this; if I’m wrong, please tell me.
- Reworked internals, with the help of OpenAI Codex.

### Other changes

- HTML tables now wrap header rows in `<thead>` (using `<th>` cells) and
  body rows in `<tbody>` when header rows are at the top of the table.
- Added Typst export via [`to_typst()`](../reference/to_typst.md) and
  [`print_typst()`](../reference/to_typst.md). Quarto integration is
  available as well as [`quick_typst()`](../reference/quick-output.md),
  [`quick_typst_pdf()`](../reference/quick-output.md),
  [`quick_typst_png()`](../reference/quick-output.md), and
  [`quick_typst_svg()`](../reference/quick-output.md) functions.
- HTML output now uses CSS classes with a shared `<style>` block instead
  of long inline styles.
- Added [`as_html()`](../reference/to_html.md) for obtaining table as
  `htmltools` tags.
- [`to_screen()`](../reference/to_screen.md) now displays double, dashed
  and dotted border styles.

## huxtable 5.6.0

CRAN release: 2025-03-05

### Breaking changes

- Removed underscore dplyr verbs (`slice_`, `select_` etc.) These have
  long been deprecated in dplyr itself.

### Other changes

- Bugfix: add newline at end of report_latex_dependencies() output.
  Thanks [@ceresek](https://github.com/ceresek).
- You can now add multiple huxtables to the same Excel worksheet in
  [`as_Workbook()`](../reference/as_Workbook.md). Suggestion by
  [@oobd](https://github.com/oobd).

## huxtable 5.5.7

CRAN release: 2024-10-01

- Bugfix: fix quarto referencing in quarto 1.5
- Bugfix: integer overflow on very large huxtables. Thanks
  [@kpagacz](https://github.com/kpagacz).

## huxtable 5.5.6

CRAN release: 2024-02-15

- Bugfix: quarto cross-referencing was giving too many warnings.

## huxtable 5.5.5

CRAN release: 2024-02-08

- Bugfix: quarto cross-referencing doesn’t work for PDF with quarto
  version 1.4. See `?huxtable-FAQ` for workarounds.
- Bugfix: [`by_cases()`](../reference/by_cases.md) wasn’t picking up
  variables from the caller environment.
- huxtable 5.5.4 was never released due to failing a reverse dependency
  check.

## huxtable 5.5.3

CRAN release: 2023-12-09

- Bugfix: disable quarto styling on HTML tables. You can reenable quarto
  processing with `options(huxtable.quarto_process = TRUE)`.
- Bugfix: borders weren’t working with merged cells in Word documents.

## huxtable 5.5.2

CRAN release: 2022-12-16

- Update [`by_cases()`](../reference/by_cases.md) to work with dplyr
  1.1.0. Within [`by_cases()`](../reference/by_cases.md) formulas, `.`
  is now vector rather than matrix when dplyr version 1.1.0 is detected.
  Thanks [@DavisVaughan](https://github.com/DavisVaughan).
- Add package checks in `quick_*` functions. Thanks
  [@reuning](https://github.com/reuning).

## huxtable 5.5.1

CRAN release: 2022-11-12

- CSS borders are now set explicitly even if they are all set to 0.
- Bugfix: shell-quote files in `quick_*` functions. Thanks to
  [@ceresek](https://github.com/ceresek).
- Bugfix: cope with adjustbox version “1.3a” among latex dependencies.

## huxtable 5.5.0

CRAN release: 2022-06-15

- Huxtable should work with [Quarto](https://quarto.org) documents.
  - Quarto labels and captions will override huxtable-provided ones.
  - Quarto style references like `@table-label` only work with quarto
    labels.
  - Please report any bugs!
- New [`column_to_header()`](../reference/column_to_header.md) function
  converts a column to header rows. New
  [`as_hux()`](../reference/as_huxtable.md) method for `grouped_df`
  objects optionally converts groups to header rows.
- New convenience functions [`stripe_rows()`](../reference/stripes.md)
  and [`stripe_columns()`](../reference/stripes.md).
- Add `format` and `...` options to
  [`fmt_percent()`](../reference/fmt_percent.md) to allow flexible
  formatting via [`formatC()`](https://rdrr.io/r/base/formatc.html).
- [`add_footnote()`](../reference/add_footnote.md) gets an explicit
  `number_format` argument which is `NA` by default.
- Bugfix: infinite loop with wide characters in
  [`to_screen()`](../reference/to_screen.md).
- Bugfix: duplicate colnames when exporting
  `huxreg(..., error_pos = "right")` to flextable.
- Bugfix: bookdown-style references weren’t working in blogdown.

## huxtable 5.4.0

CRAN release: 2021-05-14

- New behaviour: setting [`colspan()`](../reference/spans.md) or
  [`rowspan()`](../reference/spans.md) overwrites the content of cells
  that have been shadowed.

  ``` r

  ht <- hux(c(1, 1), c(2, 2), c(3, 3))
  ht <- set_all_borders(ht)
  colspan(ht)[1, 1] <- 3

  # old behaviour                
  ht[, c(2, 1, 3)]
  ##   +--------------------------+
  ##   |                  2       |
  ##   +--------+--------+--------+
  ##   |      2 |      1 |      3 |
  ##   +--------+--------+--------+

  # new behaviour
  ht[, c(2, 1, 3)]
  ##   +--------------------------+
  ##   |                  1       |
  ##   +--------+--------+--------+
  ##   |      2 |      1 |      3 |
  ##   +--------+--------+--------+
  ```

- New option `huxtable.latex_siunitx_align` allows you to use the LaTeX
  `siunitx` package to handle decimal point alignment. This is `FALSE`
  by default.

- Bugfix: centre alignment was not working in
  [`print_screen()`](../reference/to_screen.md).

- Bugfix: failure in [`to_md()`](../reference/to_md.md) with recent
  versions of `stringi` package.

- Bugfix: repeating a single row in a subset, like
  `ht[c(1, 1, 2, 3), ]`, was setting `colspan = 2` on the repeated row.

- Bugfix: zero-argument subset replacement like `ht[] <- ...` wasn’t
  working.

## huxtable 5.3.0

CRAN release: 2021-05-01

- Improve decimal alignment in LaTeX when `align(ht) == "."`. This may
  change the appearance of some documents.
- Allow [`tidy_override()`](../reference/tidy_override.md) to extend
  columns of `tidy` and `glance`.
- Bugfix: [\#196](https://github.com/hughjonesd/huxtable/issues/196) `^`
  was giving errors in LaTeX.

## huxtable 5.2.0

CRAN release: 2021-02-14

- Add `table_environment` property so you can use e.g. `"table*"` in
  TeX.
- Bugfix: `print_screen(h, colnames = FALSE)` didn’t print a final
  newline.
- Bugfix: italic from markdown was being printed as underlined in TeX.
- Minor test update for compatibility with broom.

## huxtable 5.1.1

CRAN release: 2020-10-27

- Minor test update for compatibility with broom.
- Fixes for R 4.1.0.

## huxtable 5.1.0

CRAN release: 2020-09-18

- [`as_flextable()`](../reference/as_flextable.md) now exports markdown
  in cells to RTF, and to Word with the help of the optional `ftExtra`
  package. Thanks [@atusy](https://github.com/atusy) for adding this
  feature.

- Improvements to markdown screen export. This now uses the optional
  `fansi` package.

- New feature: [`as_Workbook()`](../reference/as_Workbook.md) gains
  `start_row` and `start_col` arguments, to write a huxtable into an
  Excel worksheet starting at a particular row or column.

- New feature: [`huxreg()`](../reference/huxreg.md) gains a
  `glance_args` argument to pass arguments to
  [`glance()`](https://generics.r-lib.org/reference/glance.html).

- New feature: `options(huxtable.long_minus = TRUE)` will try to use
  long minus signs before numbers. The default is `FALSE`. It will
  probably become `TRUE` in a future version.

- Bugfix: `insert_row/column(..., after = 0)` was unsetting table
  properties.

- Bugfix: unicode characters above 32767 were incorrectly represented in
  RTF. Thanks [@kaigu1990](https://github.com/kaigu1990).

- Bugfix: columns were being collapsed in
  [`as_Workbook()`](../reference/as_Workbook.md).

- Bugfix: `style_cells` didn’t work unless huxtable was on the search
  path.

- Bugfix: `merge_repeated_rows` merged `NA` rows incorrectly.

- Bugfix: number format was not set correctly in
  [`huxreg()`](../reference/huxreg.md)’s `note`.

- Bugfix: in [`huxreg()`](../reference/huxreg.md), `tidy_args` threw an
  error if the first argument to
  [`tidy()`](https://generics.r-lib.org/reference/tidy.html) was a named
  list.

- Bugfix: [`tidy_replace()`](../reference/tidy_override.md) was broken.

- Clearer error messages for
  [`tidy_override()`](../reference/tidy_override.md) when
  `extend = FALSE`. In future, `extend` will probably default to `TRUE`.

### Other news:

- Huxtable received its first Patreon sponsor! Thanks to Ross Mattheis.

## huxtable 5.0.0

CRAN release: 2020-06-15

Huxtable 5.0.0 brings numerous changes. For a more user-friendly
introduction, see
<https://hughjonesd.github.io/whats-new-in-huxtable-5.0.0.html>.

### Breaking changes

- There are changes to LaTeX output.

  - LaTeX `\tabcolsep` is now set to 0 within huxtable tables, while
    left and right padding should now take effect even when `wrap` is
    `FALSE`.
  - The default LaTeX table environment is now “tabular” unless `width`
    is set. If `width` is set, it is “tabularx”.
  - `wrap` only matters if `width` is set. Otherwise, cell wrapping is
    off.
  - the `\centerbox` macro from the LaTeX “adjustbox” package is used to
    centre tables. This should improve centring when tables are too
    wide. You may need to update the LaTeX “adjustbox” package to a
    recent version.
    [`check_latex_dependencies()`](../reference/report_latex_dependencies.md)
    can inform you about this.

- As previously signalled, `add_colnames` has now become `TRUE` by
  default in [`huxtable()`](../reference/huxtable.md) and
  [`as_huxtable()`](../reference/as_huxtable.md). Set
  `options(huxtable.add_colnames = FALSE)` to go back to the old
  behaviour.

- Newlines in cell contents are now respected (in LaTeX, so long as
  `wrap = TRUE` and `width` has been set).

- Huxtable borders have been reworked, fixing some longstanding bugs and
  adding new features.

  - Borders are now automatically collapsed. For example:

    ``` r
    jams %>% 
        set_right_border(everywhere, 1, 1) %>% 
        set_left_border(everywhere, 2, 0.4)
    ```

    will set the border in between the columns of `jams` to `0.4`,
    overwriting the previous value. This is more in line with what you
    would expect. For example, the following code now does what you
    probably want:

        jams %>%
            set_rowspan(2, 1, 3) %>%
            set_bottom_border(4, everywhere, 1)
        ##                 Type              Price
        ##                 Strawberry         1.90
        ##                                    2.10
        ##                                    1.80
        ##               ---------------------------

    instead of the old behaviour:

        jams %>%
            set_rowspan(2, 1, 3) %>%
            set_bottom_border(4, everywhere, 1)
        ##                 Type           Price
        ##                 Strawberry      1.90
        ##                                 2.10
        ##                                 1.80
        ##                            -----------

  - [`set_left_border()`](../reference/borders.md),
    [`set_all_borders()`](../reference/set-multiple.md) and friends all
    use a default value of 0.4. So to set a default border, write e.g.

    ``` r
    as_hux(head(iris)) %>% 
          set_bottom_border(1, everywhere)
    ```

  - A new [`brdr()`](../reference/brdr.md) class encapsulates border
    thickness, style and colour. You can set all properties at once by
    writing, e.g.:

    ``` r
    as_hux(jams) %>% 
          set_bottom_border(1, everywhere, brdr(1, "dotted", "darkgreen"))
    ```

    `left_border(ht)` and friends return a `brdr` object. To access the
    border thickness, write `brdr_thickness(left_border(ht))`.

- Various deprecated items have been removed:

  - The 3-argument form of `set_*`. Instead, use `map_*`.
  - The `byrow` argument to `set_*`. Instead, use `map_*` and
    [`by_cols()`](../reference/by_rows.md).
  - `error_style` and `pad_decimal` arguments in `huxreg`. Use
    `error_format` and `align(hx) <- "."`.
  - The [`where()`](https://tidyselect.r-lib.org/reference/where.html),
    `is_a_number()` and `pad_decimal()` functions. Use `map_*`
    functions, `! is.na(as.numeric(x))`, and `align(ht) <- "."`.

- Default padding has been increased to 6 points.

- By default, [`width()`](../reference/width.md) is now unset.

- By default, [`wrap()`](../reference/wrap.md) is now `TRUE`.

- [`every()`](../reference/stripe.md) has been renamed to
  [`stripe()`](../reference/stripe.md), to avoid a clash with
  [`purrr::every()`](https://purrr.tidyverse.org/reference/every.html).
  `everywhere`, `evens` and `odds` are still the same.

- The little-used ability to set `copy_cell_props` to a character vector
  in `rbind.huxtable` and `cbind.huxtable` has been removed. You can
  still set it to `FALSE`.

- [`add_rows()`](../reference/add_rows.md) and
  [`add_columns()`](../reference/add_rows.md) now always call
  [`rbind.huxtable()`](../reference/cbind.huxtable.md) or
  [`cbind.huxtable()`](../reference/cbind.huxtable.md) and return a
  huxtable.

- Huxtable no longer supports dplyr versions less than 0.7.0 (released
  mid-2017).

- [`set_cell_properties()`](../reference/style-functions.md) has been
  renamed [`style_cells()`](../reference/style-functions.md). It is
  retained as a soft-deprecated alias.

- Various themes have been tweaked:

  - [`theme_basic()`](../reference/themes.md) now has bold headers and
    no header column by default.
  - [`theme_plain()`](../reference/themes.md) defaults to
    `position = "centre"`.
  - [`theme_striped()`](../reference/themes.md) uses grey stripes, a
    white border, and subtler headers.
  - [`theme_article()`](../reference/themes.md) has thinner borders.

### Other changes

- You can now use [markdown](https://commonmark.org/help/) within table
  cells.

  - Use `set_markdown(ht, rows, cols)` to turn this on.
  - Or use the convenience function
    [`set_markdown_contents()`](../reference/set_markdown_contents.md)
    to set cell contents that will be interpreted as markdown.
  - Markdown works for HTML and LaTeX. There’s basic support for
    on-screen display.

- Huxtable now has the concept of header row and columns.

  - By default, data frame column names will be headers.
  - To set other rows to be headers, use
    `set_header_rows(ht, row_numbers, TRUE)`. For columns, use
    [`header_cols()`](../reference/header_cols.md) or
    [`set_header_cols()`](../reference/header_cols.md).
  - New functions [`style_headers()`](../reference/style-functions.md),
    [`style_header_cols()`](../reference/style-functions.md), and
    [`style_header_rows()`](../reference/style-functions.md) to set
    multiple properties on headers.
  - In themes, `header_row/col = TRUE` set the first row/col to a
    header, and style all header rows/cols.

- [`set_bold()`](../reference/bold.md) and
  [`set_italic()`](../reference/bold.md) now use a default value of
  `TRUE`. So you can write e.g.

  ``` r
  as_hux(head(iris)) %>% 
        set_bold(1, everywhere)
  ```

- Console output in R now shows table position and caption position.

- By default, huxtable now sets labels from the current knitr chunk
  label, if there is one. This is consistent with `kable()`. In
  bookdown, you can then do e.g.

      Some iris species are shown in \@ref(tab:mytable):

      ```r
      as_hux(iris)
      ```

  Set `options(huxtable.autolabel = FALSE)` to turn off this behaviour.

- The one-argument form of `[` now works for huxtables just as it does
  for data frames. For example, `ht[2:3]` selects columns 2 and 3.

- New functions [`fmt_percent()`](../reference/fmt_percent.md) and
  [`fmt_pretty()`](../reference/fmt_pretty.md) for passing into
  [`number_format()`](../reference/number_format.md):

  ``` r
  jams$Sugar <-c ("Sugar content", 0.4, 0.35, 0.45)
  set_number_format(jams, -1, "Sugar", fmt_percent(1))
  ```

- [`split_across()`](../reference/split-across-down.md) and
  [`split_down()`](../reference/split-across-down.md) split a huxtable
  into a list of sub-tables. Headers can be automatically included.

- [`restack_across()`](../reference/restack-across-down.md) and
  [`restack_down()`](../reference/restack-across-down.md) split a
  huxtable, then join it back up. This is useful for making a table fit
  on a page.

- [`merge_across()`](../reference/merge_across.md) and
  [`merge_down()`](../reference/merge_across.md) merge an area of cells
  horizontally across rows, or vertically down columns.

- New functions
  `set_lr_borders()/_border_colors()/_border_styles()/_padding()`  
  set left and right borders and padding simultaneously. New functions
  [`set_tb_borders()`](../reference/set-multiple.md) etc. set top and
  bottom properties simultaneously. There are `map_` equivalents of all
  of these.

- [`set_outer_padding()`](../reference/set-outer.md) sets padding around
  a range of cells, similarly to
  [`set_outer_borders()`](../reference/set-outer.md).

- A new table-level property,
  [`caption_width()`](../reference/caption_width.md), allows you to set
  the width of the caption. The default, `NA`, sets the width equal to
  the table width.

- There are two new themes: [`theme_compact()`](../reference/themes.md)
  and [`theme_bright()`](../reference/themes.md).

- For [`huxreg()`](../reference/huxreg.md), a new function
  [`tidy_replace()`](../reference/tidy_override.md) allows you to
  replace the output of `tidy(x)` entirely.

- huxtable now only sets `options(huxtable.knit_print_df = TRUE)` if it
  is attached, not if it is loaded.

- huxtable supports
  [`dplyr::relocate()`](https://dplyr.tidyverse.org/reference/relocate.html),
  new in dplyr 1.0.0.

- Improvements to [`as_flextable()`](../reference/as_flextable.md).

- Improvements to [`quick_pptx()`](../reference/quick-output.md) (thanks
  [@davidgohel](https://github.com/davidgohel)).

- Bugfixes for `options(huxtable.use_fontspec = TRUE)`.

- Bugfix: `add_rownames = "string"` now works as promised.

- Bugfix: non-ASCII characters are now supported in RTF.

### Other news

- New versions of the
  [gtsummary](https://cran.r-project.org/package=gtsummary) package will
  have an [`as_huxtable()`](../reference/as_huxtable.md) method.

- Package [texreg](https://cran.r-project.org/package=texreg) on CRAN
  includes a `huxtablereg()` function for creating a table of regression
  outputs.

## huxtable 4.7.1

CRAN release: 2020-01-08

- The [expss](https://github.com/gdemin/expss) package now supports
  export to huxtables.
- [`by_quantiles()`](../reference/by_quantiles.md),
  [`by_equal_groups()`](../reference/by_quantiles.md) and
  [`by_colorspace()`](../reference/by_colorspace.md) have gained a
  `colwise` argument, which calculates quantiles or colors separately
  for each column.
- Add caption support for
  [`as_flextable()`](../reference/as_flextable.md) (thanks
  [@sjewo](https://github.com/sjewo)).

## huxtable 4.7.0

CRAN release: 2019-10-03

- Better error messages.
- New [`merge_repeated_rows()`](../reference/merge_repeated_rows.md)
  function: merge repeated rows into a single cell.
- New `fill` and `colspan/rowspan` arguments for
  `insert_row()/insert_column()`:
  - `insert_row(ht, "blah", "", "", "", "", ...)` can be written
    `insert_row(ht, "blah", fill = "")`.
  - `colspan/rowspan` set `colspan/rowspan` of the first cell in the
    inserted row/column.

## huxtable 4.6.1

CRAN release: 2019-08-06

- Bugfix: right borders in wrong place when cells were merged.
- Bugfix: chinese characters were displaying wrongly in
  [`to_screen()`](../reference/to_screen.md).

## huxtable 4.6.0

CRAN release: 2019-06-24

- Set `options('huxtable.latex_use_fontspec')` to `TRUE` to use portable
  font names in TeX documents, with the LaTeX “fontspec” package.
- Bugfix: attributes were being copied wrongly in subset assignment of
  huxtables.
- Bugfix: text colors in [`hux_logo()`](../reference/hux_logo.md).
- Bugfix: rbind of huxtable and matrix wasn’t setting `row_height`
  correctly.

## huxtable 4.5.0

CRAN release: 2019-03-19

- Add [`quick_latex()`](../reference/quick-output.md) function.
- The `texreg` package now includes a `huxtablereg` function, analogous
  to `huxreg`, which outputs a huxtable from a list of regressions. This
  will be available from the next version of `texreg`.

## huxtable 4.4.0

CRAN release: 2019-03-03

- Huxtables can now be printed directly in Word documents and Powerpoint
  presentations, thanks to the `flextable` package and recent versions
  of Pandoc. (Powerpoint printing requires Pandoc \>= 2.4.0.)
- New “wrapleft” and “wrapright” options to
  [`position()`](../reference/position.md) allow text wrapping around
  tables.
- New [`set_outer_border_colors()`](../reference/set-outer.md) and
  [`set_outer_border_styles()`](../reference/set-outer.md) functions,
  like [`set_outer_borders()`](../reference/set-outer.md).
- Huxtable no longer requires the `broom` package, instead using the
  `generics` package. If you use [`huxreg()`](../reference/huxreg.md),
  you will still need e.g. `broom` or `broom.mixed` to provide
  [`tidy()`](https://generics.r-lib.org/reference/tidy.html) and
  [`glance()`](https://generics.r-lib.org/reference/glance.html) methods
  for specific models.
- Bugfix: [`tidy.tidy_override()`](../reference/tidy_override.md) and
  [`glance.tidy_override()`](../reference/tidy_override.md) should work
  even if underlying object has no
  [`tidy()`](https://generics.r-lib.org/reference/tidy.html) or
  [`glance()`](https://generics.r-lib.org/reference/glance.html) method.
- Bugfix: huxtables had option clash when `echo = TRUE` in Rmd
  pdf_document format.
- Bugfix: [`caption()`](../reference/caption.md) and
  [`height()`](../reference/height.md) weren’t playing nicely.
- Bugfix: `mutate(..., copy_cell_props = FALSE)` was adding a column
  named `copy_cell_props`.
- Bugfix: `check_latex_dependencies` and `install_latex_dependencies`
  gave misleading errors.
- Enhancement: when `stars` is `NULL` in `huxreg`, don’t print a note by
  default.
- Enhancement: use `tinytex` when available, allowing autoinstallation
  of latex packages.

## huxtable 4.3.0

CRAN release: 2018-11-07

- More work on TeX. Tables *should* now compile when raw_attributes is
  not set.
- New `map_xxx` functions to set properties variably by cell values.
- Functions for mapping properties variably: `by_rows`, `by_values`,
  `by_ranges`, `by_quantiles` etc.
- Correct bookdown labels are now automatically created.
- New grey, blue, green and orange themes.
- New “themes” vignette.
- New `tidy_override` function to override p values etc. in `huxreg`.
- New `set_contents` function to change huxtable contents within dplyr
  pipes.
- Enhancement: left- and right-aligned captions are now set above the
  table in LaTeX, using the “threeparttable” package. You will need to
  install this using
  e.g. [`install_latex_dependencies()`](../reference/report_latex_dependencies.md)
  or `tlmgr` if it is not already on your system.
- Enhancement: in [`huxtable()`](../reference/huxtable.md) and friends,
  `add_rownames = "Colname"` now sets the name for the new column.
- Improvements to the vignettes and help files.
- Bugfix: to_md could hang with bold/italic cells.

### Deprecated

- The 3 argument form of `set_xxx` functions is deprecated, as is the
  `where` function. Use `map_xxx` instead.
- Argument `byrow` is soft-deprecated. Use
  [`by_cols()`](../reference/by_rows.md) instead.

## huxtable 4.2.1

CRAN release: 2018-10-26

- Bugfix: `wrap=TRUE` caused squeezed text in RTF.

### Important

- TeX code was getting escaped by pandoc. To avoid this, if possible,
  huxtable now adds fenced code blocks round latex tables (see
  <https://pandoc.org/MANUAL.html#extension-raw_attribute>). You must
  add

  md_extensions: +raw_attribute

  to your YAML header for this to work, and you will need a recent (\>
  2.0.0) version of Pandoc.

## huxtable 4.2.0

CRAN release: 2018-10-03

- More speedups: LaTeX 2-3x faster, as_Workbook 2-3x faster.
- Simplify LaTeX output using our own LaTeX commands.
- RTF support: new `print_rtf`, `to_rtf` and `quick_rtf` functions.
- New `border_style` properties to set “solid”, “double”, “dotted” or
  “dashed” borders. (At present, LaTeX only allows “solid” or “double”.)
- New `merge_cells` function, an alternative interface to `colspan` and
  `rowspan`.
- New `quick_pptx` function to print data frames and huxtables into
  Powerpoint.
- New `install_latex_dependencies` and `check_latex_dependencies`
  utility functions.
- `add_rows` and `add_columns` now accept data frames as arguments.
- New `theme_mondrian` theme :-D
- Enhancement: `print_md` now handles **bold** and *italic* cells.
- Enhancement: `quick_pdf` has new `width` and `height` options to
  change paper size.
- Use CSS writing-mode where possible for text rotation. Note that this
  may break on non-LTR languages. If this affects you, please file an
  issue.
- Bugfix: LaTeX didn’t compile when height and caption were both set.
- Bugfix: `print_screen` and `print_md` would hang with a wide huxtable.
- Tweaks to documentation.

## huxtable 4.1.0

CRAN release: 2018-07-31

- dplyr, knitr, rmarkdown and some other packages have moved to
  “Suggests:”, lowering the dependency load considerably. All the
  functionality is still present. huxtable gives an informative warning
  if a needed package is not installed.
- Code rewrites for better performance and maintainability: HTML is up
  to 10x faster, LaTeX is up to 4x faster.
- Documentation improvements.
- New `tribble_hux` function wrapping
  [`tibble::tribble()`](https://tibble.tidyverse.org/reference/tribble.html)
  for readable data input.
- New `add_rows` and `add_columns` functions to insert one or more rows
  into the middle of a huxtable.
- New option “huxtable.knitr_output_format” to override the default
  output format in knitr documents.
- Numeric row heights and column widths are rescaled to 1 when huxtables
  are cbinded/rbinded.
- LaTeX: at points where borders cross, priority is given to the
  horizontal border color.
- Bugfix: property accessors had the wrong environment. Thanks to Iñaki
  Úcar.
- Bugfix: row heights and column widths weren’t being copied with
  cbind/rbind.
- Bugfixes for 0-row or 0-column huxtables:
  - Output works, usually with a warning.
  - cbind and rbind work.
- Bugfix: HTML cols were printed with ‘width: NA’.
- Bugfix: width, col_width etc. can be reset to a number after setting
  them to a string.
  - The (undocumented) ability to mix numeric and non-numeric values for
    padding and/border widths has been removed. If you want a number,
    set a number and not a string.
- Bugfix: HTML tables with position “right” weren’t right-aligned.
- Nicer error messages when rbinding objects with different numbers of
  rows.
- Vignette improvements.
- `is_a_number` is deprecated.
- … and a cool new randomized [`hux_logo()`](../reference/hux_logo.md)
  ;-)

## huxtable 4.0.1

CRAN release: 2018-07-03

- Improved formatting in Excel output.
- New `format` method which returns the result of `to_html`, `to_latex`
  etc. as appropriate.
- Bugfix: `to_html` printing e.g. “left-border: NA;” in cell CSS.
- Bugfix: `set_all_*` not working when huxtable is not attached.
- Bugfix: `as_Workbook` failing with non-numeric `width`.
- Bugfix: `hux_logo` was using multiple fonts, fails with Excel output.
- Bugfix: `as_flextable` borders not working in cells with colspan \> 1.
- Documentation bugfixes.
- Compatibility with broom 5.0.0 - thanks
  [@alexpghayes](https://github.com/alexpghayes)

## huxtable 4.0.0

CRAN release: 2018-06-02

- New `theme_plain` theme.
- The default value for `add_colnames` is going to become `TRUE`. At
  present it remains `FALSE`. Set `options("huxtable.add_colnames")` to
  `TRUE` or `FALSE` to set the default and avoid warnings in future.
- `quick_*` functions now automatically open documents if used
  interactively. Use `open = FALSE` to avoid.
- Tweak top and bottom margins for HTML tables.
- `pad_decimal` is deprecated in favour of `align(ht) <- "."`.
- `huxreg` continues with a warning if `statistics` are unavailable for
  some models.

### Breaking changes

- huxtable now provides `knit_print.data.frame` methods. This means that
  bare data frames will be pretty-printed via huxtable if the package is
  loaded.
  - Set `options("huxtable.knit_print_df")` to `FALSE` if you don’t want
    this.
  - By default data frames are printed using the `theme_plain` theme.
    Set options(“huxtable.knit_print_df_theme”) to a different
    one-argument function if you want to use a different theme.
- The new `autoformat` argument lets
  [`huxtable()`](../reference/huxtable.md) and
  [`as_huxtable()`](../reference/as_huxtable.md) automatically choose
  alignment and number format based on column type. Set
  `options("huxtable.autoformat")` to `FALSE` to turn off this feature
  by default.
- The default value of `number_format` has changed from “%5.3g” to
  “%.3g”, which no longer space-pads numbers.
- `as_flextable` now does not print column names in the header. This
  matches the standard huxtable behaviour whereby headers are “just
  another row/column”. To get the old behaviour, use
  `colnames_to_header = TRUE`.

### Bugfixes

- Bugfix: Date and datetime columns were converted to numbers by
  `add_colnames`.
- LaTeX bugfix: background colors were printing an extra space.
- `huxreg` was never using built-in confidence intervals.
- Screen bugfixes:
  - set max_width to screen width (thanks
    [@jacob-long](https://github.com/jacob-long))
  - misaligned decimal points
- Various bugfixes for `number_format`, `huxreg`, `as_hux.table`,
  `as_flextable`.

## huxtable 3.0.0

CRAN release: 2018-02-23

- Output to Excel workbooks using the `openxlsx` package.
- New `quick_xlsx` function.
- dplyr select helpers now work inside `set_*` column specifications:
  e.g. set_bold(ht, 1:3, matches(“ab”), TRUE)
- Column names can now be used for the `after` argument to
  `insert_column`.
- `quick_*` functions: when the `file` argument is not explicitly
  specified, confirm overwrites manually, or fail if called
  non-interactively.
- Add pointless quote marks in Description and Title… I don’t make the
  rules.
- Don’t apply number_format to negative exponents (e.g. 1.12e-3).
- New `tidy_args` argument to huxreg allows per-model customization of
  the call to `tidy`.

### Breaking changes

- `quick_xxx` functions without an explicit `file` argument throw an
  error if called non-interactively, and prompt before overwriting files
  if called interactively.

## huxtable 2.0.2

CRAN release: 2018-02-08

- Don’t apply `number_format` to exponents in scientific notation.
- Turn off some tests on CRAN, as they fail there but not elsewhere.

## huxtable 2.0.1

- Fix `quick_pdf` error when moving output across filesystems.

## huxtable 2.0.0

CRAN release: 2018-01-02

- New `quick_html`, `quick_pdf` and `quick_docx` functions to print
  table-like objects to a new document.
- `to_screen` only shows colnames if there are any non-zero-length
  column names.

### Breaking changes

- `number_format` now applies to any number-like substrings in cells.
  This means you can include e.g. significance stars in a cell and still
  use `number_format` to format the content.
- If `number_format` is NA, numbers are unchanged.
- Default value of `number_format` has changed from “%5.2f” to “%5.3g”,
  which plays nicer with integers but may surprise you by using
  scientific format for large numbers.

## huxtable 1.2.0

CRAN release: 2017-12-17

- New `outer_borders` argument for `huxreg`. This changes default
  behaviour slightly.
- New `border` argument for `add_footnote` to choose width of footnote’s
  top border.
- Added guard assertions to many exported functions.
- Bugfix: captions and colnames are wrapped in to_screen to respect
  max_width.

## huxtable 1.1.0

CRAN release: 2017-10-20

- No more ugly autocreated column names.
- Allow huxtable to have invalid or empty column names in general.
- LaTeX should now be *much* faster on large tables.
- `set_outer_borders` now accepts the same row/column arguments as other
  `set_` functions.
- Better handling in LaTeX of horizontal borders which don’t cross the
  entire table. (But not varying positive border widths….)
- Bugfix: flextable didn’t like huxreg’s syntactically invalid column
  names.
- Accept, but silently change, English spelling of ‘centre’ in `align`,
  `position` and `caption_pos`.

## huxtable 1.0.0

CRAN release: 2017-10-07

- LaTeX implements different thicknesses for vertical and horizontal
  borders (but only one horizontal thickness per row).
- LaTeX border colors now collapse nicely: set colors override unset
  ones.
- React gracefully to lack of p values in `huxreg`.
- New `set_outer_borders` function to set borders round a rectangle of
  cells.
- `to_screen` and `to_md` now respect `wrap` and `col_widths`
  properties.
- Screen and markdown wrap respect word boundaries.
- `to_screen` and `to_md` gain a `min_width` argument; `to_md` gains a
  logical `header` argument; `to_screen` gains a `compact` argument
  replacing `blank = NULL`.
- On screen colour and bold support, if the `crayon` package is
  installed. New `huxtable.color_screen` option.
- Move from `ReporteRs` to `officer` and `flextable`. No more `RJava`
  horror.
- New `error_format` argument to `huxreg` for flexible control over
  uncertainty estimates.
- Infrastructure improvements: slightly less ugly code in screen.R and
  LaTeX.R.

### Breaking changes

- Removed options `collapse`, `borders`, `blank` and `colname_color`
  from `to_screen`/`print_screen`.
- `as_FlexTable` is deprecated and calls `as_flextable` with a warning.
  `header_rows` and `footer_rows` arguments are ignored. If you need
  this feature, tell me.
- HTML border sizes are now set in points, not pixels.
- In `huxreg`:
  - `ci_level` is `NULL` by default. Set it to a number to calculate
    confidence intervals.
  - `error_style` is deprecated with a warning in favour of
    `error_format`.
  - Use [stars](https://r-spatial.github.io/stars/) not `%stars%` to
    display significance levels in the `note` argument.
  - `borders` becomes a number specifying border width. Set to 0 for no
    borders.

## huxtable 0.3.1

CRAN release: 2017-09-12

- New convenience functions `insert_row` and `insert_column`.
- `latex_float` property allows you to change positioning in LaTeX.
- (Semantic versioning fail: this should have been 0.4.0.)

## huxtable 0.3.0

CRAN release: 2017-05-18

- New borders argument for huxreg, gives borders in sensible places.
- Allow more flexible caption positioning with `caption_pos`.
- New `set_default_properties` function to set default properties for
  new huxtables.
- Fix compatibility with dplyr 0.6.0.

## huxtable 0.2.2

CRAN release: 2017-04-27

- Fix a bug that could lead to wrong significance stars in `huxreg`.

## huxtable 0.2.1

CRAN release: 2017-04-24

- Compatibility with dplyr 0.6.0.
- Use ~ for decimal padding in LaTeX.

## huxtable 0.2.0

CRAN release: 2017-04-21

- New `huxreg` function to convert a list of models to a huxtable.
- New set\_\* interface allowing column ranges, expressions a la
  `subset`, and filling in values by row.
- Replacement methods `$<-`, `[<-` and `[[<-` now work better.
- New function `set_cell_properties` to set multiple properties on
  cells.
- `evens`, `odds`, `everywhere`, `every(n, from)`, `final(n)`,
  `where(cond)`: convenience functions to select rows, columns and
  cells.
- Export to Word/Powerpoint via `ReporteRs`.
- Huxtable now supports dplyr verbs like `filter` and `select`.
- Exported function `guess_knitr_output_format`.
- Ability to set border colors.
- Prevent overlapping row/colspans.
- Expanded introduction and new vignette for `huxreg`.
- Numerous bugs have been fixed and replaced with new, more advanced
  bugs.

### Breaking changes

- `theme_minimal` has been renamed `theme_basic` to avoid a name clash
  with `ggplot2`.

## huxtable 0.1.0

CRAN release: 2017-03-09

- Added a `NEWS.md` file to track changes to the package.
- First CRAN release.
