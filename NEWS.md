
Note that huxtable attempts to follow semantic versioning (https://semver.org). Therefore, major version
increments reflect backwards-incompatible API changes, not necessarily big changes.

# huxtable 3.0.0.9000

* The default value for `add_colnames` is going to become `TRUE`. At present it remains `FALSE`. Set 
  `options("huxtable.add_colnames")` to `TRUE` or `FALSE` to set the default and avoid warnings in
  future.
* New `theme_plain` theme.
* huxtable now provides `knit_print.data.frame` methods, using `theme_plain` by default. 
* `quick_*` functions now automatically open documents if used interactively. Use `open = FALSE` to
  avoid.
* Tweak top and bottom margins for HTML tables.
* `pad_decimal` is deprecated in favour of `align(ht) <- "."`.
* `huxreg` continues with a warning if `statistics` are unavailable for some models.

## Bugfixes

* Various bugfixes for `number_format`, `huxreg`, `as_hux.table`.
* LaTeX bugfix: background colors were printing an extra space.
* Bugfix: `huxreg` was never using built-in confidence intervals.
* Screen bugfixes:
  - set max_width to screen width (thanks @jacob-long)
  - misaligned decimal points

  
## Breaking changes 

* Default value of `number_format` has changed from "%5.3g" to "%.3g", which no longer space-pads numbers.
* `as_flextable` now does not print column names in the header. This matches the standard
  huxtable behaviour whereby headers are "just another row/column". To get the old behaviour, 
  use `colnames_to_header = TRUE`.

# huxtable 3.0.0

* Output to Excel workbooks using the `openxlsx` package.
* New `quick_xlsx` function.
* dplyr select helpers now work inside `set_*` column specifications: e.g. set_bold(ht, 1:3, matches("ab"), TRUE)
* Column names can now be used for the `after` argument to `insert_column`.
* `quick_*` functions: when the `file` argument is not explicitly specified, confirm overwrites manually, or fail 
  if called non-interactively.
* Add pointless quote marks in Description and Title... I don't make the rules.
* Don't apply number_format to negative exponents (e.g. 1.12e-3).
* New `tidy_args` argument to huxreg allows per-model customization of the call to `tidy`.

## Breaking changes

* `quick_xxx` functions without an explicit `file` argument throw an error if called non-interactively,
  and prompt before overwriting files if called interactively.

# huxtable 2.0.2

* Don't apply `number_format` to exponents in scientific notation.
* Turn off some tests on CRAN, as they fail there but not elsewhere.

# huxtable 2.0.1

* Fix `quick_pdf` error when moving output across filesystems.

# huxtable 2.0.0

* New `quick_html`, `quick_pdf` and `quick_docx` functions to print table-like objects to a new document.
* `to_screen` only shows colnames if there are any non-zero-length column names.

## Breaking changes

* `number_format` now applies to any number-like substrings in cells. This means you can include e.g.
  significance stars in a cell and still use `number_format` to format the content.
* If `number_format` is NA, numbers are unchanged. 
* Default value of `number_format` has changed from "%5.2f" to "%5.3g", which plays nicer with integers
  but may surprise you by using scientific format for large numbers.

# huxtable 1.2.0

* New `outer_borders` argument for `huxreg`. This changes default behaviour slightly.
* New `border` argument for `add_footnote` to choose width of footnote's top border.
* Added guard assertions to many exported functions.
* Bugfix: captions and colnames are wrapped in to_screen to respect max_width.

# huxtable 1.1.0

* No more ugly autocreated column names.
* Allow huxtable to have invalid or empty column names in general.
* LaTeX should now be *much* faster on large tables.
* `set_outer_borders` now accepts the same row/column arguments as other `set_` functions.
* Better handling in LaTeX of horizontal borders which don't cross the entire table. (But not varying
  positive border widths....)
* Bugfix: flextable didn't like huxreg's syntactically invalid column names.
* Accept, but silently change, English spelling of 'centre' in `align`, `position` and `caption_pos`.

# huxtable 1.0.0

* LaTeX implements different thicknesses for vertical and horizontal borders (but only one horizontal thickness per row).
* LaTeX border colors now collapse nicely: set colors override unset ones.
* React gracefully to lack of p values in `huxreg`.
* New `set_outer_borders` function to set borders round a rectangle of cells.
* `to_screen` and `to_md` now respect `wrap` and `col_widths` properties.
* Screen and markdown wrap respect word boundaries.
* `to_screen` and `to_md` gain a `min_width` argument; `to_md` gains a logical `header` argument; `to_screen` gains
  a `compact` argument replacing `blank = NULL`.
* On screen colour and bold support, if the `crayon` package is installed. New `huxtable.color_screen` option.
* Move from `ReporteRs` to `officer` and `flextable`. No more `RJava` horror.
* New `error_format` argument to `huxreg` for flexible control over uncertainty estimates.
* Infrastructure improvements: slightly less ugly code in screen.R and LaTeX.R.


## Breaking changes

* Removed options `collapse`, `borders`, `blank` and `colname_color` from `to_screen`/`print_screen`.
* `as_FlexTable` is deprecated and calls `as_flextable` with a warning. `header_rows` and `footer_rows` 
  arguments are ignored. If you need this feature, tell me.
* HTML border sizes are now set in points, not pixels.
* In `huxreg`:
  - `ci_level` is `NULL` by default. Set it to a number to calculate confidence intervals.
  - `error_style` is deprecated with a warning in favour of `error_format`.
  - Use `{stars}` not `%stars%` to display significance levels in the `note` argument.
  - `borders` becomes a number specifying border width. Set to 0 for no borders.

# huxtable 0.3.1

* New convenience functions `insert_row` and `insert_column`.
* `latex_float` property allows you to change positioning in LaTeX.
* (Semantic versioning fail: this should have been 0.4.0.)

# huxtable 0.3.0

* New borders argument for huxreg, gives borders in sensible places.
* Allow more flexible caption positioning with `caption_pos`.
* New `set_default_properties` function to set default properties for new huxtables.
* Fix compatibility with dplyr 0.6.0.

# huxtable 0.2.2

* Fix a bug that could lead to wrong significance stars in `huxreg`.

# huxtable 0.2.1

* Compatibility with dplyr 0.6.0.
* Use ~ for decimal padding in LaTeX.

# huxtable 0.2.0

* New `huxreg` function to convert a list of models to a huxtable.
* New set_* interface allowing column ranges, expressions a la `subset`, and filling in values by row.
* Replacement methods `$<-`, `[<-` and `[[<-` now work better.
* New function `set_cell_properties` to set multiple properties on cells.
* `evens`, `odds`, `everywhere`, `every(n, from)`, `final(n)`, `where(cond)`: 
  convenience functions to select rows, columns and cells.
* Export to Word/Powerpoint via `ReporteRs`.
* Huxtable now supports dplyr verbs like `filter` and `select`.
* Exported function `guess_knitr_output_format`.
* Ability to set border colors.
* Prevent overlapping row/colspans.
* Expanded introduction and new vignette for `huxreg`.
* Numerous bugs have been fixed and replaced with new, more advanced bugs.

## Breaking changes

* `theme_minimal` has been renamed `theme_basic` to avoid a name clash with `ggplot2`.

# huxtable 0.1.0

* Added a `NEWS.md` file to track changes to the package.
* First CRAN release.



