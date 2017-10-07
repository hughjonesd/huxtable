

# huxtable 0.3.1.9000

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



