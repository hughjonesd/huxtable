# Set the height of table rows

Numeric heights are scaled to 1 and treated as proportions of the table
height in HTML, or of the text height (`\textheight`) in LaTeX.
Character row heights must be valid CSS or LaTeX dimensions.

## Usage

``` r
row_height(ht)

row_height(ht) <- value

set_row_height(ht, row, value)
```

## Arguments

- ht:

  A huxtable.

- value:

  Numeric or character vector. Set to `NA` to reset to the default,
  which is NA.

- row:

  A row specifier. See [rowspecs](rowspecs.md) for details.

## Value

`row_height()` returns the `row_height` property. `set_row_height()`
returns the modified huxtable.

## See also

[`col_width()`](col_width.md), [`height()`](height.md),
[`width()`](width.md)

Other table measurements: [`col_width()`](col_width.md),
[`height()`](height.md), [`width()`](width.md)

## Examples

``` r
row_height(jams) <- c(.4, .2, .2, .2)
row_height(jams)
#>   1 1.1   2   3 
#> 0.4 0.2 0.2 0.2 
```
