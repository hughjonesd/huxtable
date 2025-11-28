# Set the table height

`height()` sets the height of the entire table, while
[`row_height()`](row_height.md) sets the height of individual rows. A
numeric height is treated as a proportion of the containing block (HTML)
or `\textheight` (LaTeX). A character height must be a valid CSS or
LaTeX dimension.

## Usage

``` r
height(ht)

height(ht) <- value

set_height(ht, value)
```

## Arguments

- ht:

  A huxtable.

- value:

  A number or string. Set to `NA` to reset to the default, which is
  `NA_real_`.

## Value

`property()` returns the property value(s). `set_property()` and
`map_property()` return the modified huxtable.

## See also

Other table measurements: [`col_width()`](col_width.md),
[`row_height()`](row_height.md), [`width()`](width.md)

## Examples

``` r
set_height(jams, 0.4)
#>                               Type           Price  
#>                               Strawberry      1.90  
#>                               Raspberry       2.10  
#>                               Plum            1.80  
#> 
#> Column names: Type, Price
```
