# Set the table width

`width()` sets the width of the entire table, while
[`col_width()`](col_width.md) sets the width of individual columns. A
numeric width is treated as a proportion of f the surrounding block
width (HTML) or text width (LaTeX). A character width must be a valid
CSS or LaTeX dimension.

## Usage

``` r
width(ht)

width(ht) <- value

set_width(ht, value)
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
[`height()`](height.md), [`row_height()`](row_height.md)

## Examples

``` r
set_width(jams, 0.8)
#>                               Type           Price  
#>                               Strawberry      1.90  
#>                               Raspberry       2.10  
#>                               Plum            1.80  
#> 
#> Column names: Type, Price
```
