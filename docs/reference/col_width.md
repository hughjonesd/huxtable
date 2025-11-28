# Set the width of table columns

Numeric column widths are treated as proportions of the table width.
Character widths must be valid CSS or LaTeX dimensions.

## Usage

``` r
col_width(ht)

col_width(ht) <- value

set_col_width(ht, col, value)
```

## Arguments

- ht:

  A huxtable.

- value:

  Numeric or character vector. Set to `NA` to reset to the default,
  which is NA.

- col:

  A column specifier. See [rowspecs](rowspecs.md) for details.

## Value

`col_width()` returns the `col_width` property. `set_col_width()`
returns the modified huxtable.

## Details

In LaTeX, if you specify a column width, but set `wrap` to `FALSE` and
have cells which overrun, then you may have problems with table position
and with background colours in other cells. The workaround is to adjust
the width, so that your cells no longer overrun.

## See also

Other table measurements: [`height()`](height.md),
[`row_height()`](row_height.md), [`width()`](width.md)

## Examples

``` r
col_width(jams) <- c(.2, .8)
col_width(jams)
#>  Type Price 
#>   0.2   0.8 

jams$Notes <- c(
  "Notes",
  "This year's finest", "", ""
)
jams
#>                            Type                Price  
#>                            Strawberry           1.90  
#>                            Raspberry            2.10  
#>                            Plum                 1.80  
#> 
#> Column names: Type, Price, Notes
#> 
#> 2/3 columns shown.
set_col_width(jams, c(.4, .5, .1))
#>                              Type            Price  
#>                              Strawberry       1.90  
#>                              Raspberry        2.10  
#>                              Plum             1.80  
#> 
#> Column names: Type, Price, Notes
#> 
#> 2/3 columns shown.
```
