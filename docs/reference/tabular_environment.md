# Set the table's tabular environment in LaTeX

By default this is either `"tabular"` or `"tabularx"`.

## Usage

``` r
tabular_environment(ht)

tabular_environment(ht) <- value

set_tabular_environment(ht, value)
```

## Arguments

- ht:

  A huxtable.

- value:

  A string. Set to `NA` to reset to the default, which is
  `NA_character_`.

## Value

`property()` returns the property value(s). `set_property()` and
`map_property()` return the modified huxtable.

## Details

No features are guaranteed to work if you set this to a non-default
value. Use at your own risk!

## Examples

``` r
set_tabular_environment(jams, "longtable")
#>                               Type           Price  
#>                               Strawberry      1.90  
#>                               Raspberry       2.10  
#>                               Plum            1.80  
#> 
#> Column names: Type, Price
```
