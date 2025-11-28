# Set the "table" environment in LaTeX

By default this is `"table"`.

## Usage

``` r
table_environment(ht)

table_environment(ht) <- value

set_table_environment(ht, value)
```

## Arguments

- ht:

  A huxtable.

- value:

  A string. Set to `NA` to reset to the default, which is `"table"`.

## Value

`property()` returns the property value(s). `set_property()` and
`map_property()` return the modified huxtable.

## Details

No features are guaranteed to work if you set this to a non-default
value. Use at your own risk! In particular, you may need to set
[`latex_float()`](latex_float.md) to a non-default value.

If [`position()`](position.md) is set to `"wrapleft"` or `"wrapright"`,
this value is overridden.

## Examples

``` r
set_table_environment(jams, "table*")
#>                               Type           Price  
#>                               Strawberry      1.90  
#>                               Raspberry       2.10  
#>                               Plum            1.80  
#> 
#> Column names: Type, Price
```
