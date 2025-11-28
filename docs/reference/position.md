# Set the table's position with respect to surrounding content

Table position may be "left", "right" or "center". If you want text to
wrap around the table, use "wrapleft" or "wrapright".

## Usage

``` r
position(ht)

position(ht) <- value

set_position(ht, value)
```

## Arguments

- ht:

  A huxtable.

- value:

  String. "left", "center", "right", "wrapleft" or "wrapright". Set to
  `NA` to reset to the default, which is `"center"`.

## Value

`property()` returns the property value(s). `set_property()` and
`map_property()` return the modified huxtable.

## Details

`"wrapleft"` and `"wrapright"` position the table to the left or right,
and allow text to wrap around the table.

## Examples

``` r
set_position(jams, "left")
#>   Type           Price  
#>   Strawberry      1.90  
#>   Raspberry       2.10  
#>   Plum            1.80  
#> 
#> Column names: Type, Price
set_position(jams, "right")
#>                                                           Type           Price  
#>                                                           Strawberry      1.90  
#>                                                           Raspberry       2.10  
#>                                                           Plum            1.80  
#> 
#> Column names: Type, Price
set_position(jams, "center")
#>                               Type           Price  
#>                               Strawberry      1.90  
#>                               Raspberry       2.10  
#>                               Plum            1.80  
#> 
#> Column names: Type, Price
```
