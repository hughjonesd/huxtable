# Change how NA values are printed

NA values in the huxtable are printed as the value of `na_string`.

## Usage

``` r
na_string(ht)

na_string(ht) <- value

set_na_string(ht, row, col, value)

map_na_string(ht, row, col, fn)
```

## Arguments

- ht:

  A huxtable.

- value:

  A character vector or matrix. Set to `NA` to reset to the default,
  which is `""`.

- row:

  A row specifier. See [rowspecs](rowspecs.md) for details.

- col:

  An optional column specifier.

- fn:

  A mapping function. See [mapping-functions](mapping-functions.md) for
  details.

## See also

Other formatting functions: [`background_color()`](background_color.md),
[`bold()`](bold.md), [`font()`](font.md), [`font_size()`](font_size.md),
[`number_format()`](number_format.md), [`text_color()`](text_color.md)

## Examples

``` r
jams[3, 2] <- NA
jams
#>                               Type           Price  
#>                               Strawberry      1.90  
#>                               Raspberry             
#>                               Plum            1.80  
#> 
#> Column names: Type, Price
set_na_string(jams, "---")
#>                               Type           Price  
#>                               Strawberry      1.90  
#>                               Raspberry     ---     
#>                               Plum            1.80  
#> 
#> Column names: Type, Price
```
