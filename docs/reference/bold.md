# Make cell text bold or italic

Make cell text bold or italic

## Usage

``` r
bold(ht)

bold(ht) <- value

set_bold(ht, row, col, value = TRUE)

map_bold(ht, row, col, fn)

italic(ht)

italic(ht) <- value

set_italic(ht, row, col, value = TRUE)

map_italic(ht, row, col, fn)
```

## Arguments

- ht:

  A huxtable.

- value:

  A logical vector or matrix. Set to `NA` to reset to the default, which
  is `FALSE`.

- row:

  A row specifier. See [rowspecs](rowspecs.md) for details.

- col:

  An optional column specifier.

- fn:

  A mapping function. See [mapping-functions](mapping-functions.md) for
  details.

## See also

Other formatting functions: [`background_color()`](background_color.md),
[`font()`](font.md), [`font_size()`](font_size.md),
[`na_string()`](na_string.md), [`number_format()`](number_format.md),
[`text_color()`](text_color.md)

## Examples

``` r
bold(jams) <- TRUE
bold(jams)
#>     Type Price
#> 1   TRUE  TRUE
#> 1.1 TRUE  TRUE
#> 2   TRUE  TRUE
#> 3   TRUE  TRUE

set_bold(jams, FALSE)
#>                               Type           Price  
#>                               Strawberry      1.90  
#>                               Raspberry       2.10  
#>                               Plum            1.80  
#> 
#> Column names: Type, Price
set_bold(
  jams,
  2:3, 1, FALSE
)
#>                               Type           Price  
#>                               Strawberry      1.90  
#>                               Raspberry       2.10  
#>                               Plum            1.80  
#> 
#> Column names: Type, Price
map_bold(
  jams,
  by_rows(FALSE, TRUE)
)
#>                               Type           Price  
#>                               Strawberry      1.90  
#>                               Raspberry       2.10  
#>                               Plum            1.80  
#> 
#> Column names: Type, Price
```
