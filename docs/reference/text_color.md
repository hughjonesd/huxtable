# Set the color of text in cells

Colors can be in any format understood by R:

- A color name like `"darkred"`

- A HTML string like `"#FF0000"`

- The result of a function like `rgb(1, 0, 0)` or `grey(0.5)`

## Usage

``` r
text_color(ht)

text_color(ht) <- value

set_text_color(ht, row, col, value)

map_text_color(ht, row, col, fn)
```

## Arguments

- ht:

  A huxtable.

- value:

  A character vector or matrix. Set to `NA` to reset to the default,
  which is `NA_character_`.

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
[`na_string()`](na_string.md), [`number_format()`](number_format.md)

## Examples

``` r
text_color(jams) <- "blue"
text_color(jams)
#>     Type   Price 
#> 1   "blue" "blue"
#> 1.1 "blue" "blue"
#> 2   "blue" "blue"
#> 3   "blue" "blue"

set_text_color(jams, "red")
#>                               Type           Price  
#>                               Strawberry      1.90  
#>                               Raspberry       2.10  
#>                               Plum            1.80  
#> 
#> Column names: Type, Price
set_text_color(jams, 2:3, 1, "red")
#>                               Type           Price  
#>                               Strawberry      1.90  
#>                               Raspberry       2.10  
#>                               Plum            1.80  
#> 
#> Column names: Type, Price
map_text_color(jams, by_rows("red", "blue"))
#>                               Type           Price  
#>                               Strawberry      1.90  
#>                               Raspberry       2.10  
#>                               Plum            1.80  
#> 
#> Column names: Type, Price
```
