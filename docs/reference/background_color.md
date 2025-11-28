# Set cell background color

Colors can be in any format understood by R:

- A color name like `"darkred"`

- A HTML string like `"#FF0000"`

- The result of a function like `rgb(1, 0, 0)` or `grey(0.5)`

## Usage

``` r
background_color(ht)

background_color(ht) <- value

set_background_color(ht, row, col, value)

map_background_color(ht, row, col, fn)
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

## Details

Transparent colors are not guaranteed to work at present.

## See also

Other formatting functions: [`bold()`](bold.md), [`font()`](font.md),
[`font_size()`](font_size.md), [`na_string()`](na_string.md),
[`number_format()`](number_format.md), [`text_color()`](text_color.md)

## Examples

``` r
background_color(jams) <- grey(0.7)
background_color(jams)
#>     Type      Price    
#> 1   "#B3B3B3" "#B3B3B3"
#> 1.1 "#B3B3B3" "#B3B3B3"
#> 2   "#B3B3B3" "#B3B3B3"
#> 3   "#B3B3B3" "#B3B3B3"

set_background_color(jams, "yellow")
#>                               Type           Price  
#>                               Strawberry      1.90  
#>                               Raspberry       2.10  
#>                               Plum            1.80  
#> 
#> Column names: Type, Price
set_background_color(jams, 2:3, 1, "yellow")
#>                               Type           Price  
#>                               Strawberry      1.90  
#>                               Raspberry       2.10  
#>                               Plum            1.80  
#> 
#> Column names: Type, Price
map_background_color(jams, by_rows("yellow", grey(0.7)))
#>                               Type           Price  
#>                               Strawberry      1.90  
#>                               Raspberry       2.10  
#>                               Plum            1.80  
#> 
#> Column names: Type, Price
```
