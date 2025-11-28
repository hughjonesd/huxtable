# Set the font for cell text

Set the font for cell text

## Usage

``` r
font(ht)

font(ht) <- value

set_font(ht, row, col, value)

map_font(ht, row, col, fn)
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

To find out what fonts are on your system,
[`systemfonts::match_font()`](https://systemfonts.r-lib.org/reference/match_fonts.html)
is useful.

For HTML, you can use comma-separated lists of font names like
`"Times New Roman, Times, Serif"`. This is not portable, though.

LaTeX and HTML use different font names. To use the same font names
across document formats, see `options("huxtable.latex_use_fontspec")` in
[huxtable-options](huxtable-options.md).

## See also

Other formatting functions: [`background_color()`](background_color.md),
[`bold()`](bold.md), [`font_size()`](font_size.md),
[`na_string()`](na_string.md), [`number_format()`](number_format.md),
[`text_color()`](text_color.md)

## Examples

``` r
font(jams) <- "times"
font(jams)
#>     Type    Price  
#> 1   "times" "times"
#> 1.1 "times" "times"
#> 2   "times" "times"
#> 3   "times" "times"

set_font(jams, "arial")
#>                               Type           Price  
#>                               Strawberry      1.90  
#>                               Raspberry       2.10  
#>                               Plum            1.80  
#> 
#> Column names: Type, Price
set_font(jams, 2:3, 1, "arial")
#>                               Type           Price  
#>                               Strawberry      1.90  
#>                               Raspberry       2.10  
#>                               Plum            1.80  
#> 
#> Column names: Type, Price
map_font(jams, by_rows("arial", "times"))
#>                               Type           Price  
#>                               Strawberry      1.90  
#>                               Raspberry       2.10  
#>                               Plum            1.80  
#> 
#> Column names: Type, Price
```
