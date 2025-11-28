# Make text larger or smaller

Font size is in points.

## Usage

``` r
font_size(ht)

font_size(ht) <- value

set_font_size(ht, row, col, value)

map_font_size(ht, row, col, fn)
```

## Arguments

- ht:

  A huxtable.

- value:

  A numeric vector. Set to `NA` to reset to the default, which is
  `NA_real_`.

- row:

  A row specifier. See [rowspecs](rowspecs.md) for details.

- col:

  An optional column specifier.

- fn:

  A mapping function. See [mapping-functions](mapping-functions.md) for
  details.

## See also

Other formatting functions: [`background_color()`](background_color.md),
[`bold()`](bold.md), [`font()`](font.md), [`na_string()`](na_string.md),
[`number_format()`](number_format.md), [`text_color()`](text_color.md)

## Examples

``` r
font_size(jams) <- 14
font_size(jams)
#>     Type Price
#> 1     14    14
#> 1.1   14    14
#> 2     14    14
#> 3     14    14

jams2 <- set_font_size(
  jams,
  12
)
font_size(jams2)
#>     Type Price
#> 1     12    12
#> 1.1   12    12
#> 2     12    12
#> 3     12    12

jams3 <- set_font_size(
  jams,
  2:3, 1, 12
)
font_size(jams3)
#>     Type Price
#> 1     14    14
#> 1.1   12    14
#> 2     12    14
#> 3     14    14

jams4 <- map_font_size(
  jams,
  by_rows(
    12,
    14
  )
)
font_size(jams4)
#>     Type Price
#> 1     12    12
#> 1.1   14    14
#> 2     12    12
#> 3     14    14
```
