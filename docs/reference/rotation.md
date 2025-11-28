# Rotate text within cells

Numbers represent degrees to rotate text anti-clockwise:

## Usage

``` r
rotation(ht)

rotation(ht) <- value

set_rotation(ht, row, col, value)

map_rotation(ht, row, col, fn)
```

## Arguments

- ht:

  A huxtable.

- value:

  A numeric vector or matrix. Set to `NA` to reset to the default, which
  is `0`.

- row:

  A row specifier. See [rowspecs](rowspecs.md) for details.

- col:

  An optional column specifier.

- fn:

  A mapping function. See [mapping-functions](mapping-functions.md) for
  details.

## Details

- 0 is the default;

- 90 is going upwards, for left-to-right languages;

- 270 is going downwards.

You will probably need to set [`col_width()`](col_width.md) and
[`row_height()`](row_height.md) explicitly to achieve a nice result, in
both HTML and LaTeX.

## Examples

``` r
rotation(jams) <- 90
rotation(jams)
#>     Type Price
#> 1     90    90
#> 1.1   90    90
#> 2     90    90
#> 3     90    90

jams2 <- set_rotation(
  jams,
  270
)
rotation(jams2)
#>     Type Price
#> 1    270   270
#> 1.1  270   270
#> 2    270   270
#> 3    270   270

jams3 <- set_rotation(
  jams,
  2:3, 1, 270
)
rotation(jams3)
#>     Type Price
#> 1     90    90
#> 1.1  270    90
#> 2    270    90
#> 3     90    90

jams4 <- map_rotation(
  jams,
  by_rows(
    270,
    90
  )
)
rotation(jams4)
#>     Type Price
#> 1    270   270
#> 1.1   90    90
#> 2    270   270
#> 3     90    90
```
