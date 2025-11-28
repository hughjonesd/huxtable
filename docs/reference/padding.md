# Cell padding

Functions to get or set the space around cell borders. Top, bottom, left
and right padding all default to 6 points.

## Usage

``` r
left_padding(ht)

left_padding(ht) <- value

set_left_padding(ht, row, col, value)

map_left_padding(ht, row, col, fn)

right_padding(ht)

right_padding(ht) <- value

set_right_padding(ht, row, col, value)

map_right_padding(ht, row, col, fn)

top_padding(ht)

top_padding(ht) <- value

set_top_padding(ht, row, col, value)

map_top_padding(ht, row, col, fn)

bottom_padding(ht)

bottom_padding(ht) <- value

set_bottom_padding(ht, row, col, value)

map_bottom_padding(ht, row, col, fn)
```

## Arguments

- ht:

  A huxtable.

- value:

  Numeric: padding width/height in points. Set to `NA` to reset to the
  default, which is `6`.

- row:

  A row specifier. See [rowspecs](rowspecs.md) for details.

- col:

  An optional column specifier.

- fn:

  A mapping function. See [mapping-functions](mapping-functions.md) for
  details.

## See also

[set-multiple](set-multiple.md), [set-outer](set-outer.md).

## Examples

``` r
left_padding(jams) <- 2
left_padding(jams)
#>     Type Price
#> 1      2     2
#> 1.1    2     2
#> 2      2     2
#> 3      2     2

jams <- set_left_padding(jams, 2)
left_padding(jams)
#>     Type Price
#> 1      2     2
#> 1.1    2     2
#> 2      2     2
#> 3      2     2
```
