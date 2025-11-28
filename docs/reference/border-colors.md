# Set border colors

These functions set border colors.

## Usage

``` r
left_border_color(ht)

right_border_color(ht)

top_border_color(ht)

bottom_border_color(ht)

left_border_color(ht) <- value

right_border_color(ht) <- value

top_border_color(ht) <- value

bottom_border_color(ht) <- value

set_left_border_color(ht, row, col, value)

set_right_border_color(ht, row, col, value)

set_top_border_color(ht, row, col, value)

set_bottom_border_color(ht, row, col, value)

map_left_border_color(ht, row, col, fn)

map_right_border_color(ht, row, col, fn)

map_top_border_color(ht, row, col, fn)

map_bottom_border_color(ht, row, col, fn)
```

## Arguments

- ht:

  A huxtable.

- value:

  A valid R color, e.g. `"red"`, `"#FF0000"`. Set to `NA` to reset to
  the default, which is `NA_character_`.

- row:

  A row specifier. See [rowspecs](rowspecs.md) for details.

- col:

  An optional column specifier.

- fn:

  A mapping function. See [mapping-functions](mapping-functions.md) for
  details.

## Details

Borders are always "collapsed": `right_border_color(ht)[, 1]` is the
same as `left_border_color(ht)[, 2]`, and setting one sets the other.

## Limitations

- Transparent borders with the alpha channel set are not guaranteed to
  work.

## See also

[set-multiple](set-multiple.md), [`brdr()`](brdr.md)

Other border properties: [`left_border()`](borders.md),
[`left_border_style()`](border-styles.md)

## Examples

``` r
jams <- set_all_borders(jams)
bottom_border_color(jams)[1, ] <- "red"
jams
#>                             ┌────────────┬─────────┐
#>                             │ Type       │   Price │
#>                             ├────────────┼─────────┤
#>                             │ Strawberry │    1.90 │
#>                             ├────────────┼─────────┤
#>                             │ Raspberry  │    2.10 │
#>                             ├────────────┼─────────┤
#>                             │ Plum       │    1.80 │
#>                             └────────────┴─────────┘
#> 
#> Column names: Type, Price

set_bottom_border_color(jams, "blue")
#>                             ┌────────────┬─────────┐
#>                             │ Type       │   Price │
#>                             ├────────────┼─────────┤
#>                             │ Strawberry │    1.90 │
#>                             ├────────────┼─────────┤
#>                             │ Raspberry  │    2.10 │
#>                             ├────────────┼─────────┤
#>                             │ Plum       │    1.80 │
#>                             └────────────┴─────────┘
#> 
#> Column names: Type, Price
```
