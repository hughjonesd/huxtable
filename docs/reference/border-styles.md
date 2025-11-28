# Set border styles

These functions set border styles.

## Usage

``` r
left_border_style(ht)

right_border_style(ht)

top_border_style(ht)

bottom_border_style(ht)

left_border_style(ht) <- value

right_border_style(ht) <- value

top_border_style(ht) <- value

bottom_border_style(ht) <- value

set_left_border_style(ht, row, col, value)

set_right_border_style(ht, row, col, value)

set_top_border_style(ht, row, col, value)

set_bottom_border_style(ht, row, col, value)

map_left_border_style(ht, row, col, fn)

map_right_border_style(ht, row, col, fn)

map_top_border_style(ht, row, col, fn)

map_bottom_border_style(ht, row, col, fn)
```

## Arguments

- ht:

  A huxtable.

- value:

  One of `"solid"`, `"double"`, `"dashed"` or `"dotted"`. Set to `NA` to
  reset to the default, which is `"solid"`.

- row:

  A row specifier. See [rowspecs](rowspecs.md) for details.

- col:

  An optional column specifier.

- fn:

  A mapping function. See [mapping-functions](mapping-functions.md) for
  details.

## Details

Borders are always "collapsed": `right_border_style(ht)[, 1]` is the
same as `left_border_style(ht)[, 2]`, and setting one sets the other.

## Limitations

- In HTML, you will need to set a width of at least 3 to get a double
  border.

- Only "solid" and "double" styles are currently implemented in LaTeX.

## See also

[set-multiple](set-multiple.md), [`brdr()`](brdr.md)

Other border properties: [`left_border()`](borders.md),
[`left_border_color()`](border-colors.md)

## Examples

``` r
jams <- set_all_borders(jams)
bottom_border_style(jams)[1, ] <- "dotted"
jams
#>                             ┌────────────┬─────────┐
#>                             │ Type       │   Price │
#>                             ├┈┈┈┈┈┈┈┈┈┈┈┈┼┈┈┈┈┈┈┈┈┈┤
#>                             │ Strawberry │    1.90 │
#>                             ├────────────┼─────────┤
#>                             │ Raspberry  │    2.10 │
#>                             ├────────────┼─────────┤
#>                             │ Plum       │    1.80 │
#>                             └────────────┴─────────┘
#> 
#> Column names: Type, Price

set_bottom_border_style(jams, "double")
#>                             ┌────────────┬─────────┐
#>                             │ Type       │   Price │
#>                             ├════════════┼═════════┤
#>                             │ Strawberry │    1.90 │
#>                             ├════════════┼═════════┤
#>                             │ Raspberry  │    2.10 │
#>                             ├════════════┼═════════┤
#>                             │ Plum       │    1.80 │
#>                             └════════════┴═════════┘
#> 
#> Column names: Type, Price
```
