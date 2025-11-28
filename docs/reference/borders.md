# Set borders

These functions set borders between cells.

## Usage

``` r
left_border(ht)

right_border(ht)

top_border(ht)

bottom_border(ht)

left_border(ht) <- value

right_border(ht) <- value

top_border(ht) <- value

bottom_border(ht) <- value

set_left_border(ht, row, col, value = 0.4)

set_right_border(ht, row, col, value = 0.4)

set_top_border(ht, row, col, value = 0.4)

set_bottom_border(ht, row, col, value = 0.4)

map_left_border(ht, row, col, fn)

map_right_border(ht, row, col, fn)

map_top_border(ht, row, col, fn)

map_bottom_border(ht, row, col, fn)
```

## Arguments

- ht:

  A huxtable.

- value:

  A numeric thickness or a [`brdr()`](brdr.md) object. Set to `NA` to
  reset to the default, which is `0`.

- row:

  A row specifier. See [rowspecs](rowspecs.md) for details.

- col:

  An optional column specifier.

- fn:

  A mapping function. See [mapping-functions](mapping-functions.md) for
  details.

## Details

Borders are always "collapsed": `right_border(ht)[, 1]` is the same as
`left_border(ht)[, 2]`, and setting one sets the other.

Setting `left_border(ht) <- number` sets the border thickness. You can
set multiple properties at once by using [`brdr()`](brdr.md).

Currently in LaTeX, all non-zero border widths on a given line must be
the same.

## Limitations

- In HTML, you will need to set a width of at least 3 to get a double
  border.

- Only "solid" and "double" styles are currently implemented in LaTeX,
  and all non-zero horizontal border widths on a given line must be the
  same.

## See also

[set-multiple](set-multiple.md)

Other border properties: [`left_border_color()`](border-colors.md),
[`left_border_style()`](border-styles.md)

## Examples

``` r
bottom_border(jams)[1, ] <- 0.4
jams
#>                               Type           Price  
#>                             ────────────────────────
#>                               Strawberry      1.90  
#>                               Raspberry       2.10  
#>                               Plum            1.80  
#> 
#> Column names: Type, Price

bottom_border(jams)[1, ] <- brdr(0.4, "solid", "blue")
jams
#>                               Type           Price  
#>                             ────────────────────────
#>                               Strawberry      1.90  
#>                               Raspberry       2.10  
#>                               Plum            1.80  
#> 
#> Column names: Type, Price

set_bottom_border(jams, brdr(0.4, "solid", "green"))
#>                               Type           Price  
#>                             ────────────────────────
#>                               Strawberry      1.90  
#>                             ────────────────────────
#>                               Raspberry       2.10  
#>                             ────────────────────────
#>                               Plum            1.80  
#>                             ────────────────────────
#> 
#> Column names: Type, Price
```
