# Set left, right, top and bottom properties

These functions set left, right, top and/or bottom properties
simultaneously for the specified cells.

## Usage

``` r
set_all_borders(ht, row, col, value = 0.4)

map_all_borders(ht, row, col, fn)

set_all_border_colors(ht, row, col, value)

map_all_border_colors(ht, row, col, fn)

set_all_border_styles(ht, row, col, value)

map_all_border_styles(ht, row, col, fn)

set_all_padding(ht, row, col, value)

map_all_padding(ht, row, col, fn)

set_tb_padding(ht, row, col, value)

map_tb_padding(ht, row, col, fn)

set_lr_padding(ht, row, col, value)

map_lr_padding(ht, row, col, fn)

set_tb_borders(ht, row, col, value)

map_tb_borders(ht, row, col, fn)

set_lr_borders(ht, row, col, value)

map_lr_borders(ht, row, col, fn)

set_tb_border_colors(ht, row, col, value)

map_tb_border_colors(ht, row, col, fn)

set_lr_border_colors(ht, row, col, value)

map_lr_border_colors(ht, row, col, fn)

set_tb_border_styles(ht, row, col, value)

map_tb_border_styles(ht, row, col, fn)

set_lr_border_styles(ht, row, col, value)

map_lr_border_styles(ht, row, col, fn)
```

## Arguments

- ht:

  A huxtable.

- row:

  A row specifier. See [rowspecs](rowspecs.md) for details.

- col:

  An optional column specifier.

- value:

  Value(s) to set. Set to `NA` to reset to the default.

- fn:

  A mapping function. See [mapping-functions](mapping-functions.md) for
  details.

## Value

The modified huxtable.

## Details

- `set_all_*` functions set top, bottom, left and right properties.

- `set_tb_*` functions set top and bottom properties.

- `set_lr_*` functions set left and right properties.

## See also

[borders](borders.md), [border-colors](border-colors.md),
[border-styles](border-styles.md), [padding](padding.md).

## Examples

``` r
ht <- as_hux(jams)
ht <- set_all_borders(ht)
ht
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
ht <- set_all_border_colors(ht, "red")
ht
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
ht <- set_all_border_styles(ht, "double")
ht <- set_all_padding(ht, 1:3, 1:2, "20px")
ht <- set_tb_padding(ht, 10)
ht <- set_tb_borders(ht)
set_tb_border_colors(ht, "red")
#>                             ╔════════════╦═════════╗
#>                             ║ Type       ║   Price ║
#>                             ╠════════════╬═════════╣
#>                             ║ Strawberry ║    1.90 ║
#>                             ╠════════════╬═════════╣
#>                             ║ Raspberry  ║    2.10 ║
#>                             ╠════════════╬═════════╣
#>                             ║ Plum       ║    1.80 ║
#>                             ╚════════════╩═════════╝
#> 
#> Column names: Type, Price
set_tb_border_styles(ht, "double")
#>                             ╔════════════╦═════════╗
#>                             ║ Type       ║   Price ║
#>                             ╠════════════╬═════════╣
#>                             ║ Strawberry ║    1.90 ║
#>                             ╠════════════╬═════════╣
#>                             ║ Raspberry  ║    2.10 ║
#>                             ╠════════════╬═════════╣
#>                             ║ Plum       ║    1.80 ║
#>                             ╚════════════╩═════════╝
#> 
#> Column names: Type, Price
```
