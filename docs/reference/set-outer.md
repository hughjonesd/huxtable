# Set borders and padding around a rectangle of cells

Set borders and padding around a rectangle of cells

## Usage

``` r
set_outer_borders(ht, row, col, value = 0.4)

set_outer_border_colors(ht, row, col, value)

set_outer_border_styles(ht, row, col, value)

set_outer_padding(ht, row, col, value)
```

## Arguments

- ht:

  A huxtable.

- row:

  A row specifier. See [rowspecs](rowspecs.md) for details.

- col:

  An optional column specifier.

- value:

  Border width, color, style or a [`brdr()`](brdr.md) object. See
  [borders](borders.md). For padding, padding width in points.

## Details

`set_outer_borders` sets borders round the top, bottom, left and right
of a group of cells. Behaviour is undefined unless `row` and `col`
specify contiguous sequences. `set_outer_border_colors` and
`set_outer_border_styles` set border colors and styles.
`set_outer_padding` sets padding, i.e. top padding on the top row of
cells, etc.

## Examples

``` r
ht2 <- huxtable(a = 1:3, b = 1:3)
set_outer_borders(ht2)
#>                              ┌───────────────────┐
#>                              │       a         b │
#>                              │       1         1 │
#>                              │       2         2 │
#>                              │       3         3 │
#>                              └───────────────────┘
#> 
#> Column names: a, b
set_outer_borders(ht2, 2:3, 1:2)
#>                                      a         b  
#>                              ┌───────────────────┐
#>                              │       1         1 │
#>                              │       2         2 │
#>                              └───────────────────┘
#>                                      3         3  
#> 
#> Column names: a, b
```
