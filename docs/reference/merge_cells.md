# Merge a range of cells

`merge_cells()` merges a rectangle of cells into a single displayed
cell, by setting [`colspan()`](spans.md) and [`rowspan()`](spans.md).

## Usage

``` r
merge_cells(ht, row, col)
```

## Arguments

- ht:

  A huxtable.

- row:

  A row specifier. See [rowspecs](rowspecs.md) for details.

- col:

  An optional column specifier.

## Value

The `ht` object.

## Details

`merge_cells(ht, c(min_row, max_row), c(min_col, max_col))` is
equivalent to

      colspan(ht)[min_row, min_col] <- max_col - min_col + 1
      rowspan(ht)[min_row, min_col] <- max_row - min_row + 1

## Cell content

In merged cell ranges, only the top left cell's content is displayed. In
addition, when you merge cells (either by setting
[`colspan()`](spans.md) or [`rowspan()`](spans.md), or using
`merge_cells()` and friends) the content of the top left cell is copied
to other cells. This prevents unexpected changes to content if you
reorder or subset rows and columns.

## See also

Other cell merging: [`merge_across()`](merge_across.md),
[`merge_repeated_rows()`](merge_repeated_rows.md)

## Examples

``` r
ht <- hux(a = 1:3, b = 1:3)
ht <- set_all_borders(ht, 1)
merge_cells(ht, 2:3, 1:2)
#>                              ┌─────────┬─────────┐
#>                              │       a │       b │
#>                              ├─────────┴─────────┤
#>                              │              1    │
#>                              │                   │
#>                              ├─────────┬─────────┤
#>                              │       3 │       3 │
#>                              └─────────┴─────────┘
#> 
#> Column names: a, b
```
