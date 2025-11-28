# Merge cells across rows or down columns

`merge_across()` creates multicolumn cells within each row.
`merge_down()` creates multirow cells within each column.

## Usage

``` r
merge_across(ht, row, col)

merge_down(ht, row, col)
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

## Cell content

In merged cell ranges, only the top left cell's content is displayed. In
addition, when you merge cells (either by setting
[`colspan()`](spans.md) or [`rowspan()`](spans.md), or using
[`merge_cells()`](merge_cells.md) and friends) the content of the top
left cell is copied to other cells. This prevents unexpected changes to
content if you reorder or subset rows and columns.

## See also

Other cell merging: [`merge_cells()`](merge_cells.md),
[`merge_repeated_rows()`](merge_repeated_rows.md)

## Examples

``` r
ht <- as_hux(matrix(1:12, 4, 3, byrow = TRUE))
ht <- set_all_borders(ht, 1)
merge_across(ht, 2:4, 2:3)
#>                            ┌───────┬───────┬───────┐
#>                            │     1 │     2 │     3 │
#>                            ├───────┼───────┴───────┤
#>                            │     4 │          5    │
#>                            ├───────┼───────────────┤
#>                            │     7 │          8    │
#>                            ├───────┼───────────────┤
#>                            │    10 │         11    │
#>                            └───────┴───────────────┘
#> 
#> Column names: V1, V2, V3
merge_down(ht, 2:4, 2:3)
#>                            ┌───────┬───────┬───────┐
#>                            │     1 │     2 │     3 │
#>                            ├───────┼───────┼───────┤
#>                            │     4 │     5 │     6 │
#>                            ├───────┤       │       │
#>                            │     7 │       │       │
#>                            ├───────┤       │       │
#>                            │    10 │       │       │
#>                            └───────┴───────┴───────┘
#> 
#> Column names: V1, V2, V3
```
