# Extend cells over multiple rows and/or columns

A cell with rowspan of 2 covers the cell directly below it. A cell with
colspan of 2 covers the cell directly to its right. A cell with rowspan
of 2 and colspan of 2 covers a 2 x 2 square, hiding three other cells.

## Usage

``` r
rowspan(ht)

rowspan(ht) <- value

set_rowspan(ht, row, col, value)

map_rowspan(ht, row, col, fn)

colspan(ht)

colspan(ht) <- value

set_colspan(ht, row, col, value)

map_colspan(ht, row, col, fn)
```

## Arguments

- ht:

  A huxtable.

- value:

  An integer vector or matrix.

- row:

  A row specifier. See [rowspecs](rowspecs.md) for details.

- col:

  An optional column specifier.

- fn:

  A mapping function. See [mapping-functions](mapping-functions.md) for
  details.

## Cell content

In merged cell ranges, only the top left cell's content is displayed. In
addition, when you merge cells (either by setting `colspan()` or
`rowspan()`, or using [`merge_cells()`](merge_cells.md) and friends) the
content of the top left cell is copied to other cells. This prevents
unexpected changes to content if you reorder or subset rows and columns.

## See also

[`merge_cells()`](merge_cells.md), [`merge_across()`](merge_across.md)
and [`merge_down()`](merge_across.md) for a higher-level interface.

## Examples

``` r
letter_hux <- as_hux(matrix(LETTERS[1:9], 3, 3))
letter_hux <- set_all_borders(letter_hux)
letter_hux
#>                            ┌───────┬───────┬───────┐
#>                            │ A     │ D     │ G     │
#>                            ├───────┼───────┼───────┤
#>                            │ B     │ E     │ H     │
#>                            ├───────┼───────┼───────┤
#>                            │ C     │ F     │ I     │
#>                            └───────┴───────┴───────┘
#> 
#> Column names: V1, V2, V3
set_rowspan(letter_hux, 1, 1, 2)
#>                            ┌───────┬───────┬───────┐
#>                            │ A     │ D     │ G     │
#>                            │       ├───────┼───────┤
#>                            │       │ E     │ H     │
#>                            ├───────┼───────┼───────┤
#>                            │ C     │ F     │ I     │
#>                            └───────┴───────┴───────┘
#> 
#> Column names: V1, V2, V3
set_colspan(letter_hux, 1, 1, 2)
#>                            ┌───────────────┬───────┐
#>                            │ A             │ G     │
#>                            ├───────┬───────┼───────┤
#>                            │ B     │ E     │ H     │
#>                            ├───────┼───────┼───────┤
#>                            │ C     │ F     │ I     │
#>                            └───────┴───────┴───────┘
#> 
#> Column names: V1, V2, V3
```
