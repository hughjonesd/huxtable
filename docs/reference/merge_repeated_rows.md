# Merge repeated rows into multirow cells

`merge_repeated_rows()` looks within each column for contiguous groups
of identical cells. These are merged by setting [`rowspan()`](spans.md).
Doing this helps remove redundant information from the table.

## Usage

``` r
merge_repeated_rows(ht, row, col)
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

If `row` contains gaps, results may be unexpected (and a warning is
given).

## Cell content

In merged cell ranges, only the top left cell's content is displayed. In
addition, when you merge cells (either by setting
[`colspan()`](spans.md) or [`rowspan()`](spans.md), or using
[`merge_cells()`](merge_cells.md) and friends) the content of the top
left cell is copied to other cells. This prevents unexpected changes to
content if you reorder or subset rows and columns.

## See also

Other cell merging: [`merge_across()`](merge_across.md),
[`merge_cells()`](merge_cells.md)

## Examples

``` r
ht <- as_hux(jams[c(1, 2, 2, 3, 3, 4), ])
ht <- add_columns(ht, c("Sugar", "30%", "40%", "30%", "40%", "30%"),
  after = 1
)
ht
#>                            Type         Sugar   Price  
#>                            Strawberry   30%      1.90  
#>                            Strawberry   40%      1.90  
#>                            Raspberry    30%      2.10  
#>                            Raspberry    40%      2.10  
#>                            Plum         30%      1.80  
#> 
#> Column names: Type, , Price
merge_repeated_rows(ht)
#>                            Type         Sugar   Price  
#>                            Strawberry   30%      1.90  
#>                                         40%            
#>                            Raspberry    30%      2.10  
#>                                         40%            
#>                            Plum         30%      1.80  
#> 
#> Column names: Type, , Price
merge_repeated_rows(ht, everywhere, "Type")
#>                            Type         Sugar   Price  
#>                            Strawberry   30%      1.90  
#>                                         40%      1.90  
#>                            Raspberry    30%      2.10  
#>                                         40%      2.10  
#>                            Plum         30%      1.80  
#> 
#> Column names: Type, , Price
```
