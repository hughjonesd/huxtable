# Split a huxtable into multiple huxtables

These functions split a huxtable horizontally or vertically, and return
the new sub-tables in a list.

## Usage

``` r
split_across(ht, after, height, headers = TRUE)

split_down(ht, after, width, headers = TRUE)
```

## Arguments

- ht:

  A huxtable.

- after:

  Rows/columns after which to split. See [rowspecs](rowspecs.md) for
  details. Note that
  [tidyselect](https://tidyselect.r-lib.org/reference/language.html)
  semantics are allowed in `split_down()` but not `split_across()`.

- height, width:

  Maximum height/width for the result.

- headers:

  Logical. Take account of header rows/columns?

## Value

A list of huxtables.

## Details

Only one of `after` and `width` or `height` must be given. If `width` or
`height` is given, the huxtable will be split by
[`col_width()`](col_width.md) or [`row_height()`](row_height.md), which
must be numeric with no `NA` values.

If `headers` is `TRUE`, all previous headers will be added to each new
table.

## See also

[restack-across-down](restack-across-down.md)

## Examples

``` r
ht <- as_hux(matrix(LETTERS[1:16], 4, 4))
ht <- set_all_borders(ht)
split_across(ht, after = 2)
#> [[1]]
#>                          ┌──────┬──────┬──────┬──────┐
#>                          │ A    │ E    │ I    │ M    │
#>                          ├──────┼──────┼──────┼──────┤
#>                          │ B    │ F    │ J    │ N    │
#>                          └──────┴──────┴──────┴──────┘
#> 
#> Column names: V1, V2, V3, V4
#> 
#> [[2]]
#>                          ┌──────┬──────┬──────┬──────┐
#>                          │ C    │ G    │ K    │ O    │
#>                          ├──────┼──────┼──────┼──────┤
#>                          │ D    │ H    │ L    │ P    │
#>                          └──────┴──────┴──────┴──────┘
#> 
#> Column names: V1, V2, V3, V4
#> 
split_down(ht, after = c(1, 3))
#> [[1]]
#>                                ┌────────────────┐
#>                                │ A              │
#>                                ├────────────────┤
#>                                │ B              │
#>                                ├────────────────┤
#>                                │ C              │
#>                                ├────────────────┤
#>                                │ D              │
#>                                └────────────────┘
#> 
#> Column names: V1
#> 
#> [[2]]
#>                              ┌─────────┬─────────┐
#>                              │ E       │ I       │
#>                              ├─────────┼─────────┤
#>                              │ F       │ J       │
#>                              ├─────────┼─────────┤
#>                              │ G       │ K       │
#>                              ├─────────┼─────────┤
#>                              │ H       │ L       │
#>                              └─────────┴─────────┘
#> 
#> Column names: V2, V3
#> 
#> [[3]]
#>                                ┌────────────────┐
#>                                │ M              │
#>                                ├────────────────┤
#>                                │ N              │
#>                                ├────────────────┤
#>                                │ O              │
#>                                ├────────────────┤
#>                                │ P              │
#>                                └────────────────┘
#> 
#> Column names: V4
#> 

col_width(ht) <- c(0.15, 0.1, 0.25, 0.3)
split_down(ht, width = 0.3)
#> [[1]]
#>                              ┌───────────┬────────┐
#>                              │ A         │ E      │
#>                              ├───────────┼────────┤
#>                              │ B         │ F      │
#>                              ├───────────┼────────┤
#>                              │ C         │ G      │
#>                              ├───────────┼────────┤
#>                              │ D         │ H      │
#>                              └───────────┴────────┘
#> 
#> Column names: V1, V2
#> 
#> [[2]]
#>                                ┌────────────────┐
#>                                │ I              │
#>                                ├────────────────┤
#>                                │ J              │
#>                                ├────────────────┤
#>                                │ K              │
#>                                ├────────────────┤
#>                                │ L              │
#>                                └────────────────┘
#> 
#> Column names: V3
#> 
#> [[3]]
#>                                ┌────────────────┐
#>                                │ M              │
#>                                ├────────────────┤
#>                                │ N              │
#>                                ├────────────────┤
#>                                │ O              │
#>                                ├────────────────┤
#>                                │ P              │
#>                                └────────────────┘
#> 
#> Column names: V4
#> 

# split by column name:
split_down(jams, "Type")
#> [[1]]
#>                                  Type            
#>                                  Strawberry      
#>                                  Raspberry       
#>                                  Plum            
#> 
#> Column names: Type
#> 
#> [[2]]
#>                                           Price  
#>                                            1.90  
#>                                            2.10  
#>                                            1.80  
#> 
#> Column names: Price
#> 

# headers are repeated:
split_across(jams, 3)
#> [[1]]
#>                               Type           Price  
#>                               Strawberry      1.90  
#>                               Raspberry       2.10  
#> 
#> Column names: Type, Price
#> 
#> [[2]]
#>                                Type        Price  
#>                                Plum         1.80  
#> 
#> Column names: Type, Price
#> 
```
