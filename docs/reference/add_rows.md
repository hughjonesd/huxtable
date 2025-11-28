# Insert one huxtable into another

These functions combine two huxtables or similar objects and return the
result.

## Usage

``` r
add_rows(x, y, after = nrow(x), copy_cell_props = TRUE)

add_columns(x, y, after = ncol(x), copy_cell_props = TRUE)
```

## Arguments

- x, y:

  Huxtables or objects that can be converted by as_hux

- after:

  Row or column after which `y` is inserted. Can be 0. Can be a row or
  column name. The default adds `y` to the end of `x`.

- copy_cell_props:

  Logical. Passed to [`rbind.huxtable()`](cbind.huxtable.md) or
  [`cbind.huxtable()`](cbind.huxtable.md).

## Value

A huxtable.

## Details

Arguments in `...` can include `copy_cell_props`.

## See also

[`insert_row()`](insert_column.md) and
[`insert_column()`](insert_column.md), which insert multiple values into
a single row.

## Examples

``` r
ht <- hux("Gooseberry", 2.15)
add_rows(jams, ht)
#>                               Type           Price  
#>                               Strawberry      1.90  
#>                               Raspberry       2.10  
#>                               Plum            1.80  
#>                               Gooseberry      2.15  
#> 
#> Column names: Type, Price
add_rows(jams, ht, after = 1)
#>                               Type           Price  
#>                               Gooseberry      2.15  
#>                               Strawberry      1.90  
#>                               Raspberry       2.10  
#>                               Plum            1.80  
#> 
#> Column names: Type, Price

mx <- matrix(
  c(
    "Sugar", "50%", "60%", "40%",
    "Weight (g)", 300, 250, 300
  ),
  4, 2
)
add_columns(jams, mx)
#>                     Type         Price    Sugar   Weight (g)  
#>                     Strawberry    1.90   50.00%       300.00  
#>                     Raspberry     2.10   60.00%       250.00  
#>                     Plum          1.80   40.00%       300.00  
#> 
#> Column names: Type, Price, , .1
```
