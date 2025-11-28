# Insert a row or column

These convenience functions wrap `cbind` or `rbind` for huxtables, to
insert a single row or column.

## Usage

``` r
insert_column(
  ht,
  ...,
  after = 0,
  fill = NULL,
  rowspan = 1,
  copy_cell_props = TRUE
)

insert_row(
  ht,
  ...,
  after = 0,
  fill = NULL,
  colspan = 1,
  copy_cell_props = TRUE
)
```

## Arguments

- ht:

  A huxtable.

- ...:

  Cell contents.

- after:

  Insert the row/column after this position. 0 (the default) inserts as
  the first row/column.

- fill:

  String. If `...` contains fewer elements than there are columns/rows
  to fill, the remaining cells will be filled with this.

- rowspan, colspan:

  Scalar integer. Sets the rowspan or colspan of the *first* cell only.
  The default `NULL` throws an error if there are too few elements.

- copy_cell_props:

  Copy cell properties from the previous row or column (if after \> 0).
  See [`cbind.huxtable()`](cbind.huxtable.md).

## Value

The modified huxtable

## Details

In `insert_column` only, you can use a column name for `after`.

Even if `colspan` or `rowspan` are greater than 1, you must still
provide values for the hidden cells. Use `fill = ""` for this.

## See also

[`add_rows()`](add_rows.md) and [`add_columns()`](add_rows.md), which
insert multiple rows/columns at once.

## Examples

``` r
insert_row(jams,
  c("Gooseberry", 2.15),
  after = 1
)
#>                               Type           Price  
#>                               Gooseberry      2.15  
#>                               Strawberry      1.90  
#>                               Raspberry       2.10  
#>                               Plum            1.80  
#> 
#> Column names: Type, Price

insert_column(jams,
  c("Sugar", "50%", "60%", "40%"),
  after = "Price"
)
#>                           Type         Price    Sugar  
#>                           Strawberry    1.90   50.00%  
#>                           Raspberry     2.10   60.00%  
#>                           Plum          1.80   40.00%  
#> 
#> Column names: Type, Price,

insert_column(jams,
  "Sugar",
  after = "Price",
  fill = "50%"
)
#>                           Type         Price    Sugar  
#>                           Strawberry    1.90   50.00%  
#>                           Raspberry     2.10   50.00%  
#>                           Plum          1.80   50.00%  
#> 
#> Column names: Type, Price,

# don't forget to use `fill`:
insert_row(jams,
  "Jams and prices",
  fill = "",
  colspan = 2
)
#>                               Jams and prices       
#>                               Type           Price  
#>                               Strawberry      1.90  
#>                               Raspberry       2.10  
#>                               Plum            1.80  
#> 
#> Column names: Type, Price
```
