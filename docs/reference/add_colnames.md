# Add column or row names

Add a first row of column names, or a first column of row names, to the
huxtable.

## Usage

``` r
add_colnames(ht, rowname = "", ...)

add_rownames(ht, colname = "rownames", preserve_rownames = TRUE, ...)
```

## Arguments

- ht:

  A huxtable.

- rowname:

  Optional row name for the new row of column names.

- ...:

  Arguments passed to methods.

- colname:

  Column name for the new column of row names.

- preserve_rownames:

  Preserve existing row names.

## Value

The modified object.

## Details

Note that `add_colnames` will change the mode of all columns to
character. Also note that it will move your rows down by one: what was
row 1 will now be row 2, and the column names will now be row 1.

`add_colnames` preserves column names. `add_rownames` only preserves
them if asked to.

## Examples

``` r
ht <- huxtable(
  First = rnorm(5),
  Second = rnorm(5),
  add_rownames = FALSE
)
add_rownames(ht)
#>                                       First   Second  
#>                            1       -1.4        1.15   
#>                            2        0.255     -1.82   
#>                            3       -2.44      -0.247  
#>                            4       -0.00557   -0.244  
#>                            5        0.622     -0.283  
#> 
#> Column names: rownames, First, Second
add_colnames(ht)
#> Warning: non-unique value when setting 'row.names': ‘’
#> Error in `.rowNamesDF<-`(x, value = value): duplicate 'row.names' are not allowed

# Out by 1:
add_rownames(add_colnames(ht))
#> Warning: non-unique value when setting 'row.names': ‘’
#> Error in `.rowNamesDF<-`(x, value = value): duplicate 'row.names' are not allowed

# Better:
add_colnames(add_rownames(ht))
#> Warning: non-unique value when setting 'row.names': ‘’
#> Error in `.rowNamesDF<-`(x, value = value): duplicate 'row.names' are not allowed

# Alternatively:
add_colnames(add_rownames(ht, ""))
#> Warning: non-unique value when setting 'row.names': ‘’
#> Error in `.rowNamesDF<-`(x, value = value): duplicate 'row.names' are not allowed
```
