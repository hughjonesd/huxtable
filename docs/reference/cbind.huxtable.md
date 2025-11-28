# Combine rows or columns

These methods are called when one argument to `cbind`/`rbind` is a
huxtable. As well as combining cell contents, they copy table, row,
column and/or cell properties into the returned result.

## Usage

``` r
# S3 method for class 'huxtable'
cbind(..., deparse.level = 1, copy_cell_props = TRUE)

# S3 method for class 'huxtable'
rbind(..., deparse.level = 1, copy_cell_props = TRUE)
```

## Arguments

- ...:

  Vectors, matrices, or huxtables.

- deparse.level:

  Unused.

- copy_cell_props:

  Cell properties to copy from neighbours (see below).

## Value

A huxtable.

## Details

Table properties will be taken from the first argument which is a
huxtable. So will row properties (for cbind) and column properties (for
rbind).

If some of the inputs are not huxtables, and `copy_cell_props` is`TRUE`,
then cell properties will be copied to non-huxtables. Objects on the
left or above get priority over those on the right or below.

If `copy_cell_props` is `FALSE`, cells from non-huxtable objects will
get the default properties.

You cannot bind huxtables with data frames, since the R method dispatch
will always call the data frame method instead of the huxtable-specific
code. For a solution, see [`add_columns()`](add_rows.md).

## Examples

``` r
sugar <- c("Sugar", "40%", "35%", "50%")
jams <- set_bold(jams, 1, everywhere)
cbind(jams, sugar)
#>                           Type         Price    Sugar  
#>                           Strawberry    1.90   40.00%  
#>                           Raspberry     2.10   35.00%  
#>                           Plum          1.80   50.00%  
#> 
#> Column names: Type, Price,
cbind(jams, sugar,
  copy_cell_props = FALSE
)
#>                            Type         Price   Sugar  
#>                            Strawberry    1.90   40%    
#>                            Raspberry     2.10   35%    
#>                            Plum          1.80   50%    
#> 
#> Column names: Type, Price,

jams <- set_text_color(
  jams,
  everywhere, 1, "red"
)
rbind(jams, c("Damson", 2.30))
#>                               Type           Price  
#>                               Strawberry      1.90  
#>                               Raspberry       2.10  
#>                               Plum            1.80  
#>                               Damson          2.30  
#> 
#> Column names: Type, Price
rbind(jams, c("Damson", 2.30),
  copy_cell_props = FALSE
)
#>                               Type           Price  
#>                               Strawberry      1.90  
#>                               Raspberry       2.10  
#>                               Plum            1.80  
#>                               Damson       2.3      
#> 
#> Column names: Type, Price
```
