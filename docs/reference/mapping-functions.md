# How to set cell properties variably by cell contents

This help page explains how to set properties differently for cells,
depending on their contents.

For example, in a table of p-values, you could bold cells where p \<
0.05:

      map_bold(pval_hux, by_ranges(0.05, c(TRUE, FALSE)))

Or you can use red text for a particular value:

      hxtbl %>% map_text_color(by_values("Warning" = "red"))

There is a `map_...` function for each huxtable cell property. The
syntax is:

      map_property(ht, row, col, fn)

where `property` is the property name.

`row` and `col` specify ranges of rows and columns. See
[rowspecs](rowspecs.md) for details. To set properties for the whole
table, omit `row` and `col`:

      map_property(ht, fn)

The `fn` argument is a *mapping function* which maps cell contents to
property values.

- To set property values in "stripes" by rows or by columns, use
  [`by_rows()`](by_rows.md) and [`by_cols()`](by_rows.md).

- To set property values for cells with specific contents, use
  [`by_values()`](by_values.md).

- To set property values for cells within a numeric range, use
  [`by_ranges()`](by_ranges.md).

- To set property values for cells by quantiles, use
  [`by_quantiles()`](by_quantiles.md) or
  [`by_equal_groups()`](by_quantiles.md).

- To set property values for cells that match a string or regular
  expression, use [`by_regex()`](by_regex.md).

- To map numeric values to a colorspace, use
  [`by_colorspace()`](by_colorspace.md).

- For a more general solution, use [`by_function()`](by_function.md) or
  [`by_cases()`](by_cases.md).

## Caveat

Most functions convert the huxtable to a matrix using
[`as.matrix()`](https://rdrr.io/r/base/matrix.html). This can have
unexpected results if you mix character and numeric data. See the
example.

## Technical details

`fn` takes four arguments: the entire original huxtable `ht`, a numeric
vector of `rows`, a numeric vector of `cols`, and the `current` property
values for `ht[rows, cols]`, as a matrix. It should return the new
property values for `ht[rows, cols]`, as a matrix.

## Examples

``` r
ht <- hux(Condition = c("OK", "Warning", "Error"))
ht <- map_text_color(ht, by_values(
  OK      = "green",
  Warning = "orange",
  Error   = "red"
))
ht
#>                                  Condition       
#>                                  OK              
#>                                  Warning         
#>                                  Error           
#> 
#> Column names: Condition

# Leaving NA values alone:
map_text_color(ht, by_values(
  "OK" = "blue", NA, ignore_na = TRUE
))
#>                                  Condition       
#>                                  OK              
#>                                  Warning         
#>                                  Error           
#> 
#> Column names: Condition

# Resetting values:
map_text_color(ht, by_values(
  "OK" = "blue", NA, ignore_na = FALSE
))
#>                                  Condition       
#>                                  OK              
#>                                  Warning         
#>                                  Error           
#> 
#> Column names: Condition

ht <- as_hux(matrix(rnorm(15), 5, 3))
map_background_color(ht, by_ranges(
  c(-1, 1),
  c("blue", "yellow", "red")
))
#>                              0.694   -1.64    -0.284  
#>                             -0.709   -0.779    0.498  
#>                             -0.209    1.54     1.61   
#>                              0.352    0.7      1.86   
#>                              1.74     0.118    0.386  
#> 
#> Column names: V1, V2, V3
map_background_color(
  ht,
  by_equal_groups(2, c("red", "green"))
)
#>                              0.694   -1.64    -0.284  
#>                             -0.709   -0.779    0.498  
#>                             -0.209    1.54     1.61   
#>                              0.352    0.7      1.86   
#>                              1.74     0.118    0.386  
#> 
#> Column names: V1, V2, V3

ht <- hux(
  Coef = c(3.5, 2.4, 1.3),
  Pval = c(0.04, 0.01, 0.07),
  add_colnames = TRUE
)
map_bold(
  ht, everywhere, "Pval",
  by_ranges(0.05, c(TRUE, FALSE))
)
#>                                   Coef      Pval  
#>                                    3.5      0.04  
#>                                    2.4      0.01  
#>                                    1.3      0.07  
#> 
#> Column names: Coef, Pval

# Problems with as.matrix:

ht <- hux(c(-1, 1, 2), letters[1:3])
as.matrix(ht) # look at the spaces...
#>              
#> [1,] "-1" "a"
#> [2,] " 1" "b"
#> [3,] " 2" "c"
as.matrix(ht) > 0 # uh oh
#>                
#> [1,] FALSE TRUE
#> [2,] FALSE TRUE
#> [3,] FALSE TRUE
map_text_color(
  ht,
  by_cases(. < 0 ~ "red", TRUE ~ "blue")
)
#>                                     -1   a        
#>                                      1   b        
#>                                      2   c        

# To avoid this, only look at the truly numeric columns:
map_text_color(ht,
  row = 1:3, col = 1,
  by_cases(. < 0 ~ "red", TRUE ~ "blue")
)
#>                                     -1   a        
#>                                      1   b        
#>                                      2   c        
```
