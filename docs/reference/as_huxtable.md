# Convert objects to huxtables

`as_huxtable` or `as_hux` converts an object to a huxtable. Conversion
methods exist for data frames and tibbles, tables, ftables, matrices and
(most) vectors.

## Usage

``` r
as_huxtable(x, ...)

as_hux(x, ...)

# Default S3 method
as_huxtable(
  x,
  add_colnames = getOption("huxtable.add_colnames", TRUE),
  add_rownames = FALSE,
  autoformat = getOption("huxtable.autoformat", TRUE),
  ...
)

# S3 method for class 'grouped_df'
as_huxtable(x, ..., groups_to_headers = FALSE)

is_huxtable(x)

is_hux(x)
```

## Arguments

- x:

  Object to convert.

- ...:

  Arguments passed on to [`huxtable()`](huxtable.md).

- add_colnames:

  If `TRUE`, add a first row of column names to the huxtable.

- add_rownames:

  If `TRUE` or a character string, add a first column of row names to
  the huxtable. The string gives the name for the new column (or
  `"rownames"` for `TRUE`).

- autoformat:

  If `TRUE`, automatically format columns by type. See below.

- groups_to_headers:

  Logical. Convert groups to header rows?

## Value

An object of class "huxtable".

## Details

`is_hux[table]` tests if an object is a huxtable.

For `table` objects, `add_colnames` and `add_rownames` are `TRUE` by
default. For `matrix` objects, they are `FALSE`. Other classes use
`options("huxtable.add_colnames")`, which is `TRUE` by default;
`add_rownames` is `FALSE`.

For
[`dplyr::grouped_df()`](https://dplyr.tidyverse.org/reference/grouped_df.html)
objects, groups will be converted to header rows if `groups_to_headers`
is `TRUE`.

## Examples

``` r
dfr <- data.frame(
  a = 1:5,
  b = letters[1:5],
  stringsAsFactors = FALSE
)
as_huxtable(dfr)
#>                                      a   b        
#>                                      1   a        
#>                                      2   b        
#>                                      3   c        
#>                                      4   d        
#>                                      5   e        
#> 
#> Column names: a, b
mx <- matrix(letters[1:12], 4, 3)
as_huxtable(mx, add_colnames = FALSE)
#>                              a       e       i      
#>                              b       f       j      
#>                              c       g       k      
#>                              d       h       l      
#> 
#> Column names: V1, V2, V3
library(stats)
tbl <- table(
  Wool    = warpbreaks$wool,
  Tension = warpbreaks$tension
)
as_huxtable(tbl) # adds row and column names by default
#>                                      L      M   H     
#>                               A      9      9   9     
#>                               B      9      9   9     
#> 
#> Column names: rownames, L, M, H

# adding rownames:
as_hux(mtcars[1:3, ],
  add_colnames = TRUE,
  add_rownames = "Car"
)
#>                      Car    mpg   cyl   disp    hp   drat     wt  
#>                    Mazda   21       6    160   110   3.9    2.62  
#>                      RX4                                          
#>                    Mazda   21       6    160   110   3.9    2.88  
#>                  RX4 Wag                                          
#>                   Datsun   22.8     4    108    93   3.85   2.32  
#>                      710                                          
#> 
#> Column names: Car, mpg, cyl, disp, hp, drat, wt, qsec, vs, am, gear, carb
#> 
#> 7/12 columns shown.

if (requireNamespace("dplyr")) {
  iris_grp <- dplyr::group_by(iris[c(1:4, 51:54, 101:104), ], Species)
  as_hux(iris_grp, groups_to_headers = TRUE)
}
#>             Sepal.Length   Sepal.Width   Petal.Length   Petal.Width  
#>             Species: setosa                                          
#>                      5.1           3.5            1.4           0.2  
#>                      4.9           3              1.4           0.2  
#>                      4.7           3.2            1.3           0.2  
#>                      4.6           3.1            1.5           0.2  
#>             Species: versicolor                                      
#>                      7             3.2            4.7           1.4  
#>                      6.4           3.2            4.5           1.5  
#>                      6.9           3.1            4.9           1.5  
#>                      5.5           2.3            4             1.3  
#>             Species: virginica                                       
#>                      6.3           3.3            6             2.5  
#>                      5.8           2.7            5.1           1.9  
#>                      7.1           3              5.9           2.1  
#>                      6.3           2.9            5.6           1.8  
#> 
#> Column names: Sepal.Length, Sepal.Width, Petal.Length, Petal.Width
```
