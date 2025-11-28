# Subset a huxtable

Subset a huxtable

## Usage

``` r
# S3 method for class 'huxtable'
x[i, j, drop = FALSE]

# S3 method for class 'huxtable'
x[i, j] <- value

# S3 method for class 'huxtable'
x$name <- value

# S3 method for class 'huxtable'
x[[i, j]] <- value
```

## Arguments

- x:

  A huxtable.

- i:

  Rows to select.

- j, name:

  Columns to select.

- drop:

  Only included for compatibility with `[.data.frame`. Do not use.

- value:

  A matrix, data frame, huxtable or similar object.

## Value

`[` returns a huxtable. `$` and `[[` return data from the underlying
data frame.

## Replacing existing rows and columns

For the replacement function `[<-`, if `value` is a huxtable, then its
properties will be copied into `x`. Replacement functions `$<-` and
`[[<-` replace existing data without affecting any properties.

## Adding new rows and columns

If new columns or rows are created, then properties will be copied from
the last column or row of `x`, or from `value` if `value` is a huxtable.

These methods are stricter than their data frame equivalents in some
places. You can't add new rows or column at a numeric location without
specifying all intervening rows/columns. New values must have the
appropriate dimensions (vectors will be interpreted appropriately).

## Examples

``` r
jams[1:3, ]
#>                               Type           Price  
#>                               Strawberry      1.90  
#>                               Raspberry       2.10  
#> 
#> Column names: Type, Price
class(jams[1:3, ])
#> [1] "huxtable"   "data.frame"
jams[, 1]
#>                                  Type            
#>                                  Strawberry      
#>                                  Raspberry       
#>                                  Plum            
#> 
#> Column names: Type
jams$Type
#> [1] "Type"       "Strawberry" "Raspberry"  "Plum"      
prices <- huxtable(c("Price", 1.70, 2.00, 2.20))
number_format(prices) <- 2
bold(prices) <- TRUE
jams[, 2] <- prices
jams
#>                               Type         Price    
#>                               Strawberry   1.70     
#>                               Raspberry    2.00     
#>                               Plum         2.20     
#> 
#> Column names: Type, Price

data(jams)
jams$price <- c("Price", 1.70, 2.00, 2.20)
jams
#>                            Type         Price   Price  
#>                            Strawberry   1.70    1.70   
#>                            Raspberry    2.00    2.00   
#>                            Plum         2.20    2.20   
#> 
#> Column names: Type, Price, price
```
