# Convert a column to header rows

Convert a column to header rows

## Usage

``` r
column_to_header(
  ht,
  col,
  ...,
  glue = "{value}",
  start_col = 1,
  ignore_headers = TRUE,
  set_headers = TRUE
)
```

## Arguments

- ht:

  A huxtable.

- col:

  A column specifier for a single column.

- ...:

  Properties to set on new rows

- glue:

  Glue string. `"{value}"` will be replaced by the column value.

- start_col:

  Integer. New header text will start at this column.

- ignore_headers:

  Logical. Ignore existing headers?

- set_headers:

  Logical. Set new rows as headers?

## Examples

``` r
column_to_header(jams, "Type")
#>                                           Price  
#>                                  Strawberry      
#>                                            1.90  
#>                                  Raspberry       
#>                                            2.10  
#>                                  Plum            
#>                                            1.80  
#> 
#> Column names: Price
column_to_header(jams, "Type", text_color = "red")
#>                                           Price  
#>                                  Strawberry      
#>                                            1.90  
#>                                  Raspberry       
#>                                            2.10  
#>                                  Plum            
#>                                            1.80  
#> 
#> Column names: Price
column_to_header(jams, "Price",
  number_format = 2,
  italic = TRUE,
  glue = "Price: {value}"
)
#>                                  Type            
#>                                  Price: 1.90     
#>                                  Strawberry      
#>                                  Price: 2.10     
#>                                  Raspberry       
#>                                  Price: 1.80     
#>                                  Plum            
#> 
#> Column names: Type

iris_hux <- as_hux(iris[c(1:4, 51:54, 101:104), ])
column_to_header(iris_hux, "Species",
  markdown = TRUE,
  glue = "Species: **{value}**"
)
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
