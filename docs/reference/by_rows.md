# Set cell properties by row or column

`by_rows` and `by_cols` set properties in horizontal or vertical
"stripes".

## Usage

``` r
by_rows(..., from = 1, ignore_na = TRUE)

by_cols(..., from = 1, ignore_na = TRUE)
```

## Arguments

- ...:

  One or more cell property values.

- from:

  Numeric. Row or column to start at.

- ignore_na:

  If `TRUE`, `NA` values in the result will be left unchanged from their
  previous values. Otherwise, `NA` normally resets to the default.

## Value

A function for use in `map_***` functions.

## See also

[mapping-functions](mapping-functions.md)

Other mapping functions: [`by_cases()`](by_cases.md),
[`by_colorspace()`](by_colorspace.md),
[`by_function()`](by_function.md), [`by_quantiles()`](by_quantiles.md),
[`by_ranges()`](by_ranges.md), [`by_regex()`](by_regex.md),
[`by_values()`](by_values.md)

## Examples

``` r
ht <- as_hux(matrix(rnorm(25), 5, 5))
map_background_color(
  ht,
  by_rows("green", "grey")
)
#>                   0.00048   -0.679    0.194   -1.78    -0.432  
#>                   0.755      0.738   -0.691   -0.716   -0.668  
#>                   0.342     -0.861    1.34     0.911    1.39   
#>                   0.168      0.421    2.74    -0.772    0.912  
#>                   1.4        1.45    -0.944   -0.782    0.205  
#> 
#> Column names: V1, V2, V3, V4, V5
map_background_color(
  ht,
  by_cols("green", "grey")
)
#>                   0.00048   -0.679    0.194   -1.78    -0.432  
#>                   0.755      0.738   -0.691   -0.716   -0.668  
#>                   0.342     -0.861    1.34     0.911    1.39   
#>                   0.168      0.421    2.74    -0.772    0.912  
#>                   1.4        1.45    -0.944   -0.782    0.205  
#> 
#> Column names: V1, V2, V3, V4, V5
```
