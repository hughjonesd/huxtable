# Map cell contents to cell properties using a function or scale

This creates a simple wrapper around a function for use in `map_xxx`.
Useful functions include scales and palettes from the `scales` package.

## Usage

``` r
by_function(inner_fn, ignore_na = TRUE)
```

## Arguments

- inner_fn:

  A one-argument function which maps cell values to property values.

- ignore_na:

  If `TRUE`, `NA` values in the result will be left unchanged from their
  previous values. Otherwise, `NA` normally resets to the default.

## Value

A function for use in `map_***` functions.

## Details

The argument of `inner_fn` will be `as.matrix(ht[row, col])`. Be aware
how matrix conversion affects the `mode` of cell data.

## See also

[mapping-functions](mapping-functions.md)

Other mapping functions: [`by_cases()`](by_cases.md),
[`by_colorspace()`](by_colorspace.md),
[`by_quantiles()`](by_quantiles.md), [`by_ranges()`](by_ranges.md),
[`by_regex()`](by_regex.md), [`by_rows()`](by_rows.md),
[`by_values()`](by_values.md)

## Examples

``` r
ht <- as_hux(matrix(runif(20), 5, 4))

map_background_color(
  ht,
  by_function(grey)
)
#>                         0.0201   0.528   0.92     0.997  
#>                         0.377    0.601   0.401    0.149  
#>                         0.56     0.261   0.213    0.519  
#>                         0.857    0.29    0.672    0.846  
#>                         0.385    0.48    0.0586   0.718  
#> 
#> Column names: V1, V2, V3, V4

if (requireNamespace("scales")) {
  map_text_color(ht, by_function(
    scales::seq_gradient_pal()
  ))
}
#>                         0.0201   0.528   0.92     0.997  
#>                         0.377    0.601   0.401    0.149  
#>                         0.56     0.261   0.213    0.519  
#>                         0.857    0.29    0.672    0.846  
#>                         0.385    0.48    0.0586   0.718  
#> 
#> Column names: V1, V2, V3, V4
```
