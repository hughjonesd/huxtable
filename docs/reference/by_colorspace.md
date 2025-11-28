# Map numeric cell contents smoothly to colors

`by_colorspace()` can be used to set background, border or text colors,
visually differentiating high or low values.

## Usage

``` r
by_colorspace(
  ...,
  range = NULL,
  na_color = NA,
  ignore_na = TRUE,
  colwise = FALSE
)
```

## Arguments

- ...:

  Colors

- range:

  Numeric endpoints. If `NULL`, these are determined from the data.

- na_color:

  Color to return for `NA` values. Can be `NA` itself.

- ignore_na:

  If `TRUE`, `NA` values in the result will be left unchanged from their
  previous values. Otherwise, `NA` normally resets to the default.

- colwise:

  Logical. Calculate breaks separately within each column?

## Value

A function for use in `map_***` functions.

## Details

`by_colorspace` requires the "scales" package.

## See also

[mapping-functions](mapping-functions.md)

Other mapping functions: [`by_cases()`](by_cases.md),
[`by_function()`](by_function.md), [`by_quantiles()`](by_quantiles.md),
[`by_ranges()`](by_ranges.md), [`by_regex()`](by_regex.md),
[`by_rows()`](by_rows.md), [`by_values()`](by_values.md)

## Examples

``` r
if (!requireNamespace("scales")) {
  stop("Please install the \"scales\" package to run this example")
}
#> Loading required namespace: scales
ht <- as_hux(matrix(rnorm(25), 5, 5))
map_background_color(
  ht,
  by_colorspace("red", "yellow", "blue")
)
#>                  0.648    -0.171      0.724     0.77    -0.872   
#>                  0.0758    1.63       2.35      0.563    0.107   
#>                  0.492    -0.783     -0.281    -0.374   -0.587   
#>                 -0.754    -0.00289   -0.481    -0.601   -0.328   
#>                  0.349     0.413      0.0792   -0.424   -0.0854  
#> 
#> Column names: V1, V2, V3, V4, V5
map_background_color(
  ht,
  by_colorspace("red", "yellow", "blue",
    colwise = TRUE
  )
)
#>                  0.648    -0.171      0.724     0.77    -0.872   
#>                  0.0758    1.63       2.35      0.563    0.107   
#>                  0.492    -0.783     -0.281    -0.374   -0.587   
#>                 -0.754    -0.00289   -0.481    -0.601   -0.328   
#>                  0.349     0.413      0.0792   -0.424   -0.0854  
#> 
#> Column names: V1, V2, V3, V4, V5
```
