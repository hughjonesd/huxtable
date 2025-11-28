# Map numeric quantiles to cell properties

These functions split cell values by quantiles. Non-numeric cells are
ignored.

## Usage

``` r
by_quantiles(
  quantiles,
  values,
  right = FALSE,
  extend = TRUE,
  ignore_na = TRUE,
  colwise = FALSE
)

by_equal_groups(n, values, ignore_na = TRUE, colwise = FALSE)
```

## Arguments

- quantiles:

  Vector of quantiles.

- values:

  Vector of values. `length(values)` should be one greater than
  `length(quantiles)`, or one less if `extend = FALSE`.

- right:

  If `TRUE`, intervals are closed on the right, i.e. if values are
  exactly equal to a [`break`](https://rdrr.io/r/base/Control.html),
  they go in the lower group. Otherwise, intervals are closed on the
  left, so equal values go in the higher group. `FALSE` by default.

- extend:

  Extend `breaks` to `c(-Inf, breaks, Inf)`, i.e. include numbers below
  and above the outermost breaks. `TRUE` by default.

- ignore_na:

  If `TRUE`, `NA` values in the result will be left unchanged from their
  previous values. Otherwise, `NA` normally resets to the default.

- colwise:

  Logical. Calculate breaks separately within each column?

- n:

  Number of equal-sized groups. `length(values)` should equal `n`.

## Value

A function for use in `map_***` functions.

## Details

`by_equal_groups(n, values)` splits the data into `n` equal-sized groups
(i.e. it is a shortcut for
`by_quantiles(seq(1/n, 1 - 1/n, 1/n), values)`).

## See also

[mapping-functions](mapping-functions.md)

Other mapping functions: [`by_cases()`](by_cases.md),
[`by_colorspace()`](by_colorspace.md),
[`by_function()`](by_function.md), [`by_ranges()`](by_ranges.md),
[`by_regex()`](by_regex.md), [`by_rows()`](by_rows.md),
[`by_values()`](by_values.md)

## Examples

``` r
ht <- hux(rnorm(5), rnorm(5))

map_background_color(
  ht,
  by_quantiles(
    c(0.2, 0.8),
    c("red", "yellow", "green")
  )
)
#>                                -0.702      0.437  
#>                                 0.973      0.413  
#>                                -0.0768     0.976  
#>                                 0.893      1.15   
#>                                -0.778      1.22   

map_background_color(
  ht,
  by_quantiles(
    c(0.2, 0.8),
    c("red", "yellow", "green"),
    colwise = TRUE
  )
)
#>                                -0.702      0.437  
#>                                 0.973      0.413  
#>                                -0.0768     0.976  
#>                                 0.893      1.15   
#>                                -0.778      1.22   

map_background_color(
  ht,
  by_equal_groups(
    3,
    c("red", "yellow", "green")
  )
)
#>                                -0.702      0.437  
#>                                 0.973      0.413  
#>                                -0.0768     0.976  
#>                                 0.893      1.15   
#>                                -0.778      1.22   
```
