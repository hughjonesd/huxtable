# Map numeric ranges to cell properties

`by_ranges()` sets property values for cells falling within different
numeric ranges.

## Usage

``` r
by_ranges(breaks, values, right = FALSE, extend = TRUE, ignore_na = TRUE)
```

## Arguments

- breaks:

  A vector of numbers in increasing order.

- values:

  A vector of property values. `length(values)` should be one greater
  than `length(breaks)` if `extend = TRUE`, or one less if
  `extend = FALSE`.

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

## Value

A function for use in `map_***` functions.

## Details

Non-numeric cells return `NA`. The effects of this depend on
`ignore_na`.

## See also

[mapping-functions](mapping-functions.md)

Other mapping functions: [`by_cases()`](by_cases.md),
[`by_colorspace()`](by_colorspace.md),
[`by_function()`](by_function.md), [`by_quantiles()`](by_quantiles.md),
[`by_regex()`](by_regex.md), [`by_rows()`](by_rows.md),
[`by_values()`](by_values.md)

## Examples

``` r
ht <- huxtable(c(1, 3, 5))
map_background_color(
  ht,
  by_ranges(
    c(2, 4),
    c("red", "yellow", "blue")
  )
)
#>                                               1  
#>                                               3  
#>                                               5  

map_background_color(
  ht,
  by_ranges(
    c(2, 4),
    "pink",
    extend = FALSE
  )
)
#>                                               1  
#>                                               3  
#>                                               5  

map_background_color(
  ht,
  by_ranges(
    c(1, 5),
    c("red", "yellow", "green"),
    right = TRUE
  )
)
#>                                               1  
#>                                               3  
#>                                               5  
map_background_color(
  ht,
  by_ranges(
    c(1, 5),
    c("red", "yellow", "green"),
    right = FALSE
  )
)
#>                                               1  
#>                                               3  
#>                                               5  
```
