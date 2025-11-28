# Map specific cell values to cell properties

Use `by_values()` to set properties for cells with specific,
pre-determined contents.

## Usage

``` r
by_values(..., ignore_na = TRUE)
```

## Arguments

- ...:

  Name-value pairs like `name = value`. Cells where contents are equal
  to `name` will have the property set to `value`. If there is a single
  unnamed argument, this is the default value for unmatched cells. More
  than one unnamed argument is an error.

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
[`by_rows()`](by_rows.md)

## Examples

``` r
ht <- hux(letters[1:3])
map_background_color(
  ht,
  by_values(a = "red", c = "yellow")
)
#>                                  a               
#>                                  b               
#>                                  c               
map_background_color(
  ht,
  by_values(a = "red", c = "yellow", "green")
)
#>                                  a               
#>                                  b               
#>                                  c               
```
