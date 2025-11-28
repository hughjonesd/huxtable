# Map cell contents to properties using `case_when`

This function uses
[`dplyr::case_when()`](https://dplyr.tidyverse.org/reference/case_when.html)
to set cell properties.

## Usage

``` r
by_cases(..., ignore_na = TRUE)
```

## Arguments

- ...:

  A list of two-sided formulas interpreted by `case_when`.

- ignore_na:

  If `TRUE`, `NA` values in the result will be left unchanged from their
  previous values. Otherwise, `NA` normally resets to the default.

## Value

A function for use in `map_***` functions.

## Details

Within the formulas, the variable `.` will refer to the content of
`ht[rows, cols]`, after conversion to a vector.

`case_when` returns `NA` when no formula LHS is matched. To avoid this,
set a default in the last formula: `TRUE ~ default`.

`case_when` can't deal with [`brdr()`](brdr.md) objects, so you cannot
use these in `by_cases()`.

## See also

[mapping-functions](mapping-functions.md)

Other mapping functions: [`by_colorspace()`](by_colorspace.md),
[`by_function()`](by_function.md), [`by_quantiles()`](by_quantiles.md),
[`by_ranges()`](by_ranges.md), [`by_regex()`](by_regex.md),
[`by_rows()`](by_rows.md), [`by_values()`](by_values.md)

## Examples

``` r
if (!requireNamespace("dplyr")) {
  stop("Please install the 'dplyr' package to run this example")
}

ht <- hux(runif(5), letters[1:5])

map_background_color(ht, by_cases(
  . == "a" ~ "red",
  . %in% letters ~ "green",
  . < 0.5 ~ "pink"
))
#>                                  0.29    a        
#>                                  0.678   b        
#>                                  0.735   c        
#>                                  0.196   d        
#>                                  0.981   e        
```
