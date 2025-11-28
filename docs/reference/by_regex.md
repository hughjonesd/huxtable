# Map cells matching a string or regex to cell properties

`by_regex()` sets properties on cells which match a [regular
expression](https://rdrr.io/r/base/regex.html).

## Usage

``` r
by_regex(..., .grepl_args = list(), ignore_na = TRUE)
```

## Arguments

- ...:

  A list of name-value pairs. The names are regular expressions. If
  there is a single unnamed argument, this is the default value for
  unmatched cells. More than one unnamed argument is an error.

- .grepl_args:

  A list of arguments to pass to
  [`grepl()`](https://rdrr.io/r/base/grep.html). Useful options include
  `fixed`, `perl` and `ignore.case`.

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
[`by_ranges()`](by_ranges.md), [`by_rows()`](by_rows.md),
[`by_values()`](by_values.md)

## Examples

``` r
ht <- hux(c("The cat sat", "on the", "mat"))

map_bold(ht, by_regex("at" = TRUE))
#>                                  The cat sat     
#>                                  on the          
#>                                  mat             
map_bold(ht, by_regex("a.*a" = TRUE))
#>                                  The cat sat     
#>                                  on the          
#>                                  mat             

map_bold(ht, by_regex(
  "the" = TRUE,
  .grepl_args = list(
    ignore.case = TRUE
  )
))
#>                                  The cat sat     
#>                                  on the          
#>                                  mat             
```
