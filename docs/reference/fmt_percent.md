# Format numbers as percent

`fmt_` functions are designed to work with
[`number_format()`](number_format.md).

## Usage

``` r
fmt_percent(digits = 1, format = "f", ...)
```

## Arguments

- digits:

  How many digits to print.

- format, ...:

  Passed into [`formatC()`](https://rdrr.io/r/base/formatc.html).

## Value

An object you can pass into [`number_format()`](number_format.md).

## See also

Other format functions: [`fmt_pretty()`](fmt_pretty.md)

## Examples

``` r
jams$Sugar <- c(
  "Sugar content",
  0.4, 0.35, 0.45
)
set_number_format(
  jams, -1, "Sugar",
  fmt_percent(1)
)
#>                        Type         Price   Sugar content  
#>                        Strawberry    1.90           40.0%  
#>                        Raspberry     2.10           35.0%  
#>                        Plum          1.80           45.0%  
#> 
#> Column names: Type, Price, Sugar
```
