# Use `prettyNum()` to format numbers

Use [`prettyNum()`](https://rdrr.io/r/base/formatc.html) to format
numbers

## Usage

``` r
fmt_pretty(big.mark = ",", ..., scientific = FALSE)
```

## Arguments

- big.mark, scientific, ...:

  Passed to [`prettyNum()`](https://rdrr.io/r/base/formatc.html).

## Value

An object you can pass into [`number_format()`](number_format.md).

## See also

Other format functions: [`fmt_percent()`](fmt_percent.md)

## Examples

``` r
jams$Sales <- c(
  "Sales", 35000,
  55500, 20000
)
set_number_format(
  jams, -1, "Sales",
  fmt_pretty()
)
#>                           Type         Price    Sales  
#>                           Strawberry    1.90   35,000  
#>                           Raspberry     2.10   55,500  
#>                           Plum          1.80   20,000  
#> 
#> Column names: Type, Price, Sales
```
