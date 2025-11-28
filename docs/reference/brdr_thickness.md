# Get thickness of a [`brdr()`](brdr.md) object

Get thickness of a [`brdr()`](brdr.md) object

## Usage

``` r
brdr_thickness(x)
```

## Arguments

- x:

  A [`brdr()`](brdr.md) object.

## Value

A number or numeric matrix.

## Examples

``` r
brdr_thickness(left_border(jams))
#>     Type Price
#> 1      0     0
#> 1.1    0     0
#> 2      0     0
#> 3      0     0
brdr_thickness(brdr(1, "solid", "red"))
#> [1] 1
```
