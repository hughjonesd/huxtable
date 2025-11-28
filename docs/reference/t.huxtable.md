# Transpose a huxtable

[`t()`](https://rdrr.io/r/base/t.html) switches a huxtable so rows
become columns and columns become rows.

## Usage

``` r
# S3 method for class 'huxtable'
t(x)
```

## Arguments

- x:

  A huxtable.

## Value

The transposed huxtable.

## Details

Row and column spans of `x` will be swapped, as will column widths and
row heights, table width and height, and cell borders (bottom becomes
right, etc.). Other properties - in particular, alignment, vertical
alignment and rotation - will be preserved.

## Examples

``` r
ht <- huxtable(
  a = 1:3,
  b = letters[1:3],
  autoformat = FALSE
)
bottom_border(ht)[3, ] <- 1
ht
#>                                a         b        
#>                                1         a        
#>                                2         b        
#>                              ─────────────────────
#>                                3         c        
#> 
#> Column names: a, b
t(ht)
#>                            a      1      2    │ 3     
#>                            b      a      b    │ c     
#> 
#> Column names: , 1, 2, 3
```
