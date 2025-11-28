# Set the horizontal alignment of cell content

Values may be "left", "center", "right", `NA` or a single character. If
`value` is a single character (e.g. a decimal point), then the cell is
aligned on this character.

## Usage

``` r
align(ht)

align(ht) <- value

set_align(ht, row, col, value)

map_align(ht, row, col, fn)
```

## Arguments

- ht:

  A huxtable.

- value:

  A character vector or matrix. Set to `NA` to reset to the default,
  which is `"left"`.

- row:

  A row specifier. See [rowspecs](rowspecs.md) for details.

- col:

  An optional column specifier.

- fn:

  A mapping function. See [mapping-functions](mapping-functions.md) for
  details.

## Aligning on a decimal point

To align cells on the decimal point, set `align` to `"."` or any other
single character (e.g. `","` in European languages).

By default, huxtable aligns these cells by padding with spaces. The
mechanics of this were improved for LaTeX in version 5.3.0, but are
still not perfect. Using a fixed-width font may help.

If `options("huxtable.latex_siunitx_align")` is set to `TRUE`, then in
LaTeX output, numbers in these cells will be surrounded by
`\\tablenum{}`. See the siunitx documentation for more details. Note
that this may have other side-effects, for example `1e3` becomes
`1 x 10^3`.

To use non-default decimal points, set both `align(ht)` and
[`number_format()`](number_format.md). See the example.

## Examples

``` r
numbers <- c(1, 1.5, 1.03, 10, 10.01)
number_hux <- as_hux(matrix(numbers, 5, 5))
number_format(number_hux) <- "%.4g"
number_format(number_hux)[, 5] <- fmt_pretty(
  decimal.mark = ",",
  big.mark = ""
)

number_hux <- map_align(
  number_hux,
  by_cols("left", "center", "right", ".", ",")
)

alignments <- c(
  "left",
  "centre",
  "right",
  "decimal (.)",
  "decimal (,)"
)
number_hux <- rbind(
  alignments,
  number_hux
)

align(number_hux)
#>   V1     V2       V3      V4     V5    
#>   "left" "left"   "left"  "left" "left"
#> 1 "left" "center" "right" "."    ","   
#> 2 "left" "center" "right" "."    ","   
#> 3 "left" "center" "right" "."    ","   
#> 4 "left" "center" "right" "."    ","   
#> 5 "left" "center" "right" "."    ","   
number_hux
#>                left    centre   right   decimal (.)   decimal (,)  
#>                1         1          1          1             1     
#>                1.5      1.5       1.5          1.5           1,5   
#>                1.03     1.03     1.03          1.03          1,03  
#>                10        10        10         10            10     
#>                10.01   10.01    10.01         10.01         10,01  
#> 
#> Column names: V1, V2, V3, V4, V5
```
