# Set the vertical alignment of cell content

Allowed values are "top", "middle", "bottom" or `NA`.

## Usage

``` r
valign(ht)

valign(ht) <- value

set_valign(ht, row, col, value)

map_valign(ht, row, col, fn)
```

## Arguments

- ht:

  A huxtable.

- value:

  A character vector or matrix. Set to `NA` to reset to the default,
  which is `"top"`.

- row:

  A row specifier. See [rowspecs](rowspecs.md) for details.

- col:

  An optional column specifier.

- fn:

  A mapping function. See [mapping-functions](mapping-functions.md) for
  details.

## Details

Vertical alignment may not work for short text in LaTeX. Defining row
heights with [`row_height()`](row_height.md) may help.

## Examples

``` r
valign(jams) <- "top"
valign(jams)
#>     Type  Price
#> 1   "top" "top"
#> 1.1 "top" "top"
#> 2   "top" "top"
#> 3   "top" "top"

jams2 <- set_valign(jams, "bottom")
valign(jams2)
#>     Type     Price   
#> 1   "bottom" "bottom"
#> 1.1 "bottom" "bottom"
#> 2   "bottom" "bottom"
#> 3   "bottom" "bottom"

jams3 <- set_valign(jams, 2:3, 1, "bottom")
valign(jams3)
#>     Type     Price
#> 1   "top"    "top"
#> 1.1 "bottom" "top"
#> 2   "bottom" "top"
#> 3   "top"    "top"

jams4 <- map_valign(jams, by_rows(
  "bottom",
  "top"
))
valign(jams4)
#>     Type     Price   
#> 1   "bottom" "bottom"
#> 1.1 "top"    "top"   
#> 2   "bottom" "bottom"
#> 3   "top"    "top"   
```
