# Add a row with a footnote

This adds a single row at the bottom. The first cell contains the
footnote; it spans all table columns and has an optional border above.

## Usage

``` r
add_footnote(ht, text, border = 0.8, number_format = NA, ...)
```

## Arguments

- ht:

  A huxtable.

- text:

  Text for the footnote.

- border:

  Width of the footnote's top border. Set to 0 for no border, or `NULL`
  to leave the border unchanged.

- number_format:

  Number format for the footnote cell.

- ...:

  Other properties, passed to
  [`set_cell_properties()`](style-functions.md) for the footnote cell.

## Value

The modified huxtable

## Examples

``` r
jams <- add_footnote(
  jams,
  "* subject to availability"
)
jams
#>                          Type                    Price  
#>                          Strawberry               1.90  
#>                          Raspberry                2.10  
#>                          Plum                     1.80  
#>                        ─────────────────────────────────
#>                          * subject to availability      
#> 
#> Column names: Type, Price
```
