# Restack huxtables across/down the page

- `restack_across()` splits a huxtable horizontally, then joins the
  parts up side by side.

- `restack_down()` splits a huxtable vertically, then joins the parts up
  top to bottom.

## Usage

``` r
restack_across(
  ht,
  rows,
  headers = TRUE,
  on_remainder = c("warn", "stop", "fill")
)

restack_down(
  ht,
  cols,
  headers = TRUE,
  on_remainder = c("warn", "stop", "fill")
)
```

## Arguments

- ht:

  A huxtable

- rows, cols:

  How many rows/columns the new result should have.

- headers:

  Logical. Take account of header rows/columns?

- on_remainder:

  String. "warn", "stop" or "fill". See below.

## Value

A new huxtable.

## Details

If `headers` is `TRUE`, header rows/columns will be repeated across/down
the restacked huxtable as necessary.

`on_remainder` determines what happens if the huxtable could not be
evenly divided for restacking:

- `"stop"`: stop with an error.

- `"fill"`: fill the remainder with empty cells.

- `"warn"` (the default): issue a warning, then fill the remainder with
  empty cells.

## See also

[split-across-down](split-across-down.md)

## Examples

``` r
ht <- as_hux(matrix(LETTERS[1:4], 2, 2))
ht <- set_all_borders(ht)
ht
#>                              ┌─────────┬─────────┐
#>                              │ A       │ C       │
#>                              ├─────────┼─────────┤
#>                              │ B       │ D       │
#>                              └─────────┴─────────┘
#> 
#> Column names: V1, V2

restack_down(ht, 1)
#>                                ┌────────────────┐
#>                                │ A              │
#>                                ├────────────────┤
#>                                │ B              │
#>                                ├────────────────┤
#>                                │ C              │
#>                                ├────────────────┤
#>                                │ D              │
#>                                └────────────────┘
#> 
#> Column names: V1
restack_across(ht, 1)
#>                          ┌──────┬──────┬──────┬──────┐
#>                          │ A    │ C    │ B    │ D    │
#>                          └──────┴──────┴──────┴──────┘
#> 
#> Column names: V1, V2, V1.1, V2.1

# headers:
restack_across(jams, 2)
#>              Type         Price   Type        Price   Type   Price  
#>              Strawberry    1.90   Raspberry    2.10   Plum    1.80  
#> 
#> Column names: Type, Price, Type.1, Price.1, Type.2, Price.2
restack_across(jams, 2,
  headers = FALSE
)
#>                      Type         Price   Raspberry   2.10  
#>                      Strawberry    1.90   Plum        1.80  
#> 
#> Column names: Type, Price, Type.1, Price.1

# on_remainder:
restack_across(jams, 3,
  on_remainder = "fill"
)
#>                        Type         Price   Type   Price  
#>                        Strawberry    1.90   Plum    1.80  
#>                        Raspberry     2.10                 
#> 
#> Column names: Type, Price, Type.1, Price.1
```
