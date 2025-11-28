# Different ways to select rows and columns

This help page describes how to use the `row` and `col` arguments in
`set_*` functions.

## The basics

The `set_*` functions for cell properties all have arguments like this:
`set_property(ht, row, col, value)`.

You can treat `row` and `col` arguments like arguments for [data frame
subsetting](https://rdrr.io/r/base/Extract.data.frame.html). For
example, you can use `row = 1:3` to get the first three rows,
`col = "salary"` to specify the column named "salary", or
`row = ht$salary >= 50000` to specify rows where a condition is true.

There are also a few extra tricks you can use:

- Write `set_property(ht, x)`, omitting `row` and `col`, to set the
  property to `x` for all cells.

- Use `everywhere` to refer to all rows or all columns.

- Use `final(n)` to refer to the last n rows or columns.

- Use `evens` to get only even rows/columns and `odds` for only odd
  ones.

- Use [`stripe(n, from = m)`](stripe.md) to get every nth row/column
  starting at row/column m.

- Use `dplyr` functions like `starts_with`, `contains` and `matches` to
  specify columns (but not rows). See
  [tidyselect::language](https://tidyselect.r-lib.org/reference/language.html)
  for a full list.

## The gory details

How the row and col arguments are parsed depends on the number of
arguments passed to the `set_*` function.

- If there are two arguments then the second argument is taken as the
  value and is set for all rows and columns.

- If there are four arguments:

  - If `row` or `col` is numeric, character or logical, it is evaluated
    just as in standard subsetting. `col` will be evaluated in a special
    context provided by
    [`tidyselect::with_vars()`](https://tidyselect.r-lib.org/reference/poke_vars.html)
    to allow the use of dplyr functions.

  - If `row` or `col` is a function,it is called with two arguments: the
    huxtable, and the dimension number being evaluated, i.e. 1 for rows,
    2 for columns. It must return a vector of column indices.
    [`evens()`](stripe.md), [`odds()`](stripe.md),
    [`stripe()`](stripe.md) and [`final()`](final.md) return functions
    for this purpose.

## Examples

``` r
set_bold(jams, 2:4, 1:2, TRUE)
#>                               Type           Price  
#>                               Strawberry      1.90  
#>                               Raspberry       2.10  
#>                               Plum            1.80  
#> 
#> Column names: Type, Price
set_background_color(
  jams, evens, everywhere,
  "grey95"
)
#>                               Type           Price  
#>                               Strawberry      1.90  
#>                               Raspberry       2.10  
#>                               Plum            1.80  
#> 
#> Column names: Type, Price
set_bold(
  jams, everywhere,
  tidyselect::matches("yp"), TRUE
)
#>                               Type           Price  
#>                               Strawberry      1.90  
#>                               Raspberry       2.10  
#>                               Plum            1.80  
#> 
#> Column names: Type, Price

set_text_color(
  jams, 2:4, 1:2,
  c("red", "violetred", "purple")
)
#>                               Type           Price  
#>                               Strawberry      1.90  
#>                               Raspberry       2.10  
#>                               Plum            1.80  
#> 
#> Column names: Type, Price
```
