# Return every n row or column numbers

This is a convenience function to use in row or column specifications.
In this context, `stripe(n, from)` will return `from, from + n, ...,` up
to the number of rows or columns of the huxtable. `evens` and `odds`
return even and odd numbers, i.e. they are equivalent to `stripe(2, 2)`
and `stripe(2, 1)` respectively. `everywhere` returns all rows or
columns, equivalently to `stripe(1)`.

## Usage

``` r
stripe(n = 1, from = n)

everywhere(ht, dimension)

evens(ht, dimension)

odds(ht, dimension)
```

## Arguments

- n:

  A number (at least 1)

- from:

  A number (at least 1)

- ht:

  An object with a `dim` attribute like a matrix or data frame.

- dimension:

  Number of the dimension to use.

## Details

Technically, `stripe` returns a 2-argument function which can be called
like `f(ht, dimension)`. See [rowspecs](rowspecs.md) for details.

Until huxtable 5.0.0, `stripe` was called `every`. It was renamed to
avoid a clash with
[`purrr::every`](https://purrr.tidyverse.org/reference/every.html).

## Examples

``` r
ht <- huxtable(a = 1:10, b = 1:10)
set_background_color(
  ht,
  evens, everywhere,
  "grey95"
)
#>                                      a         b  
#>                                      1         1  
#>                                      2         2  
#>                                      3         3  
#>                                      4         4  
#>                                      5         5  
#>                                      6         6  
#>                                      7         7  
#>                                      8         8  
#>                                      9         9  
#>                                     10        10  
#> 
#> Column names: a, b
set_background_color(
  ht,
  stripe(3), everywhere,
  "grey95"
)
#>                                      a         b  
#>                                      1         1  
#>                                      2         2  
#>                                      3         3  
#>                                      4         4  
#>                                      5         5  
#>                                      6         6  
#>                                      7         7  
#>                                      8         8  
#>                                      9         9  
#>                                     10        10  
#> 
#> Column names: a, b
```
