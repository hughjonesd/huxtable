# Use dplyr verbs with huxtable objects

Huxtable can be used with dplyr verbs
[`dplyr::select()`](https://dplyr.tidyverse.org/reference/select.html),
[`dplyr::rename()`](https://dplyr.tidyverse.org/reference/rename.html),
[`dplyr::relocate()`](https://dplyr.tidyverse.org/reference/relocate.html),
[`dplyr::slice()`](https://dplyr.tidyverse.org/reference/slice.html),
[`dplyr::arrange()`](https://dplyr.tidyverse.org/reference/arrange.html),
[`dplyr::mutate()`](https://dplyr.tidyverse.org/reference/mutate.html)
and
[`dplyr::transmute()`](https://dplyr.tidyverse.org/reference/mutate.html).
These will return huxtables. Other verbs like
[`dplyr::summarise()`](https://dplyr.tidyverse.org/reference/summarise.html)
will simply return data frames as normal;
[`dplyr::pull()`](https://dplyr.tidyverse.org/reference/pull.html) will
return a vector. `mutate` has an extra option, detailed below.

## Usage

``` r
mutate.huxtable(.data, ..., copy_cell_props = TRUE)
```

## Arguments

- .data:

  A huxtable.

- ...:

  Arguments passed to
  [`dplyr::mutate()`](https://dplyr.tidyverse.org/reference/mutate.html).

- copy_cell_props:

  Logical: copy cell and column properties from existing columns.

## Details

If `mutate` creates new columns, and the argument `copy_cell_props` is
missing or `TRUE`, then cell and column properties will be copied from
existing columns to their left, if there are any. Otherwise, they will
be the standard defaults. Row and table properties, and properties of
cells in existing columns, remain unchanged.

## Examples

``` r
ht <- hux(a = 1:5, b = 1:5, c = 1:5, d = 1:5, add_colnames = FALSE)
bold(ht)[c(1, 3), ] <- TRUE
bold(ht)[, 1] <- TRUE
ht2 <- dplyr::select(ht, b:c)
ht2
#>                                      1         1  
#>                                      2         2  
#>                                      3         3  
#>                                      4         4  
#>                                      5         5  
#> 
#> Column names: b, c
bold(ht2)
#>       b     c
#> 1  TRUE  TRUE
#> 2 FALSE FALSE
#> 3  TRUE  TRUE
#> 4 FALSE FALSE
#> 5 FALSE FALSE
ht3 <- dplyr::mutate(ht, x = a + b)
ht3
#>                             1     1     1     1     2  
#>                             2     2     2     2     4  
#>                             3     3     3     3     6  
#>                             4     4     4     4     8  
#>                             5     5     5     5    10  
#> 
#> Column names: a, b, c, d, x
bold(ht3)
#>      a     b     c     d     x
#> 1 TRUE  TRUE  TRUE  TRUE  TRUE
#> 2 TRUE FALSE FALSE FALSE FALSE
#> 3 TRUE  TRUE  TRUE  TRUE  TRUE
#> 4 TRUE FALSE FALSE FALSE FALSE
#> 5 TRUE FALSE FALSE FALSE FALSE
ht4 <- dplyr::mutate(ht,
  x = a + b,
  copy_cell_props = FALSE
)
bold(ht4)
#>      a     b     c     d     x
#> 1 TRUE  TRUE  TRUE  TRUE FALSE
#> 2 TRUE FALSE FALSE FALSE FALSE
#> 3 TRUE  TRUE  TRUE  TRUE FALSE
#> 4 TRUE FALSE FALSE FALSE FALSE
#> 5 TRUE FALSE FALSE FALSE FALSE
```
