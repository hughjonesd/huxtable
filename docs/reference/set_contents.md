# Set cell contents

`set_contents()` is a convenience function to change the cell contents
of a huxtable within a dplyr chain. `set_contents(ht, x, y, foo)` just
calls `ht[x, y] <- foo` and returns `ht`.

## Usage

``` r
contents(ht)

contents(ht) <- value

set_contents(ht, row, col, value)

map_contents(ht, row, col, fn)
```

## Arguments

- ht:

  A huxtable.

- value:

  Cell contents.

- row:

  A row specifier. See [rowspecs](rowspecs.md) for details.

- col:

  An optional column specifier.

- fn:

  A mapping function. See [mapping-functions](mapping-functions.md) for
  details.

## Examples

``` r
data(jams)
set_contents(jams, 2, 1, "Blackcurrant")
#>                              Type             Price  
#>                              Blackcurrant      1.90  
#>                              Raspberry         2.10  
#>                              Plum              1.80  
#> 
#> Column names: Type, Price
map_contents(jams, by_regex(".*berry" = "Snodberry"))
#>                               Type          Price  
#>                               Snodberry      1.90  
#>                               Snodberry      2.10  
#>                               Plum           1.80  
#> 
#> Column names: Type, Price
```
