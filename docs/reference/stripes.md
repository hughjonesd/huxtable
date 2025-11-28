# Set background color stripes

These convenience functions call
[map_background_color](background_color.md) with [by_rows](by_rows.md)
or [by_cols](by_rows.md).

## Usage

``` r
stripe_rows(ht, stripe1 = "white", stripe2 = "grey90")

stripe_columns(ht, stripe1 = "white", stripe2 = "grey90")
```

## Arguments

- ht:

  A huxtable.

- stripe1:

  Color for rows/columns 1, 3, 5, ...

- stripe2:

  Color for rows/columns 2, 4, 6, ...

## Examples

``` r
stripe_rows(jams)
#>                               Type           Price  
#>                               Strawberry      1.90  
#>                               Raspberry       2.10  
#>                               Plum            1.80  
#> 
#> Column names: Type, Price
stripe_columns(jams)
#>                               Type           Price  
#>                               Strawberry      1.90  
#>                               Raspberry       2.10  
#>                               Plum            1.80  
#> 
#> Column names: Type, Price
stripe_rows(jams, "red", "blue")
#>                               Type           Price  
#>                               Strawberry      1.90  
#>                               Raspberry       2.10  
#>                               Plum            1.80  
#> 
#> Column names: Type, Price
```
