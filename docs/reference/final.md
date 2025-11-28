# Return the last n rows or columns

This is a convenience function to use in row and column specifications.
In that context, it returns the last n row or column numbers of the
huxtable.

## Usage

``` r
final(n = 1)
```

## Arguments

- n:

  Number of rows to return.

## Details

Technically, `final` returns a two-argument function - see
[rowspecs](rowspecs.md) for more details.

## Examples

``` r
set_bold(jams, final(2), final(1), TRUE)
#>                               Type           Price  
#>                               Strawberry      1.90  
#>                               Raspberry       2.10  
#>                               Plum            1.80  
#> 
#> Column names: Type, Price
```
