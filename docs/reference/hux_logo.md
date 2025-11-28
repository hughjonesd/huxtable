# Huxtable logo

Returns a randomized huxtable logo, inspired by Mondrian.

## Usage

``` r
hux_logo(compact = FALSE, latex = NULL)
```

## Arguments

- compact:

  Logical. Create a compact 1-row huxtable (default is 2 rows)?

- latex:

  Logical. Output for LaTeX?

## Value

A huxtable.

## Examples

``` r
# Default logo
print_screen(hux_logo())
#>                       ┌────┬────┬────┬────┬──────────────┐
#>                       │ h  │ u  │ X  │    │              │
#>                       ├────┼────┤    ├────┼────┬────┬────┤
#>                       │    │    │    │ t  │ a  │ b  │ l  │
#>                       └────┴────┴────┴────┴────┴────┴────┘
#> 
#> Column names: V1, V2, V3, V4, V5, V6, V7, V8
#> 
#> 7/8 columns shown.

# Compact single-row version
print_screen(hux_logo(compact = TRUE))
#>                       ┌────┬────┬────┬────┬────┬────┬────┐
#>                       │ h  │ u  │ X  │ t  │ a  │ b  │ l  │
#>                       └────┴────┴────┴────┴────┴────┴────┘
#> 
#> Column names: V1, V2, V3, V4, V5, V6, V7, V8
#> 
#> 7/8 columns shown.
```
