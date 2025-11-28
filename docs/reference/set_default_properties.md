# Default huxtable properties

Defaults are used for new huxtables, and also when a property is set to
`NA`.

## Usage

``` r
set_default_properties(...)

get_default_properties(names = NULL)
```

## Arguments

- ...:

  Properties specified by name, or a single named list.

- names:

  Vector of property names. If `NULL`, all properties are returned.

## Value

For `set_default_properties`, a list of the previous property values,
invisibly.

For `get_default_properties`, a list of the current defaults.

## Details

Note that `autoformat = TRUE` in [`huxtable()`](huxtable.md) overrides
some defaults.

To set default border styles, use the pseudo-properties
`border`/`border_style`/`border_color`. You cannot set defaults
separately for different sides.

## See also

Options for autoformat in [huxtable-options](huxtable-options.md).

## Examples

``` r
old <- set_default_properties(
  text_color = "red",
  border     = 0.4
)
hux(a = 1:2, b = 1:2)
#>                              ┌─────────┬─────────┐
#>                              │       a │       b │
#>                              ├─────────┼─────────┤
#>                              │       1 │       1 │
#>                              ├─────────┼─────────┤
#>                              │       2 │       2 │
#>                              └─────────┴─────────┘
#> 
#> Column names: a, b
set_default_properties(old)
get_default_properties("bold")
#> $bold
#> [1] FALSE
#> 
```
