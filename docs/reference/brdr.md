# Create a border object

`brdr()` objects can be passed into [`set_top_border()`](borders.md) and
friends. They set multiple border properties simultaneously.

## Usage

``` r
brdr(thickness = 0.4, style = "solid", color = NA_character_)
```

## Arguments

- thickness:

  Thickness of the border in points.

- style:

  "solid" (the default), "double", "dashed" or "dotted".

- color:

  String representing a valid color (either a color name or a
  hexadecimalstring like "#00FF00").

## Value

An object of class "brdr".

## Examples

``` r
set_bottom_border(jams, brdr(1, "solid", "red"))
#>                               Type           Price  
#>                             ────────────────────────
#>                               Strawberry      1.90  
#>                             ────────────────────────
#>                               Raspberry       2.10  
#>                             ────────────────────────
#>                               Plum            1.80  
#>                             ────────────────────────
#> 
#> Column names: Type, Price
```
