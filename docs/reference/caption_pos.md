# Position the table's caption

If `caption_pos` is "top" or "bottom", then the horizontal position
("left", "center" or "right") will be determined by the huxtable"s
[`position()`](position.md).

## Usage

``` r
caption_pos(ht)

caption_pos(ht) <- value

set_caption_pos(ht, value)
```

## Arguments

- ht:

  A huxtable.

- value:

  String: "top", "bottom", "topleft", "topcenter", "topright",
  "bottomleft", "bottomcenter" or "bottomright". Set to `NA` to reset to
  the default, which is `"top"`.

## Value

`property()` returns the property value(s). `set_property()` and
`map_property()` return the modified huxtable.

## See also

Other caption properties: [`caption()`](caption.md),
[`caption_width()`](caption_width.md)

## Examples

``` r
caption(jams) <- "Jam for sale"
jams
#>                                   Jam for sale                                  
#>                               Type           Price  
#>                               Strawberry      1.90  
#>                               Raspberry       2.10  
#>                               Plum            1.80  
#> 
#> Column names: Type, Price
set_caption_pos(jams, "bottom")
#>                               Type           Price  
#>                               Strawberry      1.90  
#>                               Raspberry       2.10  
#>                               Plum            1.80  
#>                                   Jam for sale                                  
#> 
#> Column names: Type, Price
```
