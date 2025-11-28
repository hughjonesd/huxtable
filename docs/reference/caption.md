# Set the table caption

By default, captions are displayed above the table. You can change this
with [`caption_pos()`](caption_pos.md).

## Usage

``` r
caption(ht)

caption(ht) <- value

set_caption(ht, value)
```

## Arguments

- ht:

  A huxtable.

- value:

  A string. Set to `NA` to reset to the default, which is
  `NA_character_`.

## Value

`property()` returns the property value(s). `set_property()` and
`map_property()` return the modified huxtable.

## Details

Captions are not escaped. See the example for a workaround.

## See also

Other caption properties: [`caption_pos()`](caption_pos.md),
[`caption_width()`](caption_width.md)

## Examples

``` r
set_caption(jams, "Pots of jam for sale")
#>                               Pots of jam for sale                              
#>                               Type           Price  
#>                               Strawberry      1.90  
#>                               Raspberry       2.10  
#>                               Plum            1.80  
#> 
#> Column names: Type, Price
# escape caption characters:
caption(jams) <- sanitize(
  "Make $$$ with jam",
  type = "latex"
)
```
