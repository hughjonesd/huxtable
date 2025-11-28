# Set the width of the table caption

A numeric widths is interpreted as a proportion of text width in LaTeX,
or of width of the containing element in HTML. A character width must be
a valid LaTeX or CSS dimension. The default, `NA`, makes the caption the
same width as the table.

## Usage

``` r
caption_width(ht)

caption_width(ht) <- value

set_caption_width(ht, value)
```

## Arguments

- ht:

  A huxtable.

- value:

  Number or string. Set to `NA` to reset to the default, which is
  `NA_real_`.

## Value

`property()` returns the property value(s). `set_property()` and
`map_property()` return the modified huxtable.

## See also

Other caption properties: [`caption()`](caption.md),
[`caption_pos()`](caption_pos.md)

## Examples

``` r
set_caption_width(jams, 0.5)
#>                               Type           Price  
#>                               Strawberry      1.90  
#>                               Raspberry       2.10  
#>                               Plum            1.80  
#> 
#> Column names: Type, Price
```
