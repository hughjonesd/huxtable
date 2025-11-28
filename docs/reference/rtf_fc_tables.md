# Create RTF font and color tables

Create RTF font and color tables

## Usage

``` r
rtf_fc_tables(..., extra_fonts = "Times", extra_colors = character(0))
```

## Arguments

- ...:

  One or more objects of class `huxtable`.

- extra_fonts:

  Extra fonts to include. These will be first in the fonts table.

- extra_colors:

  Extra colors to include, as R color names.

## Value

An object of class `rtfFCTables`. This is a list containing two items:
`"fonts"` is a character vector of unique font names; `"colors"` is a
character vector of unique color names.

## Details

RTF documents have a single table of fonts, and a table of colors, in
the RTF header. To create font and color tables for multiple huxtables,
use this command. You can `print` the returned object in the RTF header.
Pass it to [`print_rtf()`](to_rtf.md) or [`to_rtf()`](to_rtf.md) to
ensure that huxtables print out the correct colour references.

## Examples

``` r
# Printing multiple huxtables:

ht <- huxtable("Blue with red border")
ht <- set_all_borders(ht, 1)
ht <- set_all_border_colors(ht, "red")
background_color(ht) <- "blue"

ht2 <- huxtable("Dark green text")
text_color(ht2) <- "darkgreen"

fc_tbls <- rtf_fc_tables(ht, ht2)

# In the document header:
print(fc_tbls)
#> $fonts
#> [1] "Times"
#> attr(,"na.action")
#> [1] 2
#> attr(,"class")
#> [1] "omit"
#> 
#> $colors
#> [1] "blue"      "red"       "darkgreen"
#> attr(,"na.action")
#> [1] 1
#> attr(,"class")
#> [1] "omit"
#> 
#> attr(,"class")
#> [1] "rtfFCTables"

# In the document body:
print_rtf(ht, fc_tables = fc_tbls)
#> 
#> {
#> \trowd
#> \trqc \clbrdrt\brdrs\brdrw20\brdrcf2\clbrdrl\brdrs\brdrw20\brdrcf2\clbrdrb\brdrs\brdrw20\brdrcf2\clbrdrr\brdrs\brdrw20\brdrcf2\clcbpat1\clvertalt\clpadfl3\clpadl120 \clpadft3\clpadt120 \clpadfb3\clpadb120 \clpadfr3\clpadr120 \cellx4320 \pard\intbl\ql{Blue with red border}\cell
#> \row
#> }
print_rtf(ht2, fc_tables = fc_tbls)
#> 
#> {
#> \trowd
#> \trqc \clbrdrt\clbrdrl\clbrdrb\clbrdrr\clvertalt\clpadfl3\clpadl120 \clpadft3\clpadt120 \clpadfb3\clpadb120 \clpadfr3\clpadr120 \cellx4320 \pard\intbl\ql{\cf3 {Dark green text}}\cell
#> \row
#> }
```
