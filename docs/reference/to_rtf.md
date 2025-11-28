# Create RTF representing a huxtable

These functions print or return an RTF character string.

## Usage

``` r
print_rtf(ht, fc_tables = rtf_fc_tables(ht), ...)

to_rtf(ht, fc_tables = rtf_fc_tables(ht), ...)
```

## Arguments

- ht:

  A huxtable.

- fc_tables:

  See [`rtf_fc_tables()`](rtf_fc_tables.md).

- ...:

  Arguments passed to methods.

## Value

`to_rtf` returns a string representing an RTF table. The `fc_tables`
attribute of the returned string will contain the `fc_tables` object
that was passed in (or autocreated). `print_rtf` prints the string and
returns `NULL`.

## Details

RTF files use a single per-document table for colors, and one for fonts.
If you are printing multiple huxtables in a document, you need to make
sure that the font and color table is set up correctly and that the RTF
tables refer back to them. See [`rtf_fc_tables()`](rtf_fc_tables.md).

1.  Prepare all the huxtables;

2.  Call [`rtf_fc_tables()`](rtf_fc_tables.md), passing in all the
    huxtables;

3.  Print the `rtfFCTables` object in the RTF document header;

4.  Pass in the `rtfFCTables` object to each call to `print_rtf`.

## Limitations

- rmarkdown"s `rtf_document` can"t yet print out customized color
  tables, so custom fonts and colors won"t work in this context.

- [`col_width()`](col_width.md) and [`width()`](width.md) can only be
  numeric or "pt".

- [`wrap()`](wrap.md) has no effect: cell contents always wrap.

- [`rotation()`](rotation.md) can only be 90 or 270, i.e. text going up
  or down.

## See also

Other printing functions: [`print_html()`](to_html.md),
[`print_latex()`](to_latex.md), [`print_md()`](to_md.md),
[`print_screen()`](to_screen.md), [`print_typst()`](to_typst.md)

## Examples

``` r
print_rtf(jams)
#> 
#> {
#> \trowd
#> \trqc \clbrdrt\clbrdrl\clbrdrb\clbrdrr\clvertalt\clpadfl3\clpadl120 \clpadft3\clpadt120 \clpadfb3\clpadb120 \clpadfr3\clpadr120 \cellx2160 
#> \clbrdrt\clbrdrl\clbrdrb\clbrdrr\clvertalt\clpadfl3\clpadl120 \clpadft3\clpadt120 \clpadfb3\clpadb120 \clpadfr3\clpadr120 \cellx4320 \pard\intbl\ql{Type}\cell
#> \pard\intbl\qr{Price}\cell
#> \row
#> }
#> 
#> {
#> \trowd
#> \trqc \clbrdrt\clbrdrl\clbrdrb\clbrdrr\clvertalt\clpadfl3\clpadl120 \clpadft3\clpadt120 \clpadfb3\clpadb120 \clpadfr3\clpadr120 \cellx2160 
#> \clbrdrt\clbrdrl\clbrdrb\clbrdrr\clvertalt\clpadfl3\clpadl120 \clpadft3\clpadt120 \clpadfb3\clpadb120 \clpadfr3\clpadr120 \cellx4320 \pard\intbl\ql{Strawberry}\cell
#> \pard\intbl\qr{1.90}\cell
#> \row
#> }
#> 
#> {
#> \trowd
#> \trqc \clbrdrt\clbrdrl\clbrdrb\clbrdrr\clvertalt\clpadfl3\clpadl120 \clpadft3\clpadt120 \clpadfb3\clpadb120 \clpadfr3\clpadr120 \cellx2160 
#> \clbrdrt\clbrdrl\clbrdrb\clbrdrr\clvertalt\clpadfl3\clpadl120 \clpadft3\clpadt120 \clpadfb3\clpadb120 \clpadfr3\clpadr120 \cellx4320 \pard\intbl\ql{Raspberry}\cell
#> \pard\intbl\qr{2.10}\cell
#> \row
#> }
#> 
#> {
#> \trowd
#> \trqc \clbrdrt\clbrdrl\clbrdrb\clbrdrr\clvertalt\clpadfl3\clpadl120 \clpadft3\clpadt120 \clpadfb3\clpadb120 \clpadfr3\clpadr120 \cellx2160 
#> \clbrdrt\clbrdrl\clbrdrb\clbrdrr\clvertalt\clpadfl3\clpadl120 \clpadft3\clpadt120 \clpadfb3\clpadb120 \clpadfr3\clpadr120 \cellx4320 \pard\intbl\ql{Plum}\cell
#> \pard\intbl\qr{1.80}\cell
#> \row
#> }
```
