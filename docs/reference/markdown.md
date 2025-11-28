# Interpret cell content as markdown

Cells where the markdown property is `TRUE` will be interpreted as
[markdown](https://commonmark.org/help/).

## Usage

``` r
markdown(ht)

markdown(ht) <- value

set_markdown(ht, row, col, value = TRUE)

map_markdown(ht, row, col, fn)
```

## Arguments

- ht:

  A huxtable.

- value:

  A logical vector or matrix. Set to `NA` to reset to the default, which
  is `FALSE`.

- row:

  A row specifier. See [rowspecs](rowspecs.md) for details.

- col:

  An optional column specifier.

- fn:

  A mapping function. See [mapping-functions](mapping-functions.md) for
  details.

## Details

Markdown is currently implemented for HTML, Word, Powerpoint, RTF, LaTeX
and on-screen display. Word requires the `ftExtra` package.

Most formats use [commonmark](https://commonmark.org), with the
"strikethrough" extension enabled.

The following features are intended to work:

- bold and italic text

- strikethrough (write `~~text~~` to strike through text).

- hyperlinks

There are some quirks:

- Paragraph-level properties (e.g. lists) won't work in Word.

- Strikethrough will probably not work in Word.

- To make lists work in LaTeX, set [`width()`](width.md) and ensure
  [`wrap()`](wrap.md) is `TRUE`.

- Inline images in RTF work using the INCLUDEPICTURE field type.

- Only local images (not urls) work in typst.

If you try to use markdown tables within a table cell, then seek
psychiatric help.

## Note

Markdown content in cells is completely separate from printing the whole
table as markdown using [`print_md()`](to_md.md). When you set
`markdown` to `TRUE`, huxtable itself interprets the cell contents as
markdown, and spits out HTML, TeX or whatever.

## See also

[`set_markdown_contents()`](set_markdown_contents.md), a shortcut
function.

## Examples

``` r
jams[3, 2] <- "~2.10~ **Sale!** 1.50"
set_markdown(jams, 3, 2)
#>                          Type                     Price  
#>                          Strawberry                1.90  
#>                          Raspberry    ~2.10~ Sale! 1.50  
#>                                                          
#>                          Plum                      1.80  
#> 
#> Column names: Type, Price
```
