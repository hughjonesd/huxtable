# Set cell contents, interpreting them as markdown

This convenience function calls [`set_contents()`](set_contents.md) and
[`set_markdown()`](markdown.md).

## Usage

``` r
set_markdown_contents(ht, row, col, value)
```

## Arguments

- ht:

  A huxtable.

- row:

  A row specifier. See [rowspecs](rowspecs.md) for details.

- col:

  An optional column specifier.

- value:

  Cell contents, as a markdown string.

## Value

The modified huxtable.

## Note

Markdown content in cells is completely separate from printing the whole
table as markdown using [`print_md()`](to_md.md). When you set
`markdown` to `TRUE`, huxtable itself interprets the cell contents as
markdown, and spits out HTML, TeX or whatever.

## See also

[`markdown()`](markdown.md).

## Examples

``` r
set_markdown_contents(
  jams, 1, 1,
  "**Type** of jam"
)
#>                              Type of jam     Price  
#>                              Strawberry       1.90  
#>                              Raspberry        2.10  
#>                              Plum             1.80  
#> 
#> Column names: Type, Price
```
