# Escape or unescape text in cells

Setting `escape_contents` to `FALSE` allows you to include raw HTML or
TeX code in your cells.

## Usage

``` r
escape_contents(ht)

escape_contents(ht) <- value

set_escape_contents(ht, row, col, value)

map_escape_contents(ht, row, col, fn)
```

## Arguments

- ht:

  A huxtable.

- value:

  A logical vector or matrix. Set to `NA` to reset to the default, which
  is `TRUE`.

- row:

  A row specifier. See [rowspecs](rowspecs.md) for details.

- col:

  An optional column specifier.

- fn:

  A mapping function. See [mapping-functions](mapping-functions.md) for
  details.

## Details

If [`markdown()`](markdown.md) is `TRUE` for a cell, the
`escape_contents` property will be ignored.

## See also

[`sanitize()`](sanitize.md) for escaping text manually.

## Examples

``` r
ht <- huxtable(
  Text   = "x squared",
  Maths  = "$x^2$"
)
ht <- set_escape_contents(ht, FALSE)
if (FALSE) { # \dontrun{
quick_pdf(ht)
} # }
```
