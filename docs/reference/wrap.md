# Wrap cell content over multiple lines

Text wrapping only works when the table [`width()`](width.md) has been
set. In particular, if you want to insert newlines in cells, then you
should set a value for [`width()`](width.md) and set `wrap` to `TRUE`.

## Usage

``` r
wrap(ht)

wrap(ht) <- value

set_wrap(ht, row, col, value)

map_wrap(ht, row, col, fn)
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

## Examples

``` r
long_text <- paste(
  rep("Some long text.", 10),
  collapse = " "
)
ht <- huxtable(Long = long_text)
width(ht) <- 0.2
wrap(ht) <- TRUE

if (FALSE) { # \dontrun{
quick_html(ht)
} # }
```
