# Mark rows or columns as headers

Arbitrary rows and columns can be headers: they do not have to be at the
top or left of the table.

## Usage

``` r
header_cols(ht)

header_cols(ht) <- value

set_header_cols(ht, col, value)

header_rows(ht)

header_rows(ht) <- value

set_header_rows(ht, row, value)
```

## Arguments

- ht:

  A huxtable.

- value:

  Logical vector. Set to `NA` to reset to the default, which is FALSE.

- col:

  A column specifier. See [rowspecs](rowspecs.md) for details.

- row:

  A row specifier. See [rowspecs](rowspecs.md) for details.

## Details

By default header rows and columns are not shown differently from other
rows, but you can change this with
[`style_headers()`](style-functions.md). Various themes may set
properties on headers. Lastly, headers are treated differently when
[restacking](restack-across-down.md).

## Examples

``` r
jams <- set_header_rows(jams, 1, TRUE)
jams <- set_header_cols(jams, 1, TRUE)
style_headers(jams,
  bold       = TRUE,
  text_color = "purple"
)
#>                               Type           Price  
#>                               Strawberry      1.90  
#>                               Raspberry       2.10  
#>                               Plum            1.80  
#> 
#> Column names: Type, Price
```
