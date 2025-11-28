# Set multiple properties on headers

These functions set arbitrary cell properties on cells in header rows
and/or columns.

## Usage

``` r
style_headers(ht, ...)

style_header_rows(ht, ...)

style_header_cols(ht, ...)

style_cells(ht, row, col, ...)

set_cell_properties(ht, row, col, ...)
```

## Arguments

- ht:

  A huxtable.

- ...:

  Named list of cell properties.

- row:

  A row specifier. See [rowspecs](rowspecs.md) for details.

- col:

  An optional column specifier.

## Details

- `style_headers` sets properties on all header cells.

- `style_header_rows` sets properties on header rows.

- `style_header_cols` sets properties on header columns.

- `style_cells` sets properties on all selected cells.

`set_cell_properties` is a deprecated alias for `style_cells`. Don't use
it.

## Examples

``` r
style_headers(jams, text_color = "red")
#>                               Type           Price  
#>                               Strawberry      1.90  
#>                               Raspberry       2.10  
#>                               Plum            1.80  
#> 
#> Column names: Type, Price
jams <- set_header_cols(jams, 1, TRUE)
style_header_cols(jams,
  text_color = c(
    NA, "red",
    "darkred", "purple"
  )
)
#>                               Type           Price  
#>                               Strawberry      1.90  
#>                               Raspberry       2.10  
#>                               Plum            1.80  
#> 
#> Column names: Type, Price

style_cells(jams, everywhere, 2, bold = TRUE)
#>                               Type           Price  
#>                               Strawberry      1.90  
#>                               Raspberry       2.10  
#>                               Plum            1.80  
#> 
#> Column names: Type, Price
```
