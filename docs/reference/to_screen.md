# Print a huxtable on screen

Print a huxtable on screen

## Usage

``` r
print_screen(ht, ...)

to_screen(
  ht,
  min_width = ceiling(getOption("width")/6),
  max_width = getOption("width", Inf),
  compact = TRUE,
  colnames = TRUE,
  color = getOption("huxtable.color_screen", default = TRUE),
  ...
)
```

## Arguments

- ht:

  A huxtable.

- ...:

  Passed on to `to_screen`.

- min_width:

  Minimum width in on-screen characters of the result.

- max_width:

  Maximum width in on-screen characters of the result. Overrides
  `min_width`.

- compact:

  Logical. To save space, don't print lines for empty horizontal
  borders.

- colnames:

  Logical. Whether or not to print colum names.

- color:

  Logical. Whether to print the huxtable in color (requires the `crayon`
  package).

## Value

`to_screen` returns a string. `print_screen` prints the string and
returns `NULL`.

## Details

Screen display shows the following features:

- Table and caption positioning

- Merged cells

- Cell alignment

- Borders

- Cell background and border color (if the "crayon" package is
  installed)

- Text color, bold and italic (if the "crayon" package is installed)

Cell padding, widths and heights are not shown.

## See also

Other printing functions: [`print_html()`](to_html.md),
[`print_latex()`](to_latex.md), [`print_md()`](to_md.md),
[`print_rtf()`](to_rtf.md), [`print_typst()`](to_typst.md)

## Examples

``` r
bottom_border(jams)[1, 1:2] <- 1
bold(jams)[1, 1:2] <- TRUE
jams <- map_text_color(
  jams,
  by_regex("berry" = "red")
)

print_screen(jams)
#>                               Type           Price  
#>                             ────────────────────────
#>                               Strawberry      1.90  
#>                               Raspberry       2.10  
#>                               Plum            1.80  
#> 
#> Column names: Type, Price
```
