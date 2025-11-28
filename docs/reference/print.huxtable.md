# Format and print huxtables using a default method

By default huxtables are printed using [`print_screen()`](to_screen.md).
In certain cases, for example in Sweave documents, it may be useful to
change this. You can do so by setting `options("huxtable.print")`.

## Usage

``` r
# S3 method for class 'huxtable'
print(x, ...)

# S3 method for class 'huxtable'
format(x, ..., output = c("latex", "html", "md", "screen", "rtf", "typst"))
```

## Arguments

- x:

  A huxtable.

- ...:

  Options passed to other methods.

- output:

  Output format. One of `"html"`, `"latex"`, `"md"`, `"screen"`, `"rtf"`
  or `"typst"`.

## Value

`print` prints the huxtable and returns `NULL` invisibly.

`format` returns a string representation from
[`to_latex()`](to_latex.md), [`to_html()`](to_html.md) etc.

## See also

To change how huxtables are printed within `knitr`, see
`options("huxtable.knitr_output_format")` in
[huxtable-options](huxtable-options.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# to print LaTeX output:
options(huxtable.print = print_latex)
# to print Typst output:
options(huxtable.print = print_typst)
} # }

format(jams, output = "screen")
#> [1] "                              Type           Price  \n                              Strawberry      1.90  \n                              Raspberry       2.10  \n                              Plum            1.80  \n\nColumn names: Type, Price\n"
format(jams, output = "md")
#> [1] "-----------------------\n Type            Price \n----------- -----------\n Strawberry       1.90 \n                       \n Raspberry        2.10 \n                       \n Plum             1.80 \n-----------------------\n\n"
format(jams, output = "typst")
#> [1] "#figure(\ntable(\n  columns: (auto, auto),\n  stroke: none,\n  table.header(\n    table.cell(align: (left + top))[Type], table.cell(align: (right + top))[Price]\n  ),\n  table.cell(align: (left + top))[Strawberry], table.cell(align: (right + top))[1.90],\n  table.cell(align: (left + top))[Raspberry], table.cell(align: (right + top))[2.10],\n  table.cell(align: (left + top))[Plum], table.cell(align: (right + top))[1.80]\n),\ncaption: none\n)"
```
