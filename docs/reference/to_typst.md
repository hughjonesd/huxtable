# Create Typst markup representing a huxtable

These functions print or return a Typst table.

## Usage

``` r
print_typst(ht, ...)

to_typst(ht, ...)
```

## Arguments

- ht:

  A huxtable.

- ...:

  Arguments passed to methods. Not currently used.

## Value

`to_typst` returns a Typst string. `print_typst` prints the string and
returns `NULL`.

## See also

Other printing functions: [`print_html()`](to_html.md),
[`print_latex()`](to_latex.md), [`print_md()`](to_md.md),
[`print_rtf()`](to_rtf.md), [`print_screen()`](to_screen.md)

## Examples

``` r
ht <- huxtable(a = 1:3, b = letters[1:3])
to_typst(ht)
#> [1] "#figure(\ntable(\n  columns: (auto, auto),\n  stroke: none,\n  table.header(\n    table.cell(align: (right + top))[a], table.cell(align: (left + top))[b]\n  ),\n  table.cell(align: (right + top))[1], table.cell(align: (left + top))[a],\n  table.cell(align: (right + top))[2], table.cell(align: (left + top))[b],\n  table.cell(align: (right + top))[3], table.cell(align: (left + top))[c]\n),\ncaption: none\n)"
```
