# Create Markdown representing a huxtable

Create Markdown representing a huxtable

## Usage

``` r
print_md(ht, ...)

to_md(ht, header = TRUE, min_width = getOption("width")/4, max_width = 80, ...)
```

## Arguments

- ht:

  A huxtable.

- ...:

  Arguments passed to methods.

- header:

  Logical. Print the first row as a header?

- min_width:

  Minimum width in on-screen characters of the result.

- max_width:

  Maximum width in on-screen characters of the result. Overrides
  `min_width`.

## Value

`to_md()` returns a string. `print_md()` prints the string and returns
`NULL`.

## Details

Only `align` and `caption` properties are used. The markdown format is
`multiline_tables`, see the [pandoc
documentation](https://pandoc.org/MANUAL.html#pandocs-markdown).

## See also

Other printing functions: [`print_html()`](to_html.md),
[`print_latex()`](to_latex.md), [`print_rtf()`](to_rtf.md),
[`print_screen()`](to_screen.md), [`print_typst()`](to_typst.md)

## Examples

``` r
print_md(jams)
#> -----------------------
#>  Type            Price 
#> ----------- -----------
#>  Strawberry       1.90 
#>                        
#>  Raspberry        2.10 
#>                        
#>  Plum             1.80 
#> -----------------------
#> 
```
