# Create HTML representing a huxtable

These functions print or return an HTML table. `print_html` also
prepends a `<style>` block defining basic CSS classes.

## Usage

``` r
print_html(ht, ...)

print_notebook(ht, ...)

to_html(ht, ...)

as_html(ht, ...)
```

## Arguments

- ht:

  A huxtable.

- ...:

  Arguments passed to methods. Not currently used.

## Value

`to_html` returns an HTML string. `as_html` wraps `to_html` and returns
an
[`htmltools::HTML`](https://rstudio.github.io/htmltools/reference/HTML.html)
object. `print_html` prints the string and returns `NULL`.

`print_notebook` prints HTML output suitable for use in an RStudio
interactive notebook.

## See also

Other printing functions: [`print_latex()`](to_latex.md),
[`print_md()`](to_md.md), [`print_rtf()`](to_rtf.md),
[`print_screen()`](to_screen.md), [`print_typst()`](to_typst.md)

## Examples

``` r
ht <- hux(a = 1:3, b = letters[1:3])
to_html(ht)
#> [1] "<table class=\"huxtable\" data-quarto-disable-processing=\"true\"  style=\"margin-left: auto; margin-right: auto;\">\n<col><col><thead>\n<tr>\n<th class=\"huxtable-cell huxtable-header\" style=\"text-align: right;  border-style: solid solid solid solid; border-width: 0pt 0pt 0pt 0pt;      font-weight: normal;\">a</th><th class=\"huxtable-cell huxtable-header\" style=\"border-style: solid solid solid solid; border-width: 0pt 0pt 0pt 0pt;      font-weight: normal;\">b</th></tr>\n</thead>\n<tbody>\n<tr>\n<td class=\"huxtable-cell\" style=\"text-align: right;  border-style: solid solid solid solid; border-width: 0pt 0pt 0pt 0pt;\">1</td><td class=\"huxtable-cell\" style=\"border-style: solid solid solid solid; border-width: 0pt 0pt 0pt 0pt;\">a</td></tr>\n<tr>\n<td class=\"huxtable-cell\" style=\"text-align: right;  border-style: solid solid solid solid; border-width: 0pt 0pt 0pt 0pt;\">2</td><td class=\"huxtable-cell\" style=\"border-style: solid solid solid solid; border-width: 0pt 0pt 0pt 0pt;\">b</td></tr>\n<tr>\n<td class=\"huxtable-cell\" style=\"text-align: right;  border-style: solid solid solid solid; border-width: 0pt 0pt 0pt 0pt;\">3</td><td class=\"huxtable-cell\" style=\"border-style: solid solid solid solid; border-width: 0pt 0pt 0pt 0pt;\">c</td></tr>\n</tbody>\n</table>\n"
as_html(ht)
#> <table class="huxtable" data-quarto-disable-processing="true"  style="margin-left: auto; margin-right: auto;">
#> <col><col><thead>
#> <tr>
#> <th class="huxtable-cell huxtable-header" style="text-align: right;  border-style: solid solid solid solid; border-width: 0pt 0pt 0pt 0pt;      font-weight: normal;">a</th><th class="huxtable-cell huxtable-header" style="border-style: solid solid solid solid; border-width: 0pt 0pt 0pt 0pt;      font-weight: normal;">b</th></tr>
#> </thead>
#> <tbody>
#> <tr>
#> <td class="huxtable-cell" style="text-align: right;  border-style: solid solid solid solid; border-width: 0pt 0pt 0pt 0pt;">1</td><td class="huxtable-cell" style="border-style: solid solid solid solid; border-width: 0pt 0pt 0pt 0pt;">a</td></tr>
#> <tr>
#> <td class="huxtable-cell" style="text-align: right;  border-style: solid solid solid solid; border-width: 0pt 0pt 0pt 0pt;">2</td><td class="huxtable-cell" style="border-style: solid solid solid solid; border-width: 0pt 0pt 0pt 0pt;">b</td></tr>
#> <tr>
#> <td class="huxtable-cell" style="text-align: right;  border-style: solid solid solid solid; border-width: 0pt 0pt 0pt 0pt;">3</td><td class="huxtable-cell" style="border-style: solid solid solid solid; border-width: 0pt 0pt 0pt 0pt;">c</td></tr>
#> </tbody>
#> </table>
#> 
```
