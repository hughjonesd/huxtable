# Create LaTeX representing a huxtable

Create LaTeX representing a huxtable

## Usage

``` r
print_latex(ht, ...)

to_latex(ht, tabular_only = FALSE, ...)
```

## Arguments

- ht:

  A huxtable.

- ...:

  Arguments passed to methods.

- tabular_only:

  Return only the LaTeX tabular, not the surrounding float.

## Value

`to_latex` returns a string. `print_latex` prints the string and returns
`NULL`.

## Details

If we appear to be in a rmarkdown document with the Pandoc markdown
`+raw_attribute` extension available, `to_latex` will return LaTeX
surrounded by a "raw attribute code block" (see
https://pandoc.org/MANUAL.html#extension-raw_attribute). This helps
protect against pandoc accidentally escaping the TeX code.

## See also

Other printing functions: [`print_html()`](to_html.md),
[`print_md()`](to_md.md), [`print_rtf()`](to_rtf.md),
[`print_screen()`](to_screen.md), [`print_typst()`](to_typst.md)

## Examples

``` r
ht <- huxtable(
  a = 1:3,
  b = letters[1:3]
)
print_latex(ht)
#> 
#>   \providecommand{\huxb}[2]{\arrayrulecolor[RGB]{#1}\global\arrayrulewidth=#2pt}
#>   \providecommand{\huxvb}[2]{\color[RGB]{#1}\vrule width #2pt}
#>   \providecommand{\huxtpad}[1]{\rule{0pt}{#1}}
#>   \providecommand{\huxbpad}[1]{\rule[-#1]{0pt}{#1}}
#> 
#> \begin{table}[ht]
#> \begin{centerbox}
#> \begin{threeparttable}
#>  \setlength{\tabcolsep}{0pt}
#> \begin{tabular}{l l}
#> 
#> 
#> \hhline{}
#> \arrayrulecolor{black}
#> 
#> \multicolumn{1}{!{\huxvb{0, 0, 0}{0}}r!{\huxvb{0, 0, 0}{0}}}{\huxtpad{6pt + 1em}\raggedleft \hspace{6pt} a \hspace{6pt}\huxbpad{6pt}} &
#> \multicolumn{1}{l!{\huxvb{0, 0, 0}{0}}}{\huxtpad{6pt + 1em}\raggedright \hspace{6pt} b \hspace{6pt}\huxbpad{6pt}} \tabularnewline[-0.5pt]
#> 
#> 
#> \hhline{}
#> \arrayrulecolor{black}
#> 
#> \multicolumn{1}{!{\huxvb{0, 0, 0}{0}}r!{\huxvb{0, 0, 0}{0}}}{\huxtpad{6pt + 1em}\raggedleft \hspace{6pt} 1 \hspace{6pt}\huxbpad{6pt}} &
#> \multicolumn{1}{l!{\huxvb{0, 0, 0}{0}}}{\huxtpad{6pt + 1em}\raggedright \hspace{6pt} a \hspace{6pt}\huxbpad{6pt}} \tabularnewline[-0.5pt]
#> 
#> 
#> \hhline{}
#> \arrayrulecolor{black}
#> 
#> \multicolumn{1}{!{\huxvb{0, 0, 0}{0}}r!{\huxvb{0, 0, 0}{0}}}{\huxtpad{6pt + 1em}\raggedleft \hspace{6pt} 2 \hspace{6pt}\huxbpad{6pt}} &
#> \multicolumn{1}{l!{\huxvb{0, 0, 0}{0}}}{\huxtpad{6pt + 1em}\raggedright \hspace{6pt} b \hspace{6pt}\huxbpad{6pt}} \tabularnewline[-0.5pt]
#> 
#> 
#> \hhline{}
#> \arrayrulecolor{black}
#> 
#> \multicolumn{1}{!{\huxvb{0, 0, 0}{0}}r!{\huxvb{0, 0, 0}{0}}}{\huxtpad{6pt + 1em}\raggedleft \hspace{6pt} 3 \hspace{6pt}\huxbpad{6pt}} &
#> \multicolumn{1}{l!{\huxvb{0, 0, 0}{0}}}{\huxtpad{6pt + 1em}\raggedright \hspace{6pt} c \hspace{6pt}\huxbpad{6pt}} \tabularnewline[-0.5pt]
#> 
#> 
#> \hhline{}
#> \arrayrulecolor{black}
#> \end{tabular}
#> \end{threeparttable}\par\end{centerbox}
#> 
#> \end{table}
```
