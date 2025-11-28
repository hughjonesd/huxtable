# Print a huxtable within knitr

Print a huxtable within knitr

## Usage

``` r
knit_print.huxtable(x, options, ...)
```

## Arguments

- x:

  A huxtable.

- options:

  Not used.

- ...:

  Not used.

## Details

knitr calls
[`knitr::knit_print()`](https://rdrr.io/pkg/knitr/man/knit_print.html)
on objects when they are printed in a knitr (or RMarkdown) document. The
method for `huxtable` objects guesses the appropriate output format
(including Typst documents when using the `typst` package) and prints
itself out appropriately. You can override the output format by setting
`options("huxtable.knitr_output_format")`.

## See also

[huxtable-options](huxtable-options.md)

Other knit_print: [`knit_print.data.frame()`](knit_print.data.frame.md)
