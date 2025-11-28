# Escape text for various formats

This escapes a string for LaTeX, HTML, Typst or RTF.

## Usage

``` r
sanitize(str, type = c("latex", "html", "typst", "rtf"))
```

## Arguments

- str:

  A character object.

- type:

  `"latex"`, `"html"`, `"typst"` or `"rtf"`.

## Value

The sanitized character object.

## Details

HTML and LaTeX code was copied over from
[`xtable::sanitize()`](https://rdrr.io/pkg/xtable/man/sanitize.html).

## Examples

``` r
txt <- "Make $$$ with us"
sanitize(txt, type = "latex")
#> [1] "Make \\$\\$\\$ with us"
```
