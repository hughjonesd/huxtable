# Guess knitr output format

Convenience function which tries to guess the ultimate output from knitr
and rmarkdown.

## Usage

``` r
guess_knitr_output_format()
```

## Value

"html", "latex", "typst", or something else. If we are not in a knitr
document, returns an empty string.

## Examples

``` r
if (FALSE) { # \dontrun{
# in a knitr document
guess_knitr_output_format()
} # }
```
