# Print data frames in knitr using huxtable

Print data frames in knitr using huxtable

## Usage

``` r
knit_print.data.frame(x, options, ...)
```

## Arguments

- x:

  A huxtable.

- options:

  Not used.

- ...:

  Not used.

## Details

`huxtable` defines a `knit_print` method for `data.frame`s. This
converts the data frame to a huxtable, with `add_colnames = TRUE`,
themes it using [`theme_plain()`](themes.md) and prints it. It also
tries to set a few intelligent defaults, e.g. wrapping long columns and
setting an appropriate width. To turn this behaviour off, set
`options(huxtable.knit_print_df = FALSE)`. To change the theme, set
`options("huxtable.knit_print_df_theme")` to a one-argument function
which should return the huxtable.

## See also

[huxtable-options](huxtable-options.md)

Other knit_print: [`knit_print.huxtable()`](knit_print.huxtable.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# in your knitr document
mytheme <- function(ht) {
  ht <- set_all_borders(ht, 0.4)
  ht <- set_all_border_colors(
    ht,
    "darkgreen"
  )
  ht <- set_background_color(
    ht,
    evens, odds, "salmon"
  )
  ht
}

options(
  huxtable.knit_print_df_theme = mytheme
)
# groovy!
data.frame(
  a = 1:5,
  b = 1:5
)
} # }
```
