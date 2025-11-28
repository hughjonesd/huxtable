# Create a huxtable to display model output

Create a huxtable to display model output

## Usage

``` r
huxreg(
  ...,
  error_format = "({std.error})",
  error_pos = c("below", "same", "right"),
  number_format = "%.3f",
  align = ".",
  ci_level = NULL,
  tidy_args = NULL,
  glance_args = NULL,
  stars = c(`***` = 0.001, `**` = 0.01, `*` = 0.05),
  bold_signif = NULL,
  borders = 0.4,
  outer_borders = 0.8,
  note = if (is.null(stars)) NULL else "{stars}.",
  statistics = c(N = "nobs", R2 = "r.squared", "logLik", "AIC"),
  coefs = NULL,
  omit_coefs = NULL
)
```

## Arguments

- ...:

  Models, or a single list of models. Names will be used as column
  headings.

- error_format:

  How to display uncertainty in estimates. See below.

- error_pos:

  Display uncertainty "below", to the "right" of, or in the "same" cell
  as estimates.

- number_format:

  Format for numbering. See [`number_format()`](number_format.md) for
  details.

- align:

  Alignment for table cells. Set to a single character to align on this
  character.

- ci_level:

  Confidence level for intervals. Set to `NULL` to not calculate
  confidence intervals.

- tidy_args:

  List of arguments to pass to
  [`generics::tidy()`](https://generics.r-lib.org/reference/tidy.html).
  A list without names will be treated as a list of argument lists, one
  for each model.

- glance_args:

  List of arguments to pass to
  [`generics::glance()`](https://generics.r-lib.org/reference/glance.html).
  A list without names will be treated as a list of argument lists, one
  for each model.

- stars:

  Levels for p value stars. Names of `stars` are symbols to use. Set to
  `NULL` to not show stars.

- bold_signif:

  Where p values are below this number, cells will be displayed in bold.
  Use `NULL` to turn off this behaviour.

- borders:

  Thickness of inner horizontal borders. Set to 0 for no borders.

- outer_borders:

  Thickness of outer (top and bottom) horizontal borders. Set to 0 for
  no borders.

- note:

  Footnote for bottom cell, which spans all columns. `{stars}` will be
  replaced by a note about significance stars. Set to `NULL` for no
  footnote.

- statistics:

  A vector of summary statistics to display. Set to `NULL` to show all
  available statistics. To change display names, name the `statistics`
  vector: `c("Displayed title" = "statistic_name", ...)`

- coefs:

  A vector of coefficients to display. Overrules `omit_coefs`. To change
  display names, name the `coef` vector:
  `c("Displayed title" = "coefficient_name", ...)`

- omit_coefs:

  Omit these coefficients.

## Value

A huxtable object.

## Details

Models must have a
[`generics::tidy()`](https://generics.r-lib.org/reference/tidy.html)
method defined, which should return "term", "estimate", "std.error",
"statistic" and "p.value". The `"broom"` package provides methods for
many model objects. If the `tidy` method does not have a `conf.int`
option, `huxreg` will calculate confidence intervals itself, using a
normal approximation.

If `...` has names or contains a single named list, the names will be
used for column headings. Otherwise column headings will be
automatically created.

If the `coef` and/or `statistics` vectors have names, these will be used
for row headings. If different values of `coef` have the same name, the
corresponding rows will be merged in the output.

`statistics` should be column names from
[`generics::glance()`](https://generics.r-lib.org/reference/glance.html).
You can also use `"nobs"` for the number of observations. If
`statistics` is `NULL` then all columns from `glance` will be used. To
use no columns, set `statistics = character(0)`.

`error_format` is a string to be interpreted by
[`glue::glue()`](https://glue.tidyverse.org/reference/glue.html). Terms
in parentheses will be replaced by computed values. You can use any
columns returned by `tidy`: typical columns include `statistic`,
`p.value`, `std.error`, as well as `conf.low` and `conf.high` if you
have set `ci_level`. For example, to show confidence intervals, you
could write `error_format = "{conf.low} to {conf.high}"`.

## Fixing p values manually

If you wish to use e.g. robust standard errors, you can pass results
from e.g.
[`lmtest::coeftest()`](https://rdrr.io/pkg/lmtest/man/coeftest.html)
into `huxreg`, since these objects have `tidy` methods. Alternatively,
to manually insert your own statistics, see
[`tidy_override()`](tidy_override.md).

## Examples

``` r
if (!requireNamespace("broom")) {
  stop("Please install 'broom' to run this example.")
}
#> Loading required namespace: broom

lm1 <- lm(mpg ~ cyl, mtcars)
lm2 <- lm(mpg ~ cyl + hp, mtcars)
glm1 <- glm(I(mpg > 20) ~ cyl, mtcars,
  family = binomial
)

huxreg(lm1, lm2, glm1)
#>            ─────────────────────────────────────────────────────────
#>                                (1)           (2)           (3)      
#>                          ───────────────────────────────────────────
#>              (Intercept)    37.885 ***    36.908 ***       64.400   
#>                             (2.074)       (2.191)      (17449.774)  
#>              cyl            -2.876 ***    -2.265 ***      -10.781   
#>                             (0.322)       (0.576)       (2908.296)  
#>              hp                           -0.019                    
#>                                           (0.015)                   
#>                          ───────────────────────────────────────────
#>              N              32            32               32       
#>              R2              0.726         0.741                    
#>              logLik        -81.653       -80.781           -4.780   
#>              AIC           169.306       169.562           13.561   
#>            ─────────────────────────────────────────────────────────
#>              *** p < 0.001; ** p < 0.01; * p < 0.05.                
#> 
#> Column names: names, model1, model2, model3

if (requireNamespace("sandwich") &&
  requireNamespace("lmtest")) {
  lm_robust <- lmtest::coeftest(lm1,
    vcov = sandwich::vcovHC
  )
  # coeftest() has no "glance" method:
  huxreg(lm_robust,
    statistics = character(0)
  )
}
#> Loading required namespace: sandwich
#> Loading required namespace: lmtest
#> Original model not retained as part of coeftest object.
#> ℹ For additional model summary information (r.squared, df, etc.), consider
#>   passing `glance.coeftest()` an object where the underlying model has been
#>   saved, i.e.  `lmtest::coeftest(..., save = TRUE)`.
#> This message is displayed once per session.
#>                 ───────────────────────────────────────────────
#>                                                  (1)           
#>                                        ────────────────────────
#>                   (Intercept)                      37.885 ***  
#>                                                    (2.742)     
#>                   cyl                              -2.876 ***  
#>                                                    (0.389)     
#>                 ───────────────────────────────────────────────
#>                   *** p < 0.001; ** p < 0.01; * p < 0.05.      
#> 
#> Column names: names, model1
```
