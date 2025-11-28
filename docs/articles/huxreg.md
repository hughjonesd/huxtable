# Regression Tables with huxreg

## Regression tables with `huxreg`

Huxtable includes the function `huxreg` to build a table of regressions.

You call `huxreg` with a list of models. These models can be of any
class which has a `tidy` method defined in the
[broom](https://cran.r-project.org/?package=broom) package. The method
should return a list of regression coefficients with names `term`,
`estimate`, `std.error` and `p.value`. That covers most standard
regression packages.

Let’s start by running some regressions to predict a diamond’s price.

``` r
data(diamonds, package = "ggplot2")
diamonds <- diamonds[1:100,]

lm1 <- lm(price ~ carat + depth, diamonds)
lm2 <- lm(price ~ depth + factor(color, ordered = FALSE), diamonds)
lm3 <- lm(log(price) ~ carat + depth, diamonds)
```

Now, we use `huxreg` to display the regression output side by side.

``` r
huxreg(lm1, lm2, lm3)
```

[TABLE]

The basic output includes estimates, standard errors and summary
statistics.

Some of those variable names are hard to read. We can change them by
providing a named vector of variables in the `coefs` argument.

``` r
color_names <- grep("factor", names(coef(lm2)), value = TRUE)
names(color_names) <- gsub(".*)(.)", "Color: \\1", color_names)

huxreg(lm1, lm2, lm3, coefs = c("Carat" = "carat", "Depth" = "depth", color_names))
```

[TABLE]

Or, since the output from `huxreg` is just a huxtable, we could just
edit its contents directly.

``` r
diamond_regs <- huxreg(lm1, lm2, lm3)
diamond_regs[seq(8, 18, 2), 1] <- paste("Color:", LETTERS[5:10])

# prints the same as above
```

Of course, we aren’t limited to just changing names. We can also make
our table prettier. Let’s put our footnote in italic, add a caption, and
highlight the cell background of significant coefficients. All of these
are just standard huxtable commands.

``` r
suppressPackageStartupMessages(library(dplyr))

diamond_regs |> 
      map_background_color(-1, -1, by_regex(
        "\\*" = "yellow"
      )) |> 
      set_italic(final(1), 1) |> 
      set_caption("Linear regressions of diamond prices")
```

[TABLE]

Linear regressions of diamond prices

By default, standard errors are shown below coefficient estimates. To
display them in a column to the right, use `error_pos = "right"`:

``` r
huxreg(lm1, lm3, error_pos = "right")
```

[TABLE]

This will give column headings a column span of 2.

To display standard errors in the same cell as estimates, use
`error_pos = "same"`:

``` r
huxreg(lm1, lm3, error_pos = "same")
```

[TABLE]

You can change the default column headings by naming the model
arguments:

``` r
huxreg("Price" = lm1, "Log price" = lm3)
```

[TABLE]

To display a particular row of summary statistics, use the `statistics`
parameter. This should be a character vector. Valid values are anything
returned from your models by
[`broom::glance`](https://generics.r-lib.org/reference/glance.html):

``` r
gl <- as_hux(broom::glance(lm1))

gl |> 
      restack_down(cols = 3, on_remainder = "fill") |> 
      set_bold(odds, everywhere)
```

|   r.squared | adj.r.squared |      sigma |
|------------:|--------------:|-----------:|
|    0.912    |      0.91     | 211        |
|   statistic |       p.value |         df |
|  504        |      5.65e-52 |   2        |
|      logLik |           AIC |        BIC |
| -676        |      1.36e+03 |   1.37e+03 |
|    deviance |   df.residual |       nobs |
|    4.33e+06 |            97 |        100 |

Another value you can use is `"nobs"`, which returns the number of
observations from the regression. If the `statistics` vector has names,
these will be used for row headings:

``` r
huxreg(lm1, lm3, statistics = c("N. obs." = "nobs", 
      "R squared" = "r.squared", "F statistic" = "statistic",
      "P value" = "p.value"))
```

[TABLE]

By default, `huxreg` displays significance stars. You can alter the
symbols used and significance levels with the `stars` parameter, or set
`stars = NULL` to turn off significance stars completely.

``` r
huxreg(lm1, lm3, stars = c(`*` = 0.1, `**` = 0.05, `***` = 0.01)) # a little boastful?
```

[TABLE]

You aren’t limited to displaying standard errors of the estimates. If
you prefer, you can display t statistics or p values, using the
`error_format` option. Any column from `tidy` can be used by putting it
in curly brackets:

``` r
# Another useful column: p.value
huxreg(
        lm1, lm3, 
        error_format = "[{statistic}]", 
        note         = "{stars}. T statistics in brackets."
      )
```

[TABLE]

Here we also changed the footnote, using `note`. If `note` contains the
string `"{stars}"` it will be replaced by a description of the
significance stars used. If you don’t want a footnote, just set
`note = NULL`.

Alternatively, you can display confidence intervals. Use `ci_level` to
set the confidence level for the interval, then use `{conf.low}` and
`{conf.high}` in `error_format`:

``` r
huxreg(lm1, lm3, ci_level = .99, error_format = "({conf.low} -- {conf.high})")
```

[TABLE]

To change number formatting, set the `number_format` parameter. This
works the same as the `number_format` property for a huxtable - if it is
numeric, numbers will be rounded to that many decimal places; if it is
character, it will be taken as a format to the base R `sprintf`
function. `huxreg` tries to be smart and to format summary statistics
like `nobs` as integers.

``` r
huxreg(lm1, lm3, number_format = 2)
```

[TABLE]

Lastly, if you want to bold all significant coefficients, set the
parameter `bold_signif` to a maximum significance level:

``` r
huxreg(lm1, lm3, bold_signif = 0.05)
```

[TABLE]

## Altering data

Sometimes, you want to report different statistics for a model. For
example, you might want to use robust standard errors.

One way to do this is to pass a `tidy`-able test object into `huxreg`.
The function `coeftest` in the “lmtest” package has `tidy` methods
defined:

``` r
library(lmtest)
```

    ## Loading required package: zoo

    ## 
    ## Attaching package: 'zoo'

    ## The following objects are masked from 'package:base':
    ## 
    ##     as.Date, as.Date.numeric

``` r
library(sandwich)
lm_robust <- coeftest(lm1, vcov = vcovHC, save = TRUE)
huxreg("Normal SEs" = lm1, "Robust SEs" = lm_robust)
```

[TABLE]

If that is not possible, you can compute statistics yourself and add
them to your model using the `tidy_override` function:

``` r
lm_fixed <- tidy_override(lm1, p.value = c(0.5, 0.2, 0.06))
huxreg("Normal p values" = lm1, "Supplied p values" = lm_fixed)
```

[TABLE]

You can override any statistics returned by `tidy` or `glance`.

If you want to completely replace the output of tidy, use the
[`tidy_replace()`](../reference/tidy_override.md) function. For example,
here’s how to print different coefficients for a multinomial model.

``` r
mnl <- nnet::multinom(gear ~ mpg, mtcars)
```

    ## # weights:  9 (4 variable)
    ## initial  value 35.155593 
    ## iter  10 value 23.131901
    ## final  value 23.129234 
    ## converged

``` r
tidied <- broom::tidy(mnl)
models <- list()
models[["4 gears"]] <- tidy_replace(mnl, tidied[tidied$y.level == 4, ])
models[["5 gears"]] <- tidy_replace(mnl, tidied[tidied$y.level == 5, ])
huxreg(models, statistics = "AIC")
```

[TABLE]
