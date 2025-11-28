# Change a model's `tidy` output

Use `tidy_override` and `tidy_replace` to provide your own p values,
confidence intervals etc. for a model.

## Usage

``` r
tidy_override(x, ..., glance = list(), extend = FALSE)

tidy_replace(x, tidied, glance = list())

# S3 method for class 'tidy_override'
tidy(x, ...)

# S3 method for class 'tidy_override'
glance(x, ...)

# S3 method for class 'tidy_override'
nobs(object, ...)
```

## Arguments

- x:

  A model with methods defined for
  [`generics::tidy()`](https://generics.r-lib.org/reference/tidy.html)
  and/or
  [`generics::glance()`](https://generics.r-lib.org/reference/glance.html).

- ...:

  In `tidy_override`, columns of statistics to replace `tidy` output. In
  `tidy` and `glance` methods, arguments passed on to the underlying
  model.

- glance:

  A list of summary statistics for `glance`.

- extend:

  Logical: allow adding new columns to `tidy(x)` and `glance(x)`?

- tidied:

  Data frame to replace the result of `tidy(x)`.

- object:

  A `tidy_override` object.

## Value

An object that can be passed in to `huxreg`.

## Details

`tidy_override` allows you to replace some columns of `tidy(x)` with
your own data.

`tidy_replace` allows you to replace the result of `tidy(x)` entirely.

## Examples

``` r
if (!requireNamespace("broom", quietly = TRUE)) {
  stop("Please install 'broom' to run this example.")
}

lm1 <- lm(mpg ~ cyl, mtcars)
fixed_lm1 <- tidy_override(lm1,
  p.value = c(.04, .12),
  glance = list(r.squared = 0.99)
)
huxreg(lm1, fixed_lm1)
#>                ─────────────────────────────────────────────────
#>                                       (1)             (2)       
#>                                ─────────────────────────────────
#>                  (Intercept)        37.885 ***        37.885 *  
#>                                     (2.074)           (2.074)   
#>                  cyl                -2.876 ***        -2.876    
#>                                     (0.322)           (0.322)   
#>                                ─────────────────────────────────
#>                  N                  32                32        
#>                  R2                  0.726             0.990    
#>                  logLik            -81.653           -81.653    
#>                  AIC               169.306           169.306    
#>                ─────────────────────────────────────────────────
#>                  *** p < 0.001; ** p < 0.01; * p < 0.05.        
#> 
#> Column names: names, model1, model2

if (requireNamespace("nnet", quietly = TRUE)) {
  mnl <- nnet::multinom(gear ~ mpg, mtcars)
  tidied <- broom::tidy(mnl)
  mnl4 <- tidy_replace(mnl, tidied[tidied$y.level == 4, ])
  mnl5 <- tidy_replace(mnl, tidied[tidied$y.level == 5, ])
  huxreg(mnl4, mnl5, statistics = "nobs")
}
#> # weights:  9 (4 variable)
#> initial  value 35.155593 
#> iter  10 value 23.131901
#> final  value 23.129234 
#> converged
#>                ─────────────────────────────────────────────────
#>                                       (1)             (2)       
#>                                ─────────────────────────────────
#>                  (Intercept)         -9.502 **        -7.691 *  
#>                                      (3.262)          (3.232)   
#>                  mpg                  0.475 **         0.358 *  
#>                                      (0.168)          (0.168)   
#>                                ─────────────────────────────────
#>                  nobs                32               32        
#>                ─────────────────────────────────────────────────
#>                  *** p < 0.001; ** p < 0.01; * p < 0.05.        
#> 
#> Column names: names, model1, model2
```
