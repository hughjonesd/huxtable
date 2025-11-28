# Set how numbers are formatted in cells

If `number_format` is:

- numeric, numbers will be rounded to that many decimal places;

- character, it will be used as an argument to
  [`sprintf()`](https://rdrr.io/r/base/sprintf.html);

- a function, the function will be applied to the numbers;

- `NA`, then numbers will not be formatted (except by conversion with
  `as.character`).

## Usage

``` r
number_format(ht)

number_format(ht) <- value

set_number_format(ht, row, col, value)

map_number_format(ht, row, col, fn)
```

## Arguments

- ht:

  A huxtable.

- value:

  A character or integer vector, a list containing a function, or `NA`.
  Note that setting to `NA` does not reset to the default.

- row:

  A row specifier. See [rowspecs](rowspecs.md) for details.

- col:

  An optional column specifier.

- fn:

  A mapping function. See [mapping-functions](mapping-functions.md) for
  details.

## Details

Number formatting is applied to any parts of cells that look like
numbers. The exception is exponents in scientific notation; huxtable
attempts to detect and ignore these.

The default value is "%.3g", which rounds numbers if they have more than
3 significant digits, and which may use scientific notation for large
numbers.

Note that if your cells are of type numeric, a number format of `NA`
doesn't guarantee you get back what you typed in, since R's default
conversion may apply scientific notation and rounding.

To set number_format to a function, enclose the function in `list`. The
function should take one argument and return a string.
[`fmt_pretty()`](fmt_pretty.md) and [`fmt_percent()`](fmt_percent.md)
are useful shortcuts for common formatting functions.

## See also

[`fmt_pretty()`](fmt_pretty.md) and
[`fmt_percent()`](fmt_percent.md).`options("huxtable.long_minus")` in
[huxtable-options](huxtable-options.md) for pretty-printing minus signs.

Other formatting functions: [`background_color()`](background_color.md),
[`bold()`](bold.md), [`font()`](font.md), [`font_size()`](font_size.md),
[`na_string()`](na_string.md), [`text_color()`](text_color.md)

## Examples

``` r
ht <- huxtable(
  number_format = c(
    "Default",
    "NA",
    "2",
    "\"%5.2f\"",
    "Pretty",
    "Sign"
  ),
  a = rep(1000, 6),
  b = rep(1000.005, 6),
  c = rep(0.0001, 6),
  d = rep(-1, 6),
  e = rep("3.2 (s.e. 1.4)", 6)
)

number_format(ht)[3, -1] <- NA
number_format(ht)[4, -1] <- 2
number_format(ht)[5, -1] <- "%5.2f"

number_format(ht)[6, -1] <- fmt_pretty()

number_format(ht)[7, -1] <- list(
  function(x) ifelse((x > 0), "+", "-")
)

right_border(ht) <- 1
bottom_border(ht)[1, ] <- 1

ht
#>       number_for │        a │         b │          c │     d │ e          │
#>       mat        │          │           │            │       │            │
#>     ─────────────┼──────────┼───────────┼────────────┼───────┼────────────┤
#>       Default    │ 1e+03    │ 1e+03     │     0.0001 │ -1    │ 3.2 (s.e.  │
#>                  │          │           │            │       │ 1.4)       │
#>       NA         │  1000    │  1000.005 │ 1e-04      │ -1    │ 3.2 (s.e.  │
#>                  │          │           │            │       │ 1.4)       │
#>       2          │  1000.00 │  1000.00  │     0.00   │ -1.00 │ 3.20 (s.e. │
#>                  │          │           │            │       │ 1.40)      │
#>       "%5.2f"    │  1000.00 │  1000.00  │     0.00   │ -1.00 │ 3.20 (s.e. │
#>                  │          │           │            │       │ 1.40)      │
#>       Pretty     │ 1,000    │ 1,000.005 │     0.0001 │ -1    │ 3.2 (s.e.  │
#>                  │          │           │            │       │ 1.4)       │
#>       Sign       │     +    │     +     │     +      │  -    │ + (s.e. +) │
#> 
#> Column names: number_format, a, b, c, d, e

ht_bands <- huxtable("10000 Maniacs", autoformat = FALSE)
# probably not what you want:
ht_bands
#>                                  1e+04 Maniacs   
# fixed:
set_number_format(ht_bands, NA)
#>                                  10000 Maniacs   
```
