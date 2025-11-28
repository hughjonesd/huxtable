# Create a huxtable

`huxtable`, or `hux`, creates a huxtable object.

## Usage

``` r
huxtable(
  ...,
  add_colnames = getOption("huxtable.add_colnames", TRUE),
  add_rownames = FALSE,
  autoformat = getOption("huxtable.autoformat", TRUE)
)

hux(
  ...,
  add_colnames = getOption("huxtable.add_colnames", TRUE),
  add_rownames = FALSE,
  autoformat = getOption("huxtable.autoformat", TRUE)
)

tribble_hux(
  ...,
  add_colnames = getOption("huxtable.add_colnames", TRUE),
  autoformat = getOption("huxtable.autoformat", TRUE)
)
```

## Arguments

- ...:

  For `huxtable`, named list of values as in
  [`data.frame()`](https://rdrr.io/r/base/data.frame.html). For
  `tribble_hux`, data values as in
  [`tibble::tribble()`](https://tibble.tidyverse.org/reference/tribble.html).

- add_colnames:

  If `TRUE`, add a first row of column names to the huxtable.

- add_rownames:

  If `TRUE` or a character string, add a first column of row names to
  the huxtable. The string gives the name for the new column (or
  `"rownames"` for `TRUE`).

- autoformat:

  If `TRUE`, automatically format columns by type. See below.

## Value

An object of class `huxtable`.

## Details

If you use `add_colnames` or `add_rownames`, be aware that these will
shift your rows and columns along by one: your old row/column 1 will now
be row/column 2, etc.

`add_colnames` defaults to `TRUE`. You can set the default globally by
setting `options("huxtable.add_colnames")` to `TRUE` or `FALSE`.

`tribble_hux` is a simple wrapper around
[`tibble::tribble()`](https://tibble.tidyverse.org/reference/tribble.html)
which lets you create data in a readable format. It requires the
"tibble" package to be installed.

## Automatic formatting

If `autoformat` is `TRUE`, then columns will have
[`number_format()`](number_format.md) and [`align()`](align.md)
properties set automatically, as follows:

- Integer columns will have `number_format` set to 0.

- Other numeric columns will have `number_format` set to `"%.3g"`.

- All other columns will have `number_format` set to `NA` (no
  formatting).

- Integer, `Date` and date-time (i.e. `POSIXct` and `POSIXlt`) columns
  will be right-aligned.

- Other numeric columns will be aligned on `options("OutDec")`, usually
  `"."`.

- Other columns will be left aligned.

You can change these defaults by editing
`options("huxtable.autoformat_number_format")` and
`options("huxtable.autoformat_align")`. See
[huxtable-package](huxtable-package.md) for more details.

Automatic alignment also applies to column headers if `add_colnames` is
`TRUE`; headers of columns aligned on a decimal point will be
right-aligned. Automatic number formatting does not apply to column
headers.

## See also

[huxtable-options](huxtable-options.md)

## Examples

``` r
ht <- huxtable(
  column1 = 1:5,
  column2 = letters[1:5]
)
ht
#>                                column1   column2  
#>                                      1   a        
#>                                      2   b        
#>                                      3   c        
#>                                      4   d        
#>                                      5   e        
#> 
#> Column names: column1, column2
tribble_hux(
  ~Name, ~Salary,
  "John Smith", 50000,
  "Jane Doe", 50000,
  "David Hugh-Jones", 50000,
  add_colnames = TRUE
)
#>                            Name                Salary  
#>                            John Smith           5e+04  
#>                            Jane Doe             5e+04  
#>                            David Hugh-Jones     5e+04  
#> 
#> Column names: Name, Salary
```
