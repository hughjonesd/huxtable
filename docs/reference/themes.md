# Theme a huxtable

These functions quickly set default styles for a huxtable.

## Usage

``` r
theme_plain(ht, header_rows = TRUE, position = "center")

theme_bright(
  ht,
  header_rows = TRUE,
  header_cols = FALSE,
  colors = c("#7eabf2", "#e376e3", "#fcbb03", "#7aba59", "#fc0356")
)

theme_basic(ht, header_rows = TRUE, header_cols = FALSE)

theme_compact(ht, header_rows = TRUE, header_cols = FALSE)

theme_striped(
  ht,
  stripe = "grey90",
  stripe2 = "grey95",
  header_rows = TRUE,
  header_cols = TRUE
)

theme_grey(ht, header_rows = TRUE, header_cols = TRUE)

theme_blue(ht, header_rows = TRUE, header_cols = TRUE)

theme_orange(ht, header_rows = TRUE, header_cols = TRUE)

theme_green(ht, header_rows = TRUE, header_cols = TRUE)

theme_article(ht, header_rows = TRUE, header_cols = TRUE)

theme_mondrian(ht, prop_colored = 0.1, font = NULL)
```

## Arguments

- ht:

  A huxtable object.

- header_rows:

  Logical: style header rows?

- position:

  "left", "center" or "right"

- header_cols:

  Logical: style header columns?

- colors:

  Colors for header rows. Can also be a palette function.

- stripe:

  Background colour for odd rows

- stripe2:

  Background colour for even rows

- prop_colored:

  Roughly what proportion of cells should have a primary-color
  background?

- font:

  Font to use. For LaTeX, try `"cmss"`.

## Value

The huxtable object, appropriately styled.

## Details

- `theme_plain` is a simple theme with a bold header, a grey striped
  background, and an outer border.

- `theme_basic` sets header rows/columns to bold, and adds a border
  beneath them.

- `theme_compact` is like `theme_basic` but with minimal padding.

- `theme_striped` uses different backgrounds for alternate rows, and for
  headers.

- `theme_article` is similar to the style of many scientific journals.
  It sets horizontal lines above and below the table.

- `theme_bright` uses thick white borders and a colourful header. It
  works nicely with sans-serif fonts.

- `theme_grey`, `theme_blue`, `theme_orange` and `theme_green` use white
  borders and subtle horizontal stripes.

- `theme_mondrian` mimics the style of a Mondrian painting, with thick
  black borders and randomized colors.

## Examples

``` r
theme_plain(jams)
#>                             ┌──────────────────────┐
#>                             │ Type           Price │
#>                             ├──────────────────────┤
#>                             │ Strawberry      1.90 │
#>                             │ Raspberry       2.10 │
#>                             │ Plum            1.80 │
#>                             └──────────────────────┘
#> 
#> Column names: Type, Price
theme_basic(jams)
#>                               Type           Price  
#>                             ────────────────────────
#>                               Strawberry      1.90  
#>                               Raspberry       2.10  
#>                               Plum            1.80  
#> 
#> Column names: Type, Price
theme_compact(jams)
#>                               Type           Price  
#>                             ────────────────────────
#>                               Strawberry      1.90  
#>                               Raspberry       2.10  
#>                               Plum            1.80  
#> 
#> Column names: Type, Price
theme_striped(jams)
#>                             ┌────────────┬─────────┐
#>                             │ Type       │   Price │
#>                             ├────────────┼─────────┤
#>                             │ Strawberry │    1.90 │
#>                             ├────────────┼─────────┤
#>                             │ Raspberry  │    2.10 │
#>                             ├────────────┼─────────┤
#>                             │ Plum       │    1.80 │
#>                             └────────────┴─────────┘
#> 
#> Column names: Type, Price
theme_article(jams)
#>                             ────────────────────────
#>                               Type           Price  
#>                             ────────────────────────
#>                               Strawberry      1.90  
#>                               Raspberry       2.10  
#>                               Plum            1.80  
#>                             ────────────────────────
#> 
#> Column names: Type, Price
theme_bright(jams)
#>                             ┌────────────┬─────────┐
#>                             │ Type       │   Price │
#>                             ├────────────┼─────────┤
#>                             │ Strawberry │    1.90 │
#>                             ├────────────┼─────────┤
#>                             │ Raspberry  │    2.10 │
#>                             ├────────────┼─────────┤
#>                             │ Plum       │    1.80 │
#>                             └────────────┴─────────┘
#> 
#> Column names: Type, Price
theme_grey(jams)
#>                             ┌────────────┬─────────┐
#>                             │ Type       │   Price │
#>                             ├────────────┼─────────┤
#>                             │ Strawberry │    1.90 │
#>                             ├────────────┼─────────┤
#>                             │ Raspberry  │    2.10 │
#>                             ├────────────┼─────────┤
#>                             │ Plum       │    1.80 │
#>                             └────────────┴─────────┘
#> 
#> Column names: Type, Price
theme_blue(jams)
#>                             ┌────────────┬─────────┐
#>                             │ Type       │   Price │
#>                             ├────────────┼─────────┤
#>                             │ Strawberry │    1.90 │
#>                             ├────────────┼─────────┤
#>                             │ Raspberry  │    2.10 │
#>                             ├────────────┼─────────┤
#>                             │ Plum       │    1.80 │
#>                             └────────────┴─────────┘
#> 
#> Column names: Type, Price
theme_orange(jams)
#>                             ┌────────────┬─────────┐
#>                             │ Type       │   Price │
#>                             ├────────────┼─────────┤
#>                             │ Strawberry │    1.90 │
#>                             ├────────────┼─────────┤
#>                             │ Raspberry  │    2.10 │
#>                             ├────────────┼─────────┤
#>                             │ Plum       │    1.80 │
#>                             └────────────┴─────────┘
#> 
#> Column names: Type, Price
theme_green(jams)
#>                             ┌────────────┬─────────┐
#>                             │ Type       │   Price │
#>                             ├────────────┼─────────┤
#>                             │ Strawberry │    1.90 │
#>                             ├────────────┼─────────┤
#>                             │ Raspberry  │    2.10 │
#>                             ├────────────┼─────────┤
#>                             │ Plum       │    1.80 │
#>                             └────────────┴─────────┘
#> 
#> Column names: Type, Price
theme_mondrian(jams)
#>                             ┌────────────┬─────────┐
#>                             │ Type       │   Price │
#>                             ├────────────┼─────────┤
#>                             │ Strawberry │    1.90 │
#>                             ├────────────┼─────────┤
#>                             │ Raspberry  │    2.10 │
#>                             ├────────────┼─────────┤
#>                             │ Plum       │    1.80 │
#>                             └────────────┴─────────┘
#> 
#> Column names: Type, Price
```
