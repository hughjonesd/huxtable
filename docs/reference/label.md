# Set a table label for external referencing

The label is used as the table's label in LaTeX, and as the "id"
property of the table element in HTML.

## Usage

``` r
label(ht)

label(ht) <- value

set_label(ht, value)
```

## Arguments

- ht:

  A huxtable.

- value:

  A string. Set to `NA` to reset to the default, which is
  `NA_character_`.

## Value

`property()` returns the property value(s). `set_property()` and
`map_property()` return the modified huxtable.

## Details

LaTeX table labels typically start with `"tab:"`.

Within knitr, huxtable labels will default to the same as the knitr
chunk label. To turn off this behaviour, set
`options(huxtable.autolabel = FALSE)`.

If you use [bookdown](https://bookdown.org), and set a label on your
table, the table [`caption()`](caption.md) will automatically be
prefixed with `(#label)`. You can then refer to the table using
`@ref(label)`. `label` needs to start with `"tab:"`; if it doesn't, the
`"tab:"` prefix will be added automatically. To turn off this behaviour,
set `options(huxtable.bookdown = FALSE)`.

## See also

huxtable-options

## Examples

``` r
set_label(jams, "tab:mytable")
#>                               Type           Price  
#>                               Strawberry      1.90  
#>                               Raspberry       2.10  
#>                               Plum            1.80  
#> 
#> Column names: Type, Price
```
