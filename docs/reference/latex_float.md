# Set the position of the table float in LaTeX

Possible values include:

- "h": here

- "h!" definitely here

- "t" top of page

- "ht" here or at top of page

- "b" bottom of page

- "p" page of floats

## Usage

``` r
latex_float(ht)

latex_float(ht) <- value

set_latex_float(ht, value)
```

## Arguments

- ht:

  A huxtable.

- value:

  A string. Set to `NA` to reset to the default, which is `"ht"`.

## Value

`property()` returns the property value(s). `set_property()` and
`map_property()` return the modified huxtable.

## Details

See LaTeX documentation for more details.

## Examples

``` r
set_latex_float(jams, "b")
#>                               Type           Price  
#>                               Strawberry      1.90  
#>                               Raspberry       2.10  
#>                               Plum            1.80  
#> 
#> Column names: Type, Price
```
