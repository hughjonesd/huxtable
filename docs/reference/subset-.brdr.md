# Replace a subset of a brdr object

Replace a subset of a brdr object

## Usage

``` r
# S3 method for class 'brdr'
x[...] <- value
```

## Arguments

- x:

  A `brdr` object.

- ...:

  Indices.

- value:

  A [`brdr()`](brdr.md) object, number or matrix.

## Value

A [`brdr()`](brdr.md) object.

## Details

You probably don't need to call this directly. If you want to access
border thicknesses, do e.g.

    l_borders <- brdr_thickness(left_border(ht))

which will give you a matrix of numbers.
