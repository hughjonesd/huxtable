# Mapping parameters

Mapping parameters

## Arguments

- values:

  A vector of property values. `length(values)` should be one greater
  than `length(breaks)` if `extend = TRUE`, or one less if
  `extend = FALSE`.

- right:

  If `TRUE`, intervals are closed on the right, i.e. if values are
  exactly equal to a [`break`](https://rdrr.io/r/base/Control.html),
  they go in the lower group. Otherwise, intervals are closed on the
  left, so equal values go in the higher group. `FALSE` by default.

- extend:

  Extend `breaks` to `c(-Inf, breaks, Inf)`, i.e. include numbers below
  and above the outermost breaks. `TRUE` by default.

- ignore_na:

  If `TRUE`, `NA` values in the result will be left unchanged from their
  previous values. Otherwise, `NA` normally resets to the default.

- colwise:

  Logical. Calculate breaks separately within each column?
