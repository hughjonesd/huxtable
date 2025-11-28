# Convert a huxtable for Word/Powerpoint

Huxtables can be converted to
[`flextable::flextable()`](https://davidgohel.github.io/flextable/reference/flextable.html)
objects, for use in Word and Powerpoint documents.

## Usage

``` r
as_flextable(x, ...)

# S3 method for class 'huxtable'
as_flextable(x, colnames_to_header = FALSE, ...)
```

## Arguments

- x:

  A huxtable.

- ...:

  Not used.

- colnames_to_header:

  Use huxtable column names as the header. If `FALSE`, the flextable
  will contain only a body and no header.

## Value

an object of class flextable.

## Details

With recent versions of "flextable" and Pandoc, huxtables can be
automatically outputted from rmarkdown `word_document` and/or
`powerpoint_presentation` documents. (Powerpoint presentations require
pandoc version \>= 2.4.0.)

Properties are supported, with the following exceptions:

- Rotation of 0, 90 or 270 is supported.

- Non-numeric widths and heights are not supported. Table heights are
  treated as a proportion of 9 inches; table widths are treated as a
  proportion of 6 inches. So e.g. `height(ht) <- 0.5` will give a height
  of 4.5 inches.

- Table wrap and table position are not supported.

- Border style "double" is not supported and becomes "solid".

- Captions are supported with recent versions of flextable, but not
  [`caption_pos()`](caption_pos.md) or
  [`caption_width()`](caption_width.md).

## Challenge

Try to say `as_flextable.huxtable` ten times without pausing.

## Examples

``` r
ht <- hux(a = 1:3, b = 1:3)
ft <- as_flextable(ht)
if (FALSE) { # \dontrun{
my_doc <- officer::read_docx()
my_doc <- flextable::body_add_flextable(
  my_doc, ft
)
print(my_doc,
  target =
    "path/to/my_doc.docx"
)
} # }
```
