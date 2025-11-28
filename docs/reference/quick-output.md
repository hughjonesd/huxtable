# Quickly print objects to a PDF, TeX, Typst, HTML, Microsoft Office or RTF document, or PNG or SVG images

These functions use huxtable to print objects to an output document.
They are useful as one-liners for data reporting.

## Usage

``` r
quick_latex(
  ...,
  file = confirm("huxtable-output.tex"),
  borders = 0.4,
  open = interactive()
)

quick_pdf(
  ...,
  file = confirm("huxtable-output.pdf"),
  borders = 0.4,
  open = interactive(),
  width = NULL,
  height = NULL
)

quick_typst(
  ...,
  file = confirm("huxtable-output.typ"),
  borders = 0.4,
  open = interactive()
)

quick_typst_pdf(
  ...,
  file = confirm("huxtable-output.pdf"),
  borders = 0.4,
  open = interactive(),
  width = NULL,
  height = NULL
)

quick_typst_png(
  ...,
  file = confirm_prefix("huxtable-output"),
  borders = 0.4,
  open = interactive(),
  width = NULL,
  height = NULL,
  ppi = NULL
)

quick_typst_svg(
  ...,
  file = confirm_prefix("huxtable-output"),
  borders = 0.4,
  open = interactive(),
  width = NULL,
  height = NULL
)

quick_html(
  ...,
  file = confirm("huxtable-output.html"),
  borders = 0.4,
  open = interactive()
)

quick_docx(
  ...,
  file = confirm("huxtable-output.docx"),
  borders = 0.4,
  open = interactive()
)

quick_pptx(
  ...,
  file = confirm("huxtable-output.pptx"),
  borders = 0.4,
  open = interactive()
)

quick_xlsx(
  ...,
  file = confirm("huxtable-output.xlsx"),
  borders = 0.4,
  open = interactive()
)

quick_rtf(
  ...,
  file = confirm("huxtable-output.rtf"),
  borders = 0.4,
  open = interactive()
)
```

## Arguments

- ...:

  One or more huxtables or R objects with an `as_huxtable` method.

- file:

  File path for the output.

- borders:

  Border width for members of `...` that are not huxtables.

- open:

  Logical. Automatically open the resulting file?

- width:

  String passed to the LaTeX `geometry` package's `paperwidth` option or
  Typst's `page` `width` option. Use `NULL` for the default width.

- height:

  String passed to the LaTeX `geometry` package's `paperheight` option
  or Typst's `page` `height` option. Use `NULL` for the default height.

- ppi:

  Pixels per inch for PNG output.

## Value

Invisible `NULL`.

## Details

Objects in `...` will be converted to huxtables, with borders added.

If ‘file’ is not specified, the command will fail in non-interactive
sessions. In interactive sessions, the default file path is
"huxtable-output.xxx" in the working directory; if this already exists,
you will be asked to confirm manually before proceeding.

To create docx and pptx files `flextable` and `officer` must be
installed, while xlsx needs `openxlsx`. `quick_typst_pdf()`,
`quick_typst_png()`, and `quick_typst_svg()` require the `typst` command
line tool.

`quick_typst_pdf()` with e.g. `file = "foo.pdf"` will overwrite and
delete the file `foo.typ`.

`quick_typst_png()` and `quick_typst_svg()` create one image per
huxtable. If there is more than one object in `...`, images will have a
numeric suffix like `"-1", "-2"` etc. Existing files with the same
`file` prefix will be overwritten after confirmation in interactive
sessions.

## Examples

``` r
if (FALSE) { # \dontrun{
m <- matrix(1:4, 2, 2)

quick_pdf(m, jams)
quick_latex(m, jams)
quick_typst(m, jams)
quick_typst_pdf(m, jams)
quick_typst_png(m, jams)
quick_typst_svg(m, jams)
quick_html(m, jams)
quick_docx(m, jams)
quick_xlsx(m, jams)
quick_pptx(m, jams)
quick_rtf(m, jams)
} # }
```
