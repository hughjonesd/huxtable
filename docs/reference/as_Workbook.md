# Convert a huxtable for Excel

If the `openxlsx` package is installed, Huxtables can be converted to
[`openxlsx::openxlsx()`](https://rdrr.io/pkg/openxlsx/man/openxlsx.html)
Worbook objects, for use in Excel documents.

## Usage

``` r
as_Workbook(ht, ...)

# S3 method for class 'huxtable'
as_Workbook(
  ht,
  Workbook = NULL,
  sheet = "Sheet 1",
  write_caption = TRUE,
  start_row = 1,
  start_col = 1,
  ...
)
```

## Arguments

- ht:

  A huxtable.

- ...:

  Not used.

- Workbook:

  An existing `Workbook` object. By default, a new workbook will be
  created.

- sheet:

  Name for the worksheet where the huxtable will be created. The
  worksheet will be created if it doesn't exist already.

- write_caption:

  If `TRUE`, print any caption in the row above or below the table.

- start_row, start_col:

  Number. Write data starting at the given row and column.

## Value

An object of class `Workbook`.

## Details

Use
[`openxlsx::saveWorkbook()`](https://rdrr.io/pkg/openxlsx/man/saveWorkbook.html)
to save the resulting object to an Excel file.

Properties are supported with the following exceptions:

- Non-numeric column widths and row heights, table width and height.

- Decimal padding.

- Cell padding.

- Table position.

- Caption width.

Huxtable tries to guess appropriate widths and height for rows and
columns; numeric [`width()`](width.md) and [`height()`](height.md) are
treated as scaling factors.

Contents are only stored as numbers if a whole column is "numeric", i.e.
can be converted by
[`as.numeric()`](https://rdrr.io/r/base/numeric.html)). Otherwise, they
are stored as text.

## Examples

``` r
wb <- as_Workbook(jams)

if (FALSE) { # \dontrun{
openxlsx::saveWorkbook(
  wb,
  "my-excel-file.xlsx"
)
} # }

# multiple sheets in a single workbook:
wb <- openxlsx::createWorkbook()
wb <- as_Workbook(jams,
  Workbook = wb, sheet = "sheet1"
)
wb <- as_Workbook(
  hux("Another", "huxtable"),
  Workbook = wb,
  sheet = "sheet2"
)
```
