# Huxtable Architecture

## Overview
Huxtable is an R package for creating and displaying richly formatted tables.
The codebase is organized primarily under the `R/` directory, with functions 
grouped by responsibility (object creation, property manipulation, output 
rendering, etc.).  Tests live in `tests/testthat/` and additional materials 
(documentation, vignettes) reside in the top level.

## Object creation and representation
The user-level constructor `huxtable()` (aliased as `hux()`) builds a data frame
and then converts it into a huxtable through `as_huxtable()` (which calls 
`new_huxtable`).  The constructor handles options like automatically adding 
column names, row names, and column auto-formatting【F:R/creation.R†L52-L72】.

`new_huxtable()` sets up the core object: a data frame with matrix or vector 
attributes for every cell, column, row, and table property, using defaults 
stored in `huxtable_env$huxtable_default_attrs`【F:R/creation-internal.R†L8-L46】
【F:R/property-helpers.R†L9-L85】.  These attributes hold formatting details 
(alignment, borders, padding, etc.) and are populated with default values 
on object creation.

## Property helpers and defaults
`R/property-helpers.R` defines the lists of supported cell, column, row, and 
table properties and a set of helper functions for getting, setting, replacing, 
or mapping over those attributes【F:R/property-helpers.R†L1-L118】.  
The `huxtable_env` environment stores default values for all properties, 
enabling global default customization and consistent initialization.

The table-level `table_background_color` property is a default beneath
cell-level `background_color` values. HTML and Typst render it directly on the
table, and non-breakable LaTeX tables use a zero-padding color box. Backends
without a table backdrop resolve it into otherwise unfilled cells with
`effective_background_color()`.

## Rendering pipeline
Cell contents are cleaned and formatted before rendering by 
`clean_contents()`, which applies number formatting, markdown rendering, 
escaping, and alignment based on cell attributes and output type
【F:R/clean-contents.R†L1-L37】【F:R/clean-contents.R†L39-L114】.

### HTML output
`to_html()` assembles the HTML table.  It builds the opening 
`<table>` tag and caption, column widths, cell markup, and row wrappers 
using helper functions such as `build_table_style()`, `build_colgroup()`, 
`build_cell_html()`, and `build_row_html()`【F:R/html.R†L35-L46】
【F:R/html.R†L49-L120】.

### LaTeX output
`to_latex()` generates LaTeX code by first constructing the `tabular` section, 
then wrapping it in an appropriate floating environment with caption, 
size adjustments, and commands for borders and padding【F:R/latex.R†L17-L102】.
Tables with `breakable()` set use a separate `longtable` assembly path: they
stay in the document flow, repeat leading header rows, and place captions
inside the longtable rather than using the normal float and `threeparttable`
wrappers.

### Multipage output
The table-level `breakable` property maps page breaking onto each paged
backend. Typst makes its figure block breakable, HTML emits fragmentation CSS,
RTF and Word control whether rows are kept together, and LaTeX uses
`longtable`. Renderers keep individual rows intact by default.

### Excel output
`as_Workbook()` writes cell contents in blocks of rows with compatible column
types. It groups cells with identical styles before calling `openxlsx::addStyle()`,
so the workbook contains one style record per distinct style and table rather
than one record per cell【F:R/Workbook.R†L64-L218】.

## Integration with R Markdown
When a huxtable is printed in a knitr/Rmarkdown context, 
`knit_print.huxtable()` is invoked.  It detects the desired output format 
(`latex`, `html`, `rtf`, etc.) via `guess_knitr_output_format()` and dispatches 
to the corresponding renderer (`to_latex`, `to_html`, `to_md`, etc.).  For 
LaTeX and HTML, the returned markup is passed to knitr as-is, possibly with 
additional dependencies for LaTeX packages【F:R/knitr.R†L18-L60】.

Caption and label behavior shared by the renderers is implemented in
`R/captions.R`. `resolve_caption()` returns caption text, the resolved label,
and whether bookdown syntax embedded that label in the caption. LaTeX, HTML,
Markdown and Typst consume this result, while each renderer remains responsible
for its own escaping, sizing and output markup. Simpler backends such as screen,
RTF and Excel use the raw caption property and the shared horizontal and
vertical position helpers.

Automatic labels are based on the knitr chunk label. Labels already emitted in
the current chunk are stored in `knitr::opts_current`, so additional huxtables
receive deterministic suffixes without state leaking into later chunks or later
knitting runs.
