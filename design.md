# Huxtable Architecture

## Overview
Huxtable is an R package for creating and displaying richly formatted tables.  The codebase is organized primarily under the `R/` directory, with functions grouped by responsibility (object creation, property manipulation, output rendering, etc.).  Tests live in `tests/testthat/` and additional materials (documentation, vignettes) reside in the top level.

## Object creation and representation
The user-level constructor `huxtable()` (aliased as `hux()`) builds a data frame and then converts it into a huxtable through `as_huxtable()` (which calls `new_huxtable`).  The constructor handles options like automatically adding column names, row names, and column auto-formatting【F:R/creation.R†L52-L72】.

`new_huxtable()` sets up the core object: a data frame with matrix or vector attributes for every cell, column, row, and table property, using defaults stored in `huxtable_env$huxtable_default_attrs`【F:R/creation-internal.R†L8-L46】【F:R/property-helpers.R†L9-L85】.  These attributes hold formatting details (alignment, borders, padding, etc.) and are populated with default values on object creation.

## Property helpers and defaults
`R/property-helpers.R` defines the lists of supported cell, column, row, and table properties and a set of helper functions for getting, setting, replacing, or mapping over those attributes【F:R/property-helpers.R†L1-L118】.  The `huxtable_env` environment stores default values for all properties, enabling global default customization and consistent initialization.

## Rendering pipeline
Cell contents are cleaned and formatted before rendering by `clean_contents()`, which applies number formatting, markdown rendering, escaping, and alignment based on cell attributes and output type【F:R/clean-contents.R†L1-L37】【F:R/clean-contents.R†L39-L114】.

### HTML output
`to_html()` assembles the HTML table.  It builds the opening `<table>` tag and caption, column widths, cell markup, and row wrappers using helper functions such as `build_table_style()`, `build_colgroup()`, `build_cell_html()`, and `build_row_html()`【F:R/html.R†L35-L46】【F:R/html.R†L49-L120】.

### LaTeX output
`to_latex()` generates LaTeX code by first constructing the `tabular` section, then wrapping it in an appropriate floating environment with caption, size adjustments, and commands for borders and padding【F:R/latex.R†L17-L102】.

## Integration with R Markdown
When a huxtable is printed in a knitr/Rmarkdown context, `knit_print.huxtable()` is invoked.  It detects the desired output format (`latex`, `html`, `rtf`, etc.) via `guess_knitr_output_format()` and dispatches to the corresponding renderer (`to_latex`, `to_html`, `to_md`, etc.).  For LaTeX and HTML, the returned markup is passed to knitr as-is, possibly with additional dependencies for LaTeX packages【F:R/knitr.R†L18-L60】.

## Potential improvements and subtasks
* **Clarify property management** – consolidate property get/set utilities into a unified module and document the mapping between attributes and helper functions to reduce duplication and ease onboarding.
* **Introduce a rendering abstraction** – create an intermediate representation of a huxtable (layout + styles) that can be serialized to HTML, LaTeX, or other formats, reducing duplication in `to_html()` and `to_latex()`.
* **Reorganize source files** – group R files by module (creation, properties, output, utilities) or use subdirectories to make navigation easier.
* **Centralize option handling** – encapsulate option defaults and runtime configuration into a dedicated configuration object or module instead of storing state directly in `huxtable_env`.
* **Expand developer documentation** – extend this design document or create vignette(s) detailing internal workflows to assist contributors.

