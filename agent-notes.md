
# Agent notes

## Meta

* This file is for llms to record their understanding of the code.

* Write notes as bullet points. At the end of each note, add the revision number
your referring to.

* Where possible, update an existing note before adding your own.

## Typst export

* `to_typst()` builds cell strings row-by-row and previously left empty rows when all cells were shadowed by merges. Filter `row_strings` and `header_rows_strings` with `nzchar()` before assembling the table to avoid stray commas. rev b07e9e37.
* Typst export sets `stroke: none` on tables to avoid default borders. 86529fa
* Typst export outputs labels using `<label>` after the `#figure` block for cross-referencing. rev 088f1fe0
* MarkdownTypstTranslator handles bold, italic, links, images, headings, strikethrough, inline code, and lists via `render_markdown("...", "typst")`. rev 45b775da
* `to_typst()` now uses `clean_contents(..., output_type = "typst")` so markdown cells output Typst markup instead of TeX. rev d233e674
