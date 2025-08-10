
# Agent notes

## Meta

* This file is for llms to record their understanding of the code.

* Write notes as bullet points. At the end of each note, add the revision number
your referring to.

* Where possible, update an existing note before adding your own.

## Typst export

* `to_typst()` builds cell strings row-by-row and previously left empty rows when all cells were shadowed by merges. Filter `row_strings` and `header_rows_strings` with `nzchar()` before assembling the table to avoid stray commas. rev b07e9e37.
