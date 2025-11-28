# Section about colspan/rowspan

Section about colspan/rowspan

## Cell content

In merged cell ranges, only the top left cell's content is displayed. In
addition, when you merge cells (either by setting
[`colspan()`](spans.md) or [`rowspan()`](spans.md), or using
[`merge_cells()`](merge_cells.md) and friends) the content of the top
left cell is copied to other cells. This prevents unexpected changes to
content if you reorder or subset rows and columns.
