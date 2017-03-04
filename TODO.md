

BUGS
====
* latex table width don't work (and cell widths probably wrong)
  - because you reset table spec to l/c/r in each 'multicolumn'?
* border width in LaTeX... not sure fixable.
  - you may be able to set it per row for horiz ones using arrayrulewidth...
* col_names argument not passing from as_hux to hux

TODO
====

* testing! test examples
* docs: examples
* would be nice to have default align set for numeric cells? But problematic if we guess cell-by-cell.
  - could guess on table creation? But it's a bit DWIM
* set wrap to TRUE or FALSE (css: white-space:nowrap) - use pmb style columns?
* bordercolor would be useful for HTML or dark themes...
* more advanced positioning (floats?)
* easy syntax to alter cells by row, column, area or "where"... this could be a separate package:
  - columns should allow name1:name2 style syntax, maybe also starts_with & friends
  - row names?
  - easy way to specify nrow(x), ncol(x)? How about last(1:3) and last() for default? cf. dplyr::nth
  - row & column groups
  - where(): could return a function that is called in place, something like...
  where <- function(cond) eval(bquote(function(x) which(.(substitute(cond)), arr.ind = TRUE)))
    do_something(ht, cells = where(x > 6))
  would become
    function(ht) which(ht > 6, arr.ind = TRUE)
  to be called by
    if (is.function(cells)) cells <- cells(ht)
  which then returns the relevant values, probably as a 2-column matrix (unusual form of indexing)



PUT OFF
=======
* auto cell align: left for text, decimal for numbers?
  - decimal alignment doesn't work in HTML. LaTeX has dcolumn and/or siunitx... maybe skip
* new version of latex code: compute "real" details (border, cell, content) for each row?
* no need to have set_... methods be subclassable (so long as setters with <- are subclassable)
* setting relative heights in LaTeX. Too complex; can't redefine array stretch.
* latex row height is of \\textheight when numeric, not % table height. Hard to fix
* when inserting one hux into another, row heights/col widths may no longer make sense,
  - because they are implicit proportions.
* way to set defaults? Maybe just export huxtable_default_attrs or have a huxtable_default()<- method

QUESTIONS
=========
do we want to have a concept of 'headers' for repetition? Cf. longtable,  <th>


