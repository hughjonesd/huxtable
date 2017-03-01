

BUGS
====
* latex table width don't work (and cell widths probably wrong)
  - because you reset table spec to l/c/r in each 'multicolumn'?
* border width in LaTeX
* collapsing empty cells in HTML; use &nbsp;?
* latex row height is of \\textheight when numeric, not % table height. Hard to fix
* when inserting one hux into another, row heights/col widths may no longer make sense,
  - because they are implicit proportions.


TODO
====

* set wrap to TRUE or FALSE (css: white-space:nowrap)
* padding, margin
* testing!
* screen method/md method
* more advanced positioning (floats?)
* as_huxtable.ftable
* "themes" i.e. standard setups for kinds of tables
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

QUESTIONS
=========
do we want to have a concept of 'headers' for repetition? Cf. longtable,  <th>


