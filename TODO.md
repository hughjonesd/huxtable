

BUGS
====
* latex table width don't work (and cell widths probably wrong)
  - because you reset table spec to l/c/r in each 'multicolumn'?
* border width in LaTeX
* latex row height is of \\textheight when numeric, not % table height. Hard to fix
* when inserting one hux into another, row heights/col widths may no longer make sense,
  - because they are implicit proportions.


TODO
====
* cbind, rbind methods
  - From Advanced R:
  When implementing a matrix/array class, you should implement these methods: dim (gets you nrow
  and ncol), t, dimnames (gets you rownames and colnames), dimnames<- (gets you colnames<-,
  rownames<-), cbind, rbind.
  - t() seems necessary; dimnames etc. not so much; dim also not required
  - cbind won't dispatch if first element is a vector. Simplest thing is probably:
  cbind(as_hux(1:5), ht) and have an as_hux method for vectors

* set wrap to TRUE or FALSE (css: white-space:nowrap)
* padding, margin
* allow column names to be put into table as option for hux, as_hux
  - maybe row names?
* testing!
* screen method
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
what to do about column names? what if we want multiple rows of column headings?
maybe make it an option
do we want to have a concept of 'headers' for repetition?


