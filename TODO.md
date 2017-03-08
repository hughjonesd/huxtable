

BUGS
====

* border width in LaTeX... not sure fixable.
  - you may be able to set it per row for horiz ones using arrayrulewidth...
  - yes, just call \global\setlength{\arrayrulewidth}{12pt} before/after the hhline
  - presumably could also do it before/after the actual row
* left/right cell padding doesn't work with multiline tables
* R CMD CHECK only works when called like so:
`RSTUDIO_PANDOC=/Applications/RStudio.app/Contents/MacOS/pandoc R CMD check --as-cran huxtable_0.1.0.tar.gz` 

TODO
====

* simple `add_footnote_row` method?
* bordercolor would be useful for HTML or dark themes...
* more advanced positioning (floats?)
* general set_properties() function: `set_properties(ht, row, col, font = 'times', align = 'left')`
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
* `col_width`, `row_height` should be plural. Not sure this is necessary.
* `top_padding`, `top_border` should be `padding_top`, `border_top` etc.
  - not sure if autocomplete is easier this way...
* auto cell align: left for text, decimal for numbers?
  - decimal alignment doesn't work in HTML. LaTeX has dcolumn and/or siunitx... maybe skip
* new version of latex code: compute "real" details (border, cell, content) for each row?
* latex row height is of \\textheight when numeric, not % table height. Hard to fix
* when inserting one hux into another, row heights/col widths may no longer make sense,
  - because they are implicit proportions.
* way to set defaults? Maybe just export huxtable_default_attrs or have a huxtable_default()<- method
* `borders()<-` shortcut
  - can't really do this if you want to assign to particular subsets
  - because that would call `borders(ht)` and what does that return?
  - stick with `set_all_borders`
* set properties byrow with byrow = TRUE
  - hard to get this right when setting subsets
  - replacement of the new value takes place outside the `property<-` function
* differentiate headers from content; don't change content to character;
  - this is interesting but hard
  - one poss is to keep the current (simple!) storage but to remember original types and to convert
    back as needed
  - or is there a data frame where types can be arbitrary?
  - or bite the bullet and have separate components
  - or store the data twice, once in the df and once separately on its own? (yuck)
  - NB not nec true that headers are first X rows/cols exclusively!
  - presumably they are always whole rows and columns, though.
  - advantage when cbinding, you could have an option to only cbind the data rows...
  
QUESTIONS
=========
do we want to have a concept of 'headers' for repetition? Cf. longtable,  <th>


