

BUGS
====

* border width in LaTeX... not sure fixable.
  - you may be able to set it per row for horiz ones using arrayrulewidth...
  - yes, just call \global\setlength{\arrayrulewidth}{12pt} before/after the hhline
  - presumably could also do it before/after the actual row
* left/right cell padding doesn't work with multiline tables

TODO
====
* outer spacing for tables
* make huxtables work with filter and other dplyr methods? 
  - NB they work ok with subset, and with dplyr::select
* simple `add_footnote_row` method?
* decimal padding option for columns
  - good for huxreg
* use numeric_format on all numbers "found" in a cell; set to NA to do nothing.
  - good for huxreg (confidence intervals, signif. stars etc.)
* bordercolor would be useful for HTML or dark themes...
* more advanced positioning (floats?)
* word output?
  - probably using http://davidgohel.github.io/ReporteRs/articles/FlexTable.html
  - ah... but you can't use within Rmarkdown, he says
  - See https://github.com/davidgohel/ReporteRs/issues/68
  - could still provide an asFlexTable method, though
* Better, longer documentation
  - Section on rowspecs
  - Use dplyr more, it's precompiled
  - List of all possible attributes
  
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
  - or subsetting, autoinclude header rows/cols
  - seems a bit automagic 



