

BUGS
====

* border width in LaTeX... not sure fixable.
  - you may be able to set it per row for horiz ones using arrayrulewidth...
  - yes, just call \global\setlength{\arrayrulewidth}{12pt} before/after the hhline
  - presumably could also do it before/after the actual row

TODO
====
* smarter website
  - multiple examples
* simple `add_footnote_row` method?
* Use strwrap in to_screen (and don't count wrap cells for width)
* use numeric_format on all numbers "found" in a cell; set to NA or -1 to do nothing.
  - good for huxreg (confidence intervals, signif. stars etc.)
* more advanced positioning (floats?)
  - LaTeX: float string

  
PUT OFF
=======
* `col_width`, `row_height` should be plural. Not sure this is necessary.
* `top_padding`, `top_border` should be `padding_top`, `border_top` etc.
  - not sure if autocomplete is easier this way...
* latex row height is of \\textheight when numeric, not % table height. Hard to fix
* when inserting one hux into another, row heights/col widths may no longer make sense,
  - because they are implicit proportions.
* way to set defaults? Maybe just export huxtable_default_attrs or have a huxtable_default()<- method
* `borders()<-` shortcut
  - can't really do this if you want to assign to particular subsets
  - because that would call `borders(ht)` and what does that return?
  - stick with `set_all_borders`




