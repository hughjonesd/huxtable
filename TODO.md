

BUGS
====

* pad_decimal of huxreg fails with significance stars in on_screen (but not HTML or TeX)
* nowrap cells
  - if we use raw content in p/m/b, content wraps, then padding fails (only left pads top line, right pads bottom)
  - if we use mbox in p/m/b content doesn't wrap, but cells don't expand
  - if we use l/c/r, then table position goes wonky when cells expand; worse, background colors
    can be messed up in previous cells which expected the table not to expand!
  - need to respect valign (which p/m/b does for wrapped cells). But what is valign for a nowrap cell?
  - cells should expand to match nowrap width by default
  - basic choice is: nowrap cells overrun if content too long; or they expand, but can mess up background
    colour and position. 
    - the first issue will happen more often, but the fix is more obvious to the user (wider table!)
* LaTeX fonts not working

TODO
====

* `print_latex` is slooow on big tables, see e.g. design-principles.Rmd
  - revert `tex_glue` changes, it is 40x slower than paste0
  - memoize collapsed_borders
  - make collapsed_* take a single row/col combo! Or, call them only once!
* `set_outer_borders` should work with `everywhere` and friends.
  - write a function that returns the relevant border indices
  - use that to set top/bot/l/r borders in a square
* use number_format on all numbers "found" in a cell; set to NA or -1 to do nothing.
  - good for huxreg (confidence intervals, signif. stars etc.)
  - default should be NA, and this should leave numbers alone, so that e.g. ints remain as ints
* A FAQ, including:
  - my table isn't in the centre with position(ht) <- 'left'!
    - try setting the width
  - my numbers have been formatted
    - use number_format
* headers property?
  - header rows and columns could be addressed using 3 argument `set_` syntax;
  - headers would be copied (by default?) when you subset huxtables
  - arbitrary rows or columns can be headers; presumably the "relevant" header is the next one to the left/top
    (except for RTL languages?)
* make \booktabs or similar work
* If possible, make variable width horizontal borders in latex work (it can work ok if background colour is unset)

PUT OFF
=======
* latex row height is of \\textheight when numeric, not % table height. Hard to fix
* when inserting one hux into another, row heights/col widths may no longer make sense,
  - because they are implicit proportions.





