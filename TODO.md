

BUGS
====

* border width in LaTeX... not sure fixable.
  - you may be able to set it per row for horiz ones using arrayrulewidth...
  - yes, just call \global\setlength{\arrayrulewidth}{12pt} before/after the hhline
  - presumably could also do it before/after the actual row
  - maybe using vborder you can set a width to the vrule; but will this work with hhline?
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
* top border is overwritten after row 1 in latex
* LaTeX fonts not working

TODO
====

* `set_outer_borders` ?
* huxreg format_error argument so you could do e.g. '(%stderr) [%ci.low to %ci.high]'?
* Use strwrap in to_screen (and don't count wrap cells for width)
* use numeric_format on all numbers "found" in a cell; set to NA or -1 to do nothing.
  - good for huxreg (confidence intervals, signif. stars etc.)
* more advanced positioning (floats?)
  - LaTeX: float string
* Easy way to set defaults
* insert_row and insert_column methods, to wrap `cbind`, `rbind`. Cf. `append`.
  - `after` and `span` arguments
* A FAQ, including:
  - my table isn't in the centre with position(ht) <- 'left'!
    - try setting the width
  
PUT OFF
=======
* `col_width`, `row_height` should be plural. Not sure this is necessary.
* `top_padding`, `top_border` should be `padding_top`, `border_top` etc.
  - not sure if autocomplete is easier this way...
* latex row height is of \\textheight when numeric, not % table height. Hard to fix
* when inserting one hux into another, row heights/col widths may no longer make sense,
  - because they are implicit proportions.





