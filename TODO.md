
TODO
====

Must be before 4.2.0
====================


Others
======

(5.0) means API change, consider for version 5.0

* Use more purrr and rlang features, as we depend on these packages anyway
  - Use `cli` for `to_screen`? Could maybe get ideas on code. It also has double borders. 

* Implement `dplyr::bind_rows` and `bind_cols`?

* Consider move to tabu package? Looks easy for dashed lines... (5.0)

* Methods for R packages like janitor (tabyl) or glimpse?

* border styles:
  - Use arydshln in LaTeX?
  - TeX Bug: single horizontal borders "start" too late after double vertical border joins them
  - Bug: HTML borders aren't precollapsed, should they be? But see next.

* Border API rework (5.0)
  - different set of commands
  - stored internally in vert/horiz arrays
  - always precollapsed by definition

* Changes to a shadowed cell should be redirected to the shadowing cell (5.0), or generally
  affect the merged cell (e.g. borders will work for the relevant area, assuming they are not
  between two merged cells)
  - This fits with the "merge_cells" idea

* manual vertical/horizontal adjustment in LaTeX?
  - doesn't seem possible in multicol

* Prepare for broom 0.7.0:
  - Create a new branch using git rebase --no-ff off the modelgenerics branch. (You need the
    "reverted" merges.)
  - Merge this branch into master.
  - Strategy is: only Import: modelgenerics; suggest all other broom packages; in huxreg,
    requireNamespace for all those packages, without warning if they aren't installed (expect maybe
    broom itself?); then just call modelgenerics::tidy. This should automatically delegate to
    packages that have registered a tidy method.
  - broom.mixed packages may have effects = "fixed" argument, but you could let the user do that
    with tidy_args

* Fix problem of different classes in padding, col_widths etc. (5.0)
  - Bring back is_a_number
  - Make everything a list-matrix
  - Or introduce explicit units with a character representation
  - Including e.g. `pct()` so you can still do `pct(.5, .25, .25)`
  - Units can have a LaTeX and HTML representation
  - Of course that depends on the context, so maybe the function is like
    format(unit, context) where context might be 'HTML-width' or something
  - could still use characters ("5pt", "10%") in API, they would be internally converted

* collapsed_border_xxx should return left, right, top, bottom rather than vert, horiz?

* Rename clean_contents and export it? 
  - Conceivably, people might want to override some individual
    cell contents with raw HTML/TeX/plain text. This still wouldn't affect properties.

* Could hux$foo <- 1:5 (when foo is a new column) copy properties like cbind? (5.0)

* Get rid of max_width in to_screen, to_md. It's a huge hassle for the code, and who uses it?

* Could S4 classes work to cbind data frames? Probably not, or hella complex and requires
  reworking internals: https://stackoverflow.com/questions/47967264/dispatch-of-rbind-and-cbind-for-a-data-frame

* After e.g. June, set huxtable.add_colnames to TRUE by default (5.0).

* Rework documentation to use more inheritance less templates.

* Better centring of LaTeX tables and handling of width. Maybe push \resizebox

* Replace latex_float by float and use the CSS float property on the table (deals with text flow)?
  - As with label, this means different things; also has different values, so
    if you set float() your code wouldn't work in both HTML and TeX...
  - Tricky. Of course many things don't work identically between HTML and TeX.
  - The wrapfig package defines \wraptable which lets text wrap around. It doesn't play nicely
    with floats anyway, so one option is to have a single float() for all the TeX possibilities.
  - Alternatively you have e.g. `wraparound()`, and keep `latex_float()` separate.
  - You can combine options between HTML float (left/right) and wrapfig float.

* Tidy LaTeX output by not spitting out colors etc. when defaults will do.

* Handle border colors in a "pre-collapsed" way to get round CSS oddities; as with borders, you have
  the same nrow+1 x ncol,nrow x ncol+1 arrays of values and the "last one set wins"

* rescale numeric row_height and col_width when subsetting, as now under rbind/cbind?

* Way to set properties by value (or by an arbitrary variable). E.g.
  `set_xxx_by(ht, row, col, values, ...)`
  where `...` is interpreted as in `cut` or `cut2` or `recode`

* Add broom.mixed dependency when it comes out.

* use tidyselect::vars_select for columns in set_ interface
  - Advantage: more consistent with dplyr, allows e.g. set_bold(ht, 1, a:b, TRUE)
  - Downside: can't use logical vectors for columns (without some strong trickery); 
    misleading because you can't do e.g. set_bold(ht, 1, a, b, c, TRUE)
  - One possibility would be to allow the above. If so we'd have to 
    take nargs (as now) and change the nargs == 4 to nargs >= 4. 
    The downside of that is there's no clear division between rows and columns.
  - See the new-colspec-ideas.md file for more thoughts
  
* simple interface for borders even when there are multiple spans. E.g.
```
ht <- hux(1:2, 3:4)
rowspan(ht)[1, 1] <- 2
bottom_border(ht)[2, ] <- 1
```
should work, whereas now it doesn't put a border below column 1.
Store borders & colors the way collapsed_borders reports them; override default getter/setters to so that rows and columns refer to "real positions". 


* way to address a particular subset of a matrix, e.g.
  - ht %>% area(1:3, 4:6) %>% set_italic(TRUE) %>% set_border_color('red')
  - presumably this returns the ht with some attribute appropriately set, analogous to groups in a tibble
  - also this should be a separate package! 
  
* use \hhline double-line feature to handle multiple border widths? i.e. top line is background,
  bottom line is border?
  - not sure we can change colours halfway through though
  
* table of descriptive statistics? Maybe a separate package.

* facilities for re-writing a table in an existing document. 
  - you'd need a way to identify the table (perhaps an automatic `id` table-level property, automatically set to a random
  number)
  - you might also want to overwrite the contents without affecting the style. (Or certain aspects of the style? E.g. bold for
  significance stars, may be data dependent.)
  - this may be a separate package.

* `huxreg`-style function in `texreg` package

* a FAQ, including:
  - my table isn't in the centre with position(ht) <- 'center'!
    - try setting the width
  - my numbers have been formatted
    - use number_format
  - LaTeX output isn't working
    - have you tried `check_latex_dependencies`?
    
* headers property?
  - header rows and columns could be addressed using 3 argument `set_` syntax; e.g. a headers() function returning a 
    matrix
  - headers would be copied (by default?) when you subset huxtables
  - arbitrary rows or columns can be headers; presumably the "relevant" header is the next one to the left/top
    (except for RTL languages?)
  - headers would be repeated after page breaks, and in HTML would use <th> style for column headers
    
* make \booktabs or similar work

* classes to represent borders and/or text styles? 
  - so you can do something like `set_left_border(ht, border(1, 'red', 'solid'))`
  - and `set_text(ht, text(10, 'black', 'italic', 'cmss'))`
  - might not add much value compared to `set_properties`...?
  
* dotted borders with LaTeX package? See https://tex.stackexchange.com/questions/20140/can-a-table-include-a-horizontal-dashed-line

* special-case single horizontal lines across whole table?



BUGS
====

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

PUT OFF
=======
* latex row height is of \\textheight when numeric, not % table height. Hard to fix
* when inserting one hux into another, row heights/col widths may no longer make sense,
  - because they are implicit proportions.


