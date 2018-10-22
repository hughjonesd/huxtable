
TODO
====

(5.0) means API change, consider for version 5.0

Priority changes
================


* Update website from website-4.3-changes branch

* border styles:
  - TeX Bug: single horizontal borders "start" too late after double vertical border joins them
  - Bug: HTML borders aren't precollapsed, should they be? (Check status.)

* Prepare for broom 0.7.0 and generics:
  - Create a new branch using git rebase --no-ff off the modelgenerics branch. (You need the
    "reverted" merges.)
  - Merge this branch into master.
  - Strategy is: only Import: generics; suggest all other broom packages; in huxreg,
    requireNamespace for all those packages, without warning if they aren't installed (expect maybe
    broom itself?); then just call generics::tidy. This should automatically delegate to
    packages that have registered a tidy method.
  - broom.mixed packages may have effects = "fixed" argument, but you could let the user do that
    with tidy_args
  - Add broom.mixed dependency when it comes out.

* `huxtablereg` function in `texreg` package
  - Waiting for texreg guys to get back


Changes for 5.0
===============

One Q: if these changes are that radical, should it be "huxtable2" or even some other name
(and work with others?)

* Change `theme_striped()` to have two greys - E0 and F0 look OK - with white
  borders and less intense headers

* Change `header_col` to default to `FALSE` in themes.
  
* Consider move to tabu package? Looks easy for dashed lines... (5.0)
  - check that tabu can handle multirow and multicol with background colors
  - also check https://tex.stackexchange.com/questions/48280/longtabu-and-floats-wrong-table-breaks-on-pages-with-floats
  - move to tabu for easier sizing, description of vertical lines and vertical padding;
  - continue to use hhline for horiz non-dashed lines;
  - use tabucline for horiz dashed lines; always merge when possible, and if not 
    (ie if color/width changes), warn that lines will "step" down the page

* Get rid of max_width in to_screen, to_md. It's a huge hassle for the code, and who uses it?

* Fix problem of different classes in padding, col_widths etc. (5.0)
  - Bring back is_a_number
  - Make everything a list-matrix
  - Or introduce explicit units with a character representation
  - Including e.g. `pct()` so you can still do `pct(.5, .25, .25)`
  - Units can have a LaTeX and HTML representation
  - Of course that depends on the context, so maybe the function is like
    format(unit, context) where context might be 'HTML-width' or something
  - could still use characters ("5pt", "10%") in API, they would be internally converted
  - presumably the `units` (and `errors`?) package would be relevant

* Border API rework (5.0)
  - different set of commands
  - stored internally in vert/horiz arrays
  - always precollapsed by definition
  - maybe having a single class for borders
  - so you could do e.g. `set_borders(ht, 1:2, 1:2, border("all", 1, "red", "solid"))
  - first arg could be lrtb, all, outer, vert, horiz, inner
  - similarly for padding: `set_padding(ht, 1:2, 1:2, pad("all", 2))

* Text class also? (5.0)
  - and `set_text(ht, text(10, 'black', 'italic', 'cmss'))`
  
* Could hux$foo <- 1:5 (when foo is a new column) copy properties like cbind? (5.0)

* Changes to a shadowed cell should be redirected to the shadowing cell (5.0), or generally
  affect the merged cell (e.g. borders will work for the relevant area, assuming they are not
  between two merged cells)
  - This fits with the "merge_cells" idea

* After e.g. June, set huxtable.add_colnames to TRUE by default (5.0).

* headers property? (5.0)
  - header rows and columns could be addressed using 3 argument `set_` syntax; e.g. a headers() function returning a 
    matrix
  - headers would be copied (by default?) when you subset huxtables
  - arbitrary rows or columns can be headers; presumably the "relevant" header is the next one to the left/top
    (except for RTL languages?)
  - headers would be repeated after page breaks, and in HTML would use <th> style for column headers
  - maybe headers might have a numeric level rather than simple TRUE or FALSE. Then you could have different styles for
    different levels.

* use tidyselect::vars_select for columns in set_ interface (5.0)
  - Advantage: more consistent with dplyr, allows e.g. set_bold(ht, 1, a:b, TRUE)
  - Downside: can't use logical vectors for columns (without some strong trickery); 
    misleading because you can't do e.g. set_bold(ht, 1, a, b, c, TRUE)
  - One possibility would be to allow the above. If so we'd have to 
    take nargs (as now) and change the nargs == 4 to nargs >= 4. 
    The downside of that is there's no clear division between rows and columns.
  - See the new-colspec-ideas.md file for more thoughts


Bugs
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


Possibles
=========

* Should table-wide or row/col-wide settings automatically apply to new cells?
  - e.g.
    `ht %>% set_bold(TRUE) %>% insert_row("Blah", "blah")`, should the new row have bold TRUE?
  - this is a different way of thinking from copying stuff to new rows, and it might be more natural
  - implementation means that you store table/row/col/cell-level properties separately, and then 
    overlay them when you pull them out.
  - having this AND copying could get messy fast.
  - having it instead of copying would mean that only properties set with e.g. `everywhere` on a 
    row would be copied to a new column. But what about new rows when the header row is different?
    And with the `<-` syntax, you can't tell what the subset applied is, right?
    
* opendocument text/spreadsheet for libreoffice?
  - At some point. Guess not a lot of demand.

* as_datatable for DT::renderDT()?
  - Useful if you want to add the slick features of DT to your work
  - has format* functions for number_format
  - formatStyle can deal with bold, text_color, background_color, font_size et al.
  - also styleInterval and styleEqual are fun to look at
  - is there a use case for going via huxtable and not directly to renderDT?
    - familiarity? Easy to change over?
    
* Look at perl's Latex::Table and how they do it
  - autouses 'p{5cm}' column for cols with > 30 chars in a cell

* Use more purrr and rlang features, as we depend on these packages anyway
  - Use `cli` for `to_screen`? Could maybe get ideas on code. It also has double borders. 

* `col_to_headers` which would take a column, sort by unique values, and replace the column with a rowspan-merged header?
  - might relate to a `headers` flag, if we allow header rows to be in the middle of data
  - perhaps have a method for data frames (which outputs a huxtable)
  - maybe allow multiple columns

* Rotate whole table using "rotating" package (for PDF) and what for HTML (simple CSS rotation on table element, probably)?


* Better centring of LaTeX tables and handling of width. Maybe push \resizebox

* Replace latex_float by float and use the CSS float property on the table (deals with text flow)?
  - As with label, this means different things; also has different values, so
    if you set float() your code wouldn't work in both HTML and TeX...
  - Tricky. Of course many things don't work identically between HTML and TeX.
  - The wrapfig package defines \wraptable which lets text wrap around. It doesn't play nicely
    with floats anyway, so one option is to have a single float() for all the TeX possibilities.
  - Alternatively you have e.g. `wraparound()`, and keep `latex_float()` separate.
  - You can combine options between HTML float (left/right) and wrapfig float.

* rescale numeric row_height and col_width when subsetting, as now under rbind/cbind?

* Rename clean_contents and export it? 
  - Conceivably, people might want to override some individual
    cell contents with raw HTML/TeX/plain text. This still wouldn't affect properties.

* `contents(x) <-` and `set_contents/map_contents`? Useful in pipelines if you want to e.g.
  both bold and upper case certain cells...
  - not sure how `contents(x)[1, 1:3] <- blah` would be different from `x[1, 1:3] <- blah`.
  

* use \hhline double-line feature to handle multiple border widths? i.e. top line is background,
  bottom line is border?
  - not sure we can change colours halfway through though
  

* facilities for re-writing a table in an existing document. 
  - you'd need a way to identify the table (perhaps an automatic `id` table-level property, automatically set to a random
  number)
  - you might also want to overwrite the contents without affecting the style. (Or certain aspects of the style? E.g. bold for
  significance stars, may be data dependent.)
  - this may be a separate package.

* collapsed_border_xxx should return left, right, top, bottom rather than vert, horiz?
  - You do the collapsing, but might want to provide lrtb borders

* special-case single horizontal lines across whole table?

* make \booktabs or similar work

* way to address a particular subset of a matrix, e.g.
  - ht %>% area(1:3, 4:6) %>% set_italic(TRUE) %>% set_border_color('red')
  - presumably this returns the ht with some attribute appropriately set, analogous to groups in a tibble
  - also this should be a separate package! 
  

Put off or abandoned
====================

* cell spacing?
  - this allows natural gaps between borders; see e.g. economist
    forecaster tables.
  - an alternative is to have small empty cells, but that's a hack
  - just too hard in TeX


* table of descriptive statistics using `skimr`:
  - think if this is even a good idea and people shouldn't just DIY with
    `skim_to_wide`? Yeah, maybe not

* Methods for R packages?
  - janitor::tabyl is just a data frame, no useful information available
  - tibble::glimpse prints stuff and returns the original
  
* Use arydshln in LaTeX? Seems incompatible with hhline

* latex row height is of \\textheight when numeric, not % table height. Hard to fix.

* when inserting one hux into another, row heights/col widths may no longer make sense,
  - because they are implicit proportions.

* Could S4 classes work to cbind data frames? Probably not, or hella complex and requires
  reworking internals: https://stackoverflow.com/questions/47967264/dispatch-of-rbind-and-cbind-for-a-data-frame

* Implement `dplyr::bind_rows` and `bind_cols`?
  - they aren't methods.
