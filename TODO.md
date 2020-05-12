
TODO
====

(5.0) means API change, consider for version 5.0

Priority changes
================


* border styles:
  - TeX Bug: single horizontal borders "start" too late after double vertical border joins them
  - Bug: HTML borders aren't precollapsed, should they be? (Check status.)
    

Changes for 5.0
===============

* `set_vert_padding` and `set_horiz_padding`?

* Merge in the headers-property branch
  - Change `theme_basic()` to simply bold headers
  
* Let width/height/colwidth/rowheight use cssunits?

* New pastel theme?
  
* You can "trim" hhline borders by adding e.g.
  `>{\huxb{255,255,255}{2}}|`
  after each vertical border line - or =. It has to be the same width as
  the vertical border, because it isn't centred otherwise.
  So you could have it as a general property 
  (roughly speaking, do
  vertical borders have "priority"); then maybe a convenience function 
  `trim_borders` which would set the relevant border width and set vertical
  border priority.
  - There's a similarly hackish approach in CSS using :after to create an imaginary
  border.
  - Does one need the same thing for vertical borders? (Actuallly this is pretty
    much how it currently works in LaTeX.)
  - In HTML (on FF), at the 4 "outer" corners different colours split diagonally; at
    all other corners, horizontal borders have priority except on the top row. 
    i.e. it's an unreliable mess!
* `by_` function to use palettes/scales for colour?
    
* Ability to `restack()` a table sideways or lengthways, and to `split` a table.
  - `restack_across(ht, ncol, width)` where `ncol` is the number of columns
    of the new result; or `width` is the maximum width?
  - `restack_down(ht, nrow, height)` similar
  - `split_across` and `split_down` would do the split but not the restacking,
    and would return a list of huxtables.
* subcaption option?
* transparent colours in HTML, RTF, docx?
* Change `theme_striped()` to have two greys - E0 and F0 look OK - with white
  borders and less intense headers.

* Check any interesting stuff from v5.0-devel branch
* In general, when properties are unset, pick them dynamically for "good defaults", rather
  than setting a predictable default.
  - This is a better more general solution than `autoformat`, which could maybe be retired....
* Get rid of max_width in to_screen, to_md. It's a huge hassle for the code, and
  who uses it? 
  - though the problem is having to keep to `options(screen)`, not
    the fact that it is an argument.


Future thoughts
===============


* Drop shadows? :-)
  - looks like a Tikz job in TeX...
  
* Separate out table format representation and output from table creation.
* Create a "textable" package which handles representation
  - just basic as_textable methods for data frames etc.
  - representation probably as a list with rows, columns, cells, each
    with subcomponents - so content is just another subcomponent
  - maybe R6
* Output formats: 
  - allow different engines for different output formats (and engines could
    have options, e.g. use_cline vs use_hhline)
  - e.g. tikz (see "matrix" package in tikz manual)
  - maybe separate output formats into different packages?

  


  
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
  - the easiest thing is probably to have widths as always character
    if you have multiple units. And autoconvert numbers to 
    percentages when they are input.

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
  - one could also have e.g. escaped("$\lambda$") which would autoset
    escape_contents
  
* Could hux$foo <- 1:5 (when foo is a new column) copy properties like cbind? (5.0)

* Changes to a shadowed cell should be redirected to the shadowing cell (5.0), or generally
  affect the merged cell (e.g. borders will work for the relevant area, assuming they are not
  between two merged cells)
  - This fits with the "merge_cells" idea
  - But be aware you could have accidents e.g. if a border is
    set below a row, and one merged cell then picks it up
    (as now!)

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
