
TODO
====

* After e.g. June, set huxtable.add_colnames to TRUE by default.
  
* Better centring of tables and handling of width. Maybe push \resizebox

* Way to set properties by value (or by an arbitrary variable). E.g.
  `set_xxx_by(ht, row, col, values, ...)`
  where `...` is interpreted as in `cut` or `cut2` or `recode`

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
Possibilities:
```
set_border_at(ht, below, above, left, right, value)
```
where one of below/above/left/right must be specified.
Underlying model options: keep the same as now. Simple, but it breaks if you call e.g. `rowspan`
after calling `set_border_at`. Or, store borders in a new way, e.g. in a (nrow+1) x ncol array
for horiz borders, nrow x (ncol + 1) for vertical ones. Then everything works and you avoid
a huge amount of conversion hassle. You could then deprecate the old interface while
translating calls into the new version. (OK, the conversion hassle still exists...)

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

* rewrite LaTeX for speed, vectorizing and putting bits in appropriately

* a FAQ, including:
  - my table isn't in the centre with position(ht) <- 'left'!
    - try setting the width
  - my numbers have been formatted
    - use number_format
    
* headers property?
  - header rows and columns could be addressed using 3 argument `set_` syntax; e.g. a headers() function returning a 
    matrix
  - headers would be copied (by default?) when you subset huxtables
  - arbitrary rows or columns can be headers; presumably the "relevant" header is the next one to the left/top
    (except for RTL languages?)
    
* make \booktabs or similar work

* classes to represent borders and/or text styles? 
  - so you can do something like `set_left_border(ht, border(1, 'red', 'solid'))`
  - and `set_text(ht, text(10, 'black', 'italic'))`
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





