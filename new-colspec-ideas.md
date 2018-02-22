
```
# ideal interface:
# - select cells with where() and friends
# - select columns by dplyr-style interface, maybe including unquoted syntax... perhaps via vars()
#   to avoid guessing argument semantics from position?
# - allow selecting of rows/cols by standard methods too
# - skip arguments to select everything...
#
#

# how about a single 3 argument form, with the 2 argument form created by a default:
#
set_xxx(ht, spec, value)

# where spec is implemented via matrix subsetting could be:


rows(1:3)              # implies all columns
rows(c(T, F, F))       # logical vectors
rows(a:b)              # could row names be useful for e.g. huxreg?
rows(matches("foo"))   # select helpers
rows()

cols(2:4)              # implies all rows
cols(c(F, T, T))       # logical vectors
cols(a, b, c:d)        # select
cols(matches("foo"))   # select helpers

rc(1:3, 4:6)           # standard subsetting
rc(1:3, a, b, c:f, -e) # select arguments allowed after first argument? i.e. by default only first arg is rows
rc(row_a, row_b, cols = col_a, col_d:col_e) # sneakily divide arguments up by position of cols?

where(ht == "blah")    # as now
where(. > 1)           # . replaced by first argument, allows use in %>% chains
where(row(.) > col(.)) # . replacement works generally
text_is("blah")        # like where(. == "blah")
text_matches("blah")   # like where(grepl("blah", .)) except that wouldn't work
                       # matching by one or more properties:
properties(font = "Times", text_color = "red")

all_rows(where(ht == "blah"))  # any row matching the inner spec
all_cols(text_matches("blah")) # any col, ditto

# all these things return a set of cells (ie a 2 col matrix); they could be anded and or-ed if you defined a
# class; or if they simply returned a nrow x ncol logical matrix, you could use & and | directly;
# then spec could call `which(arr.ind=T)` on the result.
 

```
