
# PLAN for rewrite of subsetting and creation
# 

* All subsetting/replacement functions immediately get control of indices, and turn
  them into two sequences of numbers.
* Subsetting functions:
  - call `delete_rows/cols` on the old huxtable
* Replacement functions:

```r
ht$col <- NULL
ht[[col]] <- NULL
ht$colname <- vector_or_hux
ht[[col]] <- vector_or_hux
ht[[row, col]] <- scalar # just runs the data frame method
ht[cols] <- dataframe_or_hux # or vector if cols is a scalar
ht[rows, cols] <- dataframe_or_hux
ht[num_matrix] <- dataframe_or_hux # do we want to allow this? tibble doesn't
```
  - If `value` is `NULL`, and all rows are selected, then delete the relevant
    column(s). 
    - Dfrs don't allow `obj[all_rows, col] <- NULL` but they
      do allow `obj[, col] <- NULL` and this even works if `col` mixes
      existing and non-existent columns.
    - I think we should warn when deleting a non-existent column.
    - Call `delete_cols(ht, existing_cols)` which should delete the data
      and properties.
    - Dfrs don't allow `obj[row,] <- NULL`.
  - Determine which columns and rows are new, and which exist
    - data frame methods are very relaxed about this - you can combine old
      and new columns in a single call, add new rows, add new columns while
      only specifying a subset of rows, or vice versa, add new rows and 
      columns simultaneously
    - actually, so are tibbles. The only difference is that tibble[, "x"] returns
      a tibble. (tibble["x"] and dataframe["X"] both return tibbles/dfs).

```r
ht[mix_new_and_old, mix_new_and_old] <- value
```
    - do we want to be so relaxed? Probably: we say "it's just a data frame..."
      - But maybe we warn if you are adding new rows/cols with a subset of
        cols/rows - this could easily be a user error.
  - May have to turn value into a row-matrix , if we are replacing a single
    row in the 2 argument [ form.
  - Check that `value` has the right dimension. If it is a length-1 vector,
    recycle it.
  - `replace_cells(ht, rows, cols, value)` for replacement rows and columns
    - this will only replace old properties if value is a huxtable
    - maybe another S3-multimethod?
    - if value is a huxtable, then it replaces cell properties; if it 
      is filling a whole row/column, it replaces row/column properties.
      - maybe also some way of dealing with row heights? `normalize` function
        to make everything add to 1?
  - Call `bind_rows_2(ht, extended_value)` for new rows
  - Call `bind_cols_2(ht, extended_value)` for new columns
  
* `cbind.huxtable(...)` and `rbind.huxtable(...)`
  - call `Reduce(bind_rows/cols_2, ...)`.


* `bind_rows_2(obj1, obj2)` and `bind_cols_2(obj1, obj2)`
  - if either obj is not a huxtable, then a new one is created.
    Properties are copied down/across by default.
  - new_huxtable() is called with the data from `obj1` and `obj2`
  - the new object has properties set by merging the properties of the
    two old ones using `merge_properties_across/down(new_obj, obj1, obj2)`
  - should we autoformat new columns (assuming we haven't merged properties
    across?) Should this be an option? (Maybe not...)


* `merge_properties_across(new_obj, obj1, obj2)` and `merge_properties_down(new_obj, obj1, obj2)`
  - assserts they have huxtables
  - do the merge, if possible using the function interface
  - can these be inlined into `bind2_rows/cols`? but maybe clean to
    have them separate just for code clarity

* `delete_rows(obj, rows)` and `delete_cols(obj, cols)`
  - asserts `obj` is a huxtable
  - delete cell and row/col properties
  - delete data using data frame methods
  - we can call `as.data.frame` since `as.data.frame.data.frame` 
    automatically unclasses (weird but true)
  - merge borders as currently
  - truncate spans
  
TODO:
* when we e.g. cbind 2 huxtables, right table's leftmost border will
  vanish if left table has 0 rightmost border. Do we accept this? Or
  do we take the maximum of the border widths? Presumably we accept styles?
* do we want to autoformat new columns in `cbind` and `ht$new_col <- vec`?
* add tests for subsetting
