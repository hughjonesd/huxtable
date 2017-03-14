

# utility functions-----------------------------------------------------------------------------------------------------

# return character matrix of formatted contents, suitably escaped
clean_contents <- function(ht, type = c('latex', 'html', 'screen', 'markdown', 'word'), ...) {
  mytype <- match.arg(type)
  contents <- as.matrix(as.data.frame(ht))

  for (col in 1:ncol(contents)) {
    for (row in 1:nrow(contents)) {
      cell <- contents[row, col]
      if (is_a_number(cell)) {
        cell <- as.numeric(cell)
        cell <- format_number(cell, number_format(ht)[[row, col]]) # a list element, double brackets needed
      }
      if (is.na(cell)) cell <- na_string(ht)[row, col]

      contents[row, col] <- cell
    }
    contents[, col] <- decimal_pad(contents[, col], pad_decimal(ht)[,col])
    if (type %in% c('latex', 'html')) {
      # xtable::sanitize.numbers would do very little and is buggy
      to_esc <- escape_contents(ht)[, col]
      contents[to_esc, col] <-  xtable::sanitize(contents[to_esc, col], type)
    }
  }

  contents
}


format_number <- function (num, nf) {
  res <- num
  if (is.function(nf)) res[] <- nf(num)
  if (is.character(nf)) res[] <- sprintf(nf, num)
  if (is.numeric(nf)) res[] <- formatC(round(num, nf), format = 'f', digits = nf)
  res[is.na(num)] <- NA

  res
}

decimal_pad <- function(col, pad_chars) {
  # where pad_chars is NA we do not pad
  orig_col  <- col
  na_pad    <- is.na(pad_chars)
  col       <- col[! na_pad]
  pad_chars <- pad_chars[! na_pad]
  if (length(col) == 0) return(orig_col)

  find_pos  <- function(string, char) {
    regex <- gregexpr(char, string, fixed = TRUE)[[1]]
    regex[length(regex)]
  }
  pos <- mapply(find_pos, col, pad_chars)
  nchars <- nchar(col, type = 'width')
  # take the biggest distance from the decimal point
  pos[pos == -1L] <- nchars[pos == -1L] + 1
  chars_after_. <- nchars - pos

  pad_to <- max(chars_after_.) - chars_after_.
  col <- paste0(col, str_rep(' ', pad_to))

  orig_col[! na_pad] <- col
  orig_col
}

# return data frame mapping real cell positions to cells displayed
display_cells <- function(ht, new_rowspan = rowspan(ht), new_colspan = colspan(ht)) {
  dcells <- data.frame(row = rep(1:nrow(ht), ncol(ht)), col = rep(1:ncol(ht), each = nrow(ht)),
    rowspan = as.vector(new_rowspan), colspan = as.vector(new_colspan))
  dcells$display_row <- dcells$row
  dcells$display_col <- dcells$col
  dcells$shadowed <- FALSE

  change_cols <- c('display_row', 'display_col', 'rowspan', 'colspan')
  for (i in 1:nrow(dcells)) {
    if (dcells$rowspan[i] == 1 && dcells$colspan[i] == 1) next
    if (dcells$shadowed[i]) next

    dr <- dcells$row[i]
    dc <- dcells$col[i]
    spanned <- dcells$row %in% dr:(dr + dcells$rowspan[i] - 1) & dcells$col %in% dc:(dc + dcells$colspan[i] - 1)
    dcells[spanned, change_cols] <- matrix(as.numeric(dcells[i, change_cols]), sum(spanned), length(change_cols), byrow = TRUE)

    shadowed <- spanned & (1:nrow(dcells)) != i
    dcells$shadowed[shadowed] <- TRUE
  }
  dcells$end_row <- dcells$display_row + dcells$rowspan - 1
  dcells$end_col <- dcells$display_col + dcells$colspan - 1

  dcells
}
