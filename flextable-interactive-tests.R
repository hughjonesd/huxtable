
ht_orig <- huxtable(a = c('Foo', 'Bar', 'Baz'), b = 2:4)

ht <- ht_orig
as.FlexTable(ht)
bold(ht)[1, 1:2] <- TRUE
italic(ht)[1:2, 1] <- TRUE
as.FlexTable(ht)
top_border(ht)[1,] <- 1
top_border(ht)[2,] <- 2
top_border(ht)[3,] <- 1
left_border(ht)[1,1]  <- 1
right_border(ht)[2,2] <- 1
as.FlexTable(ht)
top_padding(ht)[1,]     <- 10
left_padding(ht)[2, ]   <- 10
right_padding(ht)[2, 2] <- 10
bottom_padding(ht)[3,1] <- 0
as.FlexTable(ht)
font_size(ht)[2,2] <- 16
font(ht)[3,1] <- 'Arial'
as.FlexTable(ht)

ht <- hux(a = c('short', 'much longer', 'short', 'short'), b = 1:4)

align(ht)[1, 1] <- 'left'
align(ht)[2, 1] <- 'left'
align(ht)[3, 1] <- 'center'
align(ht)[4, 1] <- 'right'
as.FlexTable(ht)

na_string(ht) <- '--'
ht[1,1] <- NA
as.FlexTable(ht)

ht$a <- c('top', 'middle', 'bottom', 'top')
valign(ht)[,1] <- ht$a
font_size(ht)[, 2] <- 16
as.FlexTable(ht)

ht <- hux('Long title' = 1:2, 'Long title 2' = 1:2, add_colnames = TRUE)
rotation(ht)[1:2, ] <- 90
doc <- addFlexTable(docx(), as.FlexTable(ht, header_rows = 2)) # not visible in R
writeDoc(doc, file = 'rotation-test.docx')

ht <- ht_orig
right_border(ht)[,1] <- 1
as.FlexTable(ht)
width(ht) <- .75
col_width(ht) <- c(.7, .3)
as.FlexTable(ht)
col_width(ht) <- c(.2, .8)
as.FlexTable(ht)

