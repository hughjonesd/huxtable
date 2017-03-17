
library(ReporteRs)
library(magrittr)
library(huxtable)

show_add <- function(doc, ht, message) {
  if (is_hux(ht)) ft <- as_FlexTable(ht)
  print(ft)
  doc <- addParagraph(doc, pot(paste0("\n\n", message, "\n"), textProperties(font.weight = 'bold')))
  doc <- addFlexTable(doc, ft)
  doc
}

doc <- docx()
ht_orig <- huxtable(a = c('Foo', 'Bar', 'Baz'), b = 2:4)

ht <- ht_orig
doc %<>% show_add(ht, 'Basic table')
bold(ht)[1, 1:2] <- TRUE
italic(ht)[1:2, 1] <- TRUE
doc %<>% show_add(ht, 'Bold + Italic')
top_border(ht)[1,] <- 1
top_border(ht)[2,] <- 2
top_border(ht)[3,] <- 1
left_border(ht)[1,1]  <- 1
right_border(ht)[2,2] <- 1
doc %<>% show_add(ht, 'Borders: second top should be thicker')

ht <- set_all_border_colors(ht, 1:2, 1, 'blue')
ht <- set_all_border_colors(ht, 2:3, 2, 'red')
doc %<>% show_add(ht, 'Border colors: left/top blue, bottom/right red')


top_padding(ht)[1,]     <- 10
left_padding(ht)[2, ]   <- 10
right_padding(ht)[2, 2] <- 10
bottom_padding(ht)[3,1] <- 0
doc %<>% show_add(ht, 'Top padding row 1; left padding row 2; right padding cell (2,2); bottom padding 0 cell (3,1)')

font_size(ht)[2,2] <- 16
font(ht)[3,1] <- 'Arial'
doc %<>% show_add(ht, 'Font size and font')

ht <- hux(a = c('short', 'much longer', 'short', 'short'), b = 1:4)

align(ht)[1, 1] <- 'left'
align(ht)[2, 1] <- 'left'
align(ht)[3, 1] <- 'center'
align(ht)[4, 1] <- 'right'
doc %<>% show_add(ht, 'Horizontal alignment')

na_string(ht) <- '--'
ht[1,1] <- NA
doc %<>% show_add(ht, 'NA string --')

ht$a <- c('top', 'middle', 'bottom', 'top')
valign(ht)[,1] <- ht$a
font_size(ht)[, 2] <- 16
doc %<>% show_add(ht, 'Vertical alignment')

ht <- ht_orig
ht <- set_all_borders(ht, , , 1)
doc %<>% show_add(ht, 'Width: nothing specified')

width(ht) <- .75
col_width(ht) <- c(.7, .3)
doc %<>% show_add(ht, 'Width: .7/.3, total .75')
col_width(ht) <- c(.2, .8)
doc %<>% show_add(ht, 'Width: .2/.8, total .75')

ht <- ht_orig
background_color(ht)[1,] <- 'yellow'
background_color(ht)[1,2] <- 'orange'
background_color(ht)[2,] <- grey(.95)
background_color(ht)[3,] <- '#9AAA35'
doc %<>% show_add(ht, 'Background color')

ht <- ht_orig
text_color(ht)[, 1] <- 'blue'
text_color(ht)[, 2] <- 'red'
doc %<>% show_add(ht, 'Text color')

ht <- hux(Head1 = LETTERS[1:3], Head2 = letters[1:3], Head3 = 1:3, add_colnames = TRUE)
ft <- as_FlexTable(ht, header_rows = 1)
doc %<>% show_add(ft, 'Single header row')
colspan(ht)[1, 1] <- 3
ft <- as_FlexTable(ht, header_rows = 1)
doc %<>% show_add(ft, 'Single header row with colspan')
ft <- as_FlexTable(ht, header_rows = 2)
doc %<>% show_add(ft, 'Two header rows with colspan')
ht[4, 1] <- 'A footnote'
colspan(ht)[4, 1] <- 3
ft <- as_FlexTable(ht, header_rows = 1, footer_rows = 1)
doc %<>% show_add(ft, 'Header and footer with colspan')

ht <- ht_orig
ht <- set_all_borders(ht, , , 1)
colspan(ht)[2,1] <- 2
doc %<>% show_add(ht, 'Body colspan')
colspan(ht)[2,1] <- 1
rowspan(ht)[2,1] <- 2
doc %<>% show_add(ht, 'Body rowspan')

ht <- hux('Long title' = 1:4, 'Long title 2' = 1:4, add_colnames = TRUE)
rotation(ht)[1:5, ] <- 90
ft <- as_FlexTable(ht, header_rows = 2, footer_rows = 1)
doc %<>% show_add(ft, 'Rotation: trying all rows, first 2 are headers, last is footer')

writeDoc(doc, file = 'scripts/flextable-test.docx')
