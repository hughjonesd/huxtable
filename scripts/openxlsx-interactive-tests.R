
library(openxlsx)
library(magrittr)
library(huxtable)

doc <- openxlsx::createWorkbook()
show_add <- function(doc, ht, message) {
  if (is_hux(ht)) {
    caption(ht) <- message
    if (caption_pos(ht) == "top") caption_pos(ht) <- "bottom"
  }
  sheet <- substr(message, 1, 20)
  sheet <- make.names(sheet)

  if (is_hux(ht)) doc <- as_Workbook(ht, Workbook = doc, sheet = sheet)
}

ht_orig <- huxtable(a = c('Foo', 'Bar', 'Baz'), b = 2:4)

ht <- ht_orig
doc %<>% show_add(ht, 'Basic table')
huxtable::bold(ht)[1, 1:2] <- TRUE
huxtable::italic(ht)[1:2, 1] <- TRUE
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

ht <- ht_orig
ht <- set_all_borders(ht, 1)
colspan(ht)[2,1] <- 2
doc %<>% show_add(ht, 'Body colspan')
colspan(ht)[2,1] <- 1
rowspan(ht)[2,1] <- 2
doc %<>% show_add(ht, 'Body rowspan')
colspan(ht)[2,1] <- 2
rowspan(ht)[2,1] <- 2
doc %<>% show_add(ht, 'Body row and colspan')

ht <- hux('Long title' = 1:4, 'Long title 2' = 1:4, add_colnames = TRUE)
rotation(ht)[1, 1] <- 90
doc %<>% show_add(ht, 'Rotation 90 degrees')

ht <- ht_orig
ht[1, 1:2] <- "Some very very long long long text"
wrap(ht)[1, 1] <- TRUE
doc %<>% show_add(ht, 'wrap on and off')

ht <- ht_orig
caption_pos(ht) <- "topleft"
doc %<>% show_add(ht, 'topleft caption')
caption_pos(ht) <- "topright"
doc %<>% show_add(ht, 'topright caption')
caption_pos(ht) <- "topcenter"
doc %<>% show_add(ht, 'topcenter caption')
ht$c <- ht$d <- ht$e <- ht$f <- ht$a
caption_pos(ht) <- "top"
position(ht) <- "right"
doc %<>% show_add(ht, "caption determined by pos (right)")


saveWorkbook(doc, fp <- file.path("scripts", "openxlsx-test-output.xlsx"), overwrite = TRUE)
openXL(fp)
