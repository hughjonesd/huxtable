
library(huxtable)
library(magrittr)
library(microbenchmark)

ht <- as_hux(matrix(1:20, 4, 5))
ht %<>%
      set_background_color(everywhere, evens, c('green', 'yellow', 'lightblue', 'pink')) %>%
      set_left_border(evens, everywhere, 1) %>%
      set_top_border(odds, everywhere, 1) %>%
      set_bottom_border(everywhere, odds, 1) %>%
      set_bottom_border(2, everywhere, 0)
ht

ht_long <- ht[rep(1:4, 10), ]
ht_wide <- ht[, rep(1:5, 10)]

mbs <- try(readRDS('mbs.Rds'))
if (inherits(mbs, 'try-error')) mbs <- list()
hv  <- paste0('huxtable-', as.character(packageVersion('huxtable')))
mbs[[hv]] <- microbenchmark(to_latex(ht), to_latex(ht_long), to_latex(ht_wide), times = 20)
saveRDS(mbs, 'mbs.Rds')
