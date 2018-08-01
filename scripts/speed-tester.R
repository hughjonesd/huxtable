
library(huxtable)
library(magrittr)
library(microbenchmark)
library(git2r)

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

mbs <- try(readRDS('scripts/mbs.Rds'))
if (inherits(mbs, 'try-error')) mbs <- list()
hv <- as.character(packageVersion('huxtable'))
hv  <- paste0('huxtable-', hv)
if (grepl('9000', hv)) {
  sha <- substr(git2r::commits(n=1)[[1]]$sha, 1, 8)
  hv <- paste0(hv, '-', sha)
}

mbs[[hv]] <- summary(microbenchmark(
        to_latex(ht), to_latex(ht_long), to_latex(ht_wide),
        to_html(ht), to_html(ht_long), to_html(ht_wide),
        to_screen(ht), to_screen(ht_long), # to_screen(ht_wide), # hangs
        # as_flextable(ht), as_flextable(ht_long), as_flextable(ht_wide),
        # as_Workbook(ht), as_Workbook(ht_long), as_Workbook(ht_wide),
        times = 20
      ))
saveRDS(mbs, 'scripts/mbs.Rds')
