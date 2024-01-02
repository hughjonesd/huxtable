library(huxtable)
htexample <- function(data) {

  tab = huxtable(data, add_colnames = FALSE)
  tab <- tab |>
    dplyr::mutate(
      x = (tab[["colc"]] - tab[["colp"]]) / tab[["colp"]]
    )

  evolmean <- tab[[nrow(tab), "x"]] |> as.numeric()

  tab <- add_colnames(tab)

  far_mean <- by_cases(
    . < 0 ~ "firebrick",
    . < evolmean / 1.1 ~ "springgreen3",
    . > evolmean * 1.1 ~ "springgreen4",
    TRUE ~ "black"
  )

  # formatage pct importants
  tab <- map_text_color(tab, -(1), final(1), far_mean)
  tab
}


t1 = data.frame(

  seg=letters[1:5],
  colp = 12:16,
  colc = 20:24
)

htexample(t1)
