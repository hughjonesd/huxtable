## code to prepare `jams` dataset goes here

jams <- hux(
  Type = c("Strawberry", "Raspberry", "Plum"),
  Price = c(1.90, 2.10, 1.80),
  add_colnames = TRUE
  )
number_format(jams)[,2] <- 2
usethis::use_data(jams, overwrite = TRUE, compress = FALSE)
