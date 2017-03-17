# Documentation template for attribute examples
#' @examples
#'
#' ht <- huxtable(a = 1:3, b = 3:1)
#' ht2 <- set_<%= attr_name %>(ht, <%= attr_val %>)
#' <%= attr_name %>(ht2)
#' ht3 <- set_<%= attr_name %>(ht, 1:2, 1, <%= attr_val %>)
#' <%= attr_name %>(ht3)
#' ht4 <- set_<%= attr_name %>(ht, 1:2, 1:2, c(<%= attr_val %>, <%= attr_val2 %>), byrow = TRUE)
#' <%= attr_name %>(ht4)
#' ht5 <- set_<%= attr_name %>(ht, where(ht == 1), <%= attr_val %>)
#' <%= attr_name %>(ht5)
