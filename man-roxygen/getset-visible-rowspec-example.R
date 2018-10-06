# Documentation template for attribute examples where results can be shown in terminal
#' @examples
#'
#' ht <- huxtable(a = 1:3, b = 3:1)
#' set_<%= attr_name %>(ht, <%= attr_val %>)
#' set_<%= attr_name %>(ht, 1:2, 1, <%= attr_val %>)
#' set_<%= attr_name %>(ht, 1:2, 1:2, c(<%= attr_val %>, <%= attr_val2 %>), byrow = TRUE)
