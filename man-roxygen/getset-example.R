

# Documentation template for attribute examples
<% if (! exists('subscript')) subscript <- '' %>
#' @examples
#'
#' ht <- huxtable(a = 1:3, b = 1:3)
#' <%= attr_name %>(ht)<%= subscript %> <-  <%= attr_val %>
#' <%= attr_name %>(ht)
#' <%= if (exists('extra')) gsub('## *', '\n#\' ', extra) %>
