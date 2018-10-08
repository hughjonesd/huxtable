
# Documentation template for attribute examples

<% if (! exists('subscript')) subscript <- '' %>
#' @examples
#'
#' data(jams)
#'
#' <%= attr_name %>(jams)<%= subscript %> <-  <%= attr_val %>
#' <%= attr_name %>(jams)
#' <%= if (exists('extra')) gsub('## *', '\n#\' ', extra) %>
