

# Documentation template for attribute examples
<% if (! exists('subscript')) subscript <- '' %>
#' @examples
#'
#' data(jams)
#' jams <- as_hux(jams)
#' <%= attr_name %>(jams)<%= subscript %> <-  <%= attr_val %>
#' <%= attr_name %>(jams)
#' <%= if (exists('extra')) gsub('## *', '\n#\' ', extra) %>
