# Usage strings for cell properties

<%
default_str <- if (exists("default")) paste("=", default) else ""
%>

#' @usage
#' <%= attr_name %>(ht)
#' <%= attr_name %>(ht) <- value
#' set_<%= attr_name %>(ht, row, col, value <%= default_str %>)
#' map_<%= attr_name %>(ht, row, col, fn)
