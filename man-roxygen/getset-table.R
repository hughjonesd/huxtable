
<%
more_alias_str <- ''
if (exists('morealiases')) {
  morealiases <- strsplit(morealiases, ' ')[[1]]
  more_alias_str <- c(outer(morealiases, c('', '<-'), paste0))
  more_alias_str <- c(more_alias_str, paste0('set_', morealiases))
  more_alias_str <- paste(more_alias_str, collapse = ' ')
}

default_property <- huxtable::get_default_properties(attr_name)[[1]]
if (typeof(default_property) == "character") {
  default_property <- sprintf("\"%s\"", default_property)
}
%>

#' @usage
#' <%= attr_name %>(ht)
#' <%= attr_name %>(ht) <- value
#' set_<%= attr_name %>(ht, value)
#'
#' @param ht A huxtable.
#' @param value <%= value_param_desc %> Set to `NA` to reset to the default, which is
#'   `<%= default_property %>`.
#'
#' @return `<%= attr_name %>()` returns the `<%= attr_name %>` property.
#' `set_<%= attr_name %>()` returns the modified huxtable.
#'
#' @aliases <%= attr_name %><- set_<%= attr_name %> <%= more_alias_str %>
#' @name <%= attr_name %>


