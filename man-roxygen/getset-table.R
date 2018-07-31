
# Documentation template for table attributes

<%
more_alias_str <- ''
if (exists('morealiases')) {
  morealiases <- strsplit(morealiases, ' ')[[1]]
  more_alias_str <- c(outer(morealiases, c('', '<-', '.huxtable', '<-.huxtable'), paste0))
  more_alias_str <- c(more_alias_str, paste0('set_', morealiases))
  more_alias_str <- paste(more_alias_str, collapse = ' ')
}

default_property <- huxtable::get_default_properties(attr_name)[[1]]
if (typeof(default_property) == "character") default_property <- sprintf("\"%s\"", default_property)

%>

#' @title <%= attr_desc %>
#'
#' @description
#' Functions to get or set the table-level *<%= tolower(attr_desc) %>* property of a huxtable.
#'
#' @usage
#' <%= attr_name %>(ht)
#' <%= attr_name %>(ht) <- value
#' set_<%= attr_name %>(ht, value)
#'
#' @param ht A huxtable.
#' @param value <%= value_param_desc %> Set to \code{NA} to reset to the default, which is
#'   \code{<%= default_property %>}.
#'
#' @return For \code{<%= attr_name %>}, the \code{<%= attr_name %>} property.
#' For \code{set_<%= attr_name %>}, the \code{ht} object.
#'
#' @aliases <%= attr_name %><- set_<%= attr_name %> <%= attr_name %>.huxtable <%= attr_name %><-.huxtable <%= more_alias_str %>
#' @name <%= attr_name %>


