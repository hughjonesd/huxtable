
# Documentation template for cell attributes

<%
more_alias_str <- ''
if (exists('morealiases')) {
  morealiases <- strsplit(morealiases, ' ')[[1]]
  more_alias_str <- c(outer(morealiases, c('', '<-', '.huxtable', '<-.huxtable'), paste0))
  more_alias_str <- c(more_alias_str, paste0('set_', morealiases))
  more_alias_str <- paste(more_alias_str, collapse = ' ')
}
%>

#' @title Get or Set <%= attr_desc %>
#'
#' @description
#' Functions to get or set the <%= tolower(attr_desc) %> property of huxtable table cells.
#'
#' @usage
#' <%= attr_name %>(ht)
#' <%= attr_name %>(ht) <- value
#' set_<%= attr_name %>(ht, row, col, value)
#'
#' @param ht A huxtable.
#' @param value <%= value_param_desc %> Set to \code{NA} to reset to the default.
#' @param row A row specifier.
#' @param col A column specifier.
#'
#' @return For \code{<%= attr_name %>}, the \code{<%= attr_name %>} attribute.
#' For \code{set_<%= attr_name %>}, the \code{ht} object.
#'
#' @aliases <%= attr_name %><- set_<%= attr_name %> <%= attr_name %>.huxtable <%= attr_name %><-.huxtable <%= more_alias_str %>
#' @name <%= attr_name %>

