
# Documentation template for cell attributes

<%
more_alias_str <- ''
if (exists('morealiases')) {
  morealiases <- strsplit(morealiases, ' ')[[1]]
  more_alias_str <- c(outer(morealiases, c('', '<-'), paste0))
  more_alias_str <- c(more_alias_str, paste0('set_', morealiases))
  more_alias_str <- paste(more_alias_str, collapse = ' ')
}

default_property <- huxtable::get_default_properties(attr_name)[[1]]
if (is.list(default_property)) default_property <- default_property[[1]]
if (typeof(default_property) == "character") default_property <- sprintf("\"%s\"", default_property)
%>

#' @title <%= attr_desc %>
#'
#' @description
#' Functions to get or set the *<%= tolower(attr_desc) %>* property of huxtable cells.
#'
#' @usage
#' <%= attr_name %>(ht)
#' <%= attr_name %>(ht) <- value
#' set_<%= attr_name %>(ht, row, col, value, byrow = FALSE)
#' set_<%= attr_name %>_by(ht, row, col, fn)
#'
#' @param ht A huxtable.
#' @param value <%= value_param_desc %> Set to \code{NA} to reset to the default, which is
#'   \code{<%= default_property %>}.
#' @param row A row specifier. See \code{\link{rowspecs}} for details.
#' @param col An optional column specifier.
#' @param byrow If \code{TRUE}, fill in values by row rather than by column.
#' @param fn A "by" function. See \code{\link{set-by}} for details.
#'
#' @return For \code{<%= attr_name %>}, the \code{<%= attr_name %>} property.
#' For \code{set_<%= attr_name %>}, the \code{ht} object.
#'
#' @aliases <%= attr_name %><- set_<%= attr_name %> set_<%= attr_name %>_by <%= more_alias_str %>
#' @name <%= attr_name %>

