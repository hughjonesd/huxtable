
# Documentation template for cell attributes

<%
more_alias_str <- ''
if (exists('morealiases')) {
  morealiases <- strsplit(morealiases, ' ')[[1]]
  more_alias_str <- c(outer(morealiases, c('', '<-'), paste0))
  more_alias_str <- c(more_alias_str,
        sprintf('set_%s', morealiases),
        sprintf('set_%s_by', morealiases))
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
#'
#' @param ht A huxtable.
#' @param value <%= value_param_desc %> Set to `NA` to reset to the default, which is
#'   `<%= default_property %>`.
#' @param row A row specifier. See [rowspecs] for details.
#' @param col An optional column specifier.
#' @param byrow If `TRUE`, fill in values by row rather than by column.
#' @param fn A mapping function. See [mapping-functions] for details.
#'
#' @return For `<%= attr_name %>`, the `<%= attr_name %>` property.
#' For `set_<%= attr_name %>` and `set_<%= attr_name %>_by`, the modified huxtable.
#'
#' @aliases <%= attr_name %><- set_<%= attr_name %> set_<%= attr_name %>_by <%= more_alias_str %>
#' @name <%= attr_name %>

