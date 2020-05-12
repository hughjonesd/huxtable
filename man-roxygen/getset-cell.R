
# Documentation template for cell attributes

<%
more_alias_str <- ''
if (exists('morealiases')) {
  morealiases <- strsplit(morealiases, ' ')[[1]]
  more_alias_str <- c(outer(morealiases, c('', '<-'), paste0))
  more_alias_str <- c(more_alias_str,
        sprintf('set_%s', morealiases),
        sprintf('map_%s', morealiases))
  more_alias_str <- paste(more_alias_str, collapse = ' ')
}

default_property <- huxtable::get_default_properties(attr_name)[[1]]
if (is.list(default_property)) default_property <- default_property[[1]]
if (typeof(default_property) == "character") default_property <- sprintf("\"%s\"", default_property)

# this is the function default
default_str <- if (exists("default")) paste("=", default) else ""

%>

#' @title <%= attr_desc %>
#'
#' @description
#' Functions to get or set the *<%= tolower(attr_desc) %>* property of huxtable cells.
#'
#' @usage
#' <%= attr_name %>(ht)
#' <%= attr_name %>(ht) <- value
#' set_<%= attr_name %>(ht, row, col, value <%= default_str %>)
#' map_<%= attr_name %>(ht, row, col, fn)
#'
#' @param ht A huxtable.
#' @param value <%= value_param_desc %>
#'   <% if (! exists('NA_does_not_reset')) {%>
#'   Set to `NA` to reset to the default, which is `<%= default_property %>`.
#'   <% } else { %>
#'   Note that setting to `NA` does not reset to the default.
#'   <% } %>
#' @param row A row specifier. See [rowspecs] for details.
#' @param col An optional column specifier.
#' @param fn A mapping function. See [mapping-functions] for details.
#'
#' @return For `<%= attr_name %>`, the `<%= attr_name %>` property.
#' For `set_<%= attr_name %>` and `map_<%= attr_name %>`, the modified huxtable.
#'
#' @aliases <%= attr_name %><- set_<%= attr_name %> map_<%= attr_name %> <%= more_alias_str %>
#' @name <%= attr_name %>

