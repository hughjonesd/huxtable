

#' @title Get or Set <%= attr_desc %>
#'
#' @usage
#' <%= attr_name %>(ht)
#' <%= attr_name %>(ht) <- value
#' set_<%= attr_name %>(ht, <%= rowcol %>, value)
#'
#' @param ht A huxtable.
#' @param value <%= value_param_desc %>
#' @param <%= rowcol %> A <%= rowcol %> specifier.
#'
#' @return For \code{<%= attr_name %>}, the \code{<%= attr_name %>} attribute. For \code{<%= attr_name %><-}, the
#' value of the right hand side (technically, the function itself returns the \code{ht} object, but this is an
#' implementation detail of how R handles replacement functions). For \code{set_<%= attr_name %>}, the \code{ht} object.
#'
#' @aliases <%= attr_name %><- set_<%= attr_name %>
#' @name <%= attr_name %>
