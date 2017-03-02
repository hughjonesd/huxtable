

#' @title Get or Set <%= attr_desc %>
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
#' @aliases <%= attr_name %><- set_<%= attr_name %>
#' @name <%= attr_name %>
