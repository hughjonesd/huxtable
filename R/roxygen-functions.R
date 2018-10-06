

# functions for @evalRd and @evalNamespace in roxygen


make_namespace_S3_entries <- function (accessors) {
  entries <- lapply(accessors, function (getter) {
    setter <- paste0('"', getter, '<-"')
    paste0('S3method(', c(getter, setter), ', huxtable)')
  })

  unlist(entries)
}


make_exports <- function (properties, with_by = FALSE) {
  fun_templates <- c('%s', '"%s<-"', 'set_%s')
  if (with_by) fun_templates <- c(fun_templates, 'set_%s_by')
  funs <- c(outer(fun_templates, properties, sprintf))

  paste0('export(', funs ,')')
}
