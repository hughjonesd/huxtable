

# functions for @evalRd and @evalNamespace in roxygen


make_namespace_S3_entries <- function (accessors) {
  entries <- lapply(accessors, function (getter) {
    setter <- paste0("\"", getter, "<-\"")
    paste0("S3method(", c(getter, setter), ", huxtable)")
  })

  unlist(entries)
}


make_exports <- function (properties, with_map = FALSE) {
  fun_templates <- c("%s", "\"%s<-\"", "set_%s", if (with_map) "map_%s")
  funs <- c(outer(fun_templates, properties, sprintf))

  paste0("export(", funs ,")")
}


make_border_aliases <- function (suffix) {
  lrtb <- c("left", "right", "top", "bottom")
  fns <- outer(
    c("map", "set"),
    lrtb,
    FUN = paste,
    "border",
    sep = "_"
  )
  fns <- paste0(fns, suffix)
  fns <- c(fns, paste0(lrtb, "_border", suffix, "<-"))
  paste("@aliases", paste(fns, collapse = " "))
}
