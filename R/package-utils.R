#' @import assertthat
NULL

#' Register an S3 method on package load
#'
#' @noRd
register_s3_method <- function(pkg, generic, class = "huxtable") {
  assert_that(is.string(pkg), is.string(generic))
  fun <- get(paste0(generic, ".", class), envir = parent.frame())

  if (pkg %in% loadedNamespaces()) {
    registerS3method(generic, class, fun, envir = asNamespace(pkg))
  }
  setHook(packageEvent(pkg, "onLoad"), function(...) {
    registerS3method(generic, class, fun, envir = asNamespace(pkg))
  })
}


#' Assert presence of suggested package and optional version
#'
#' @noRd
assert_package <- function(fun, package, version = NULL) {
  if (!requireNamespace(package, quietly = TRUE)) {
    stop(glue::glue(
      "`{fun}` requires the \"{package}\" package. To install, type:\n",
      "install.packages(\"{package}\")"
    ))
  }
  if (!is.null(version)) {
    cur_ver <- utils::packageVersion(package)
    if (cur_ver < version) {
      stop(glue::glue(
        "`{fun}` requires version {version} or higher of the \"{package}\" ",
        "package. You have version {cur_ver} installed. To update the package,",
        "type:\n",
        "install.packages(\"{package}\")"
      ))
    }
  }
}
