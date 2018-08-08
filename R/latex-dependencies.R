
# Tools for managing LaTeX dependencies ------------------------------------------------------------

#' @import assertthat
NULL


huxtable_latex_dependencies <- list(
  list(name = 'array'),
  list(name = 'caption'),
  list(name = 'graphicx'),
  list(name = 'siunitx'),
  list(name = 'xcolor', options = 'table'),
  list(name = 'multirow'),
  list(name = 'hhline'),
  list(name = 'calc'),
  list(name = 'tabularx')
  #list(name = 'arydshln')
)


#' Tools for LaTeX dependencies
#'
#' `report_latex_dependencies` prints out and/or returns a list of LaTeX dependencies for adding
#' to a LaTeX preamble.
#'
#' @param quiet Logical. For `report_latex_dependencies`, suppress printing of dependencies.
#'   For `check_latex_dependencies`, suppress messages.
#' @param as_string Logical: return dependencies as a string.
#'
#' @return If `as_string` is `TRUE`, `report_latex_dependencies` returns a string of
#'   `"\\\\usepackage\\{...\\}"` statements; otherwise it returns a list of
#'   `rmarkdown::latex_dependency` objects, invisibly.
#' @export
#'
#' @examples
#' report_latex_dependencies()
#'
report_latex_dependencies <- function(quiet = FALSE, as_string = FALSE) {
  assert_that(is.flag(quiet), is.flag(as_string))

  report <- sapply(huxtable_latex_dependencies, function(ld) {
    package_str <- '\\usepackage'
    if (! is.null(ld$options)) {
      options_str <- paste(ld$options, collapse = ',')
      package_str <- paste0(package_str, '[', options_str, ']')
    }
    package_str <- paste0(package_str, '{', ld$name, '}\n')
    package_str
  })
  if (! quiet) {
    cat(paste0(report, collapse = ''))
    cat('% These are LaTeX packages. You can install them using your LaTex management software,\n')
    cat('% or by running `huxtable::install_latex_dependencies()` from within R.\n')
    cat('% Other packages may be required if you use non-standard tabulars (e.g. tabulary).')
  }

  if (as_string) {
    return(paste0(report, collapse = ''))
  } else {
    assert_package('report_latex_dependencies', 'rmarkdown')
    huxtable_latex_dependencies <- lapply(huxtable_latex_dependencies, function (x) {
      rmarkdown::latex_dependency(x$name, options = x$options)
    })
    return(invisible(huxtable_latex_dependencies))
  }
}


#' Tools for LaTeX dependencies
#'
#' `check_latex_dependencies` checks whether the required LaTeX packages are installed.
#' @rdname report_latex_dependencies
#' @export
#' @return `check_latex_dependencies()` returns `TRUE` or `FALSE`.
#' @examples
#' \dontrun{
#' check_latex_dependencies()
#' }
check_latex_dependencies <- function (quiet = FALSE) {
  ld <- report_latex_dependencies(quiet = TRUE)
  ld <- vapply(ld, `[[`, character(1), 'name')
  ld <- setdiff(ld, c('graphicx', 'calc', 'array'))
  if (requireNamespace('tinytex', quietly = TRUE)) {
    pkgs <- tinytex::tl_pkgs()
  } else {
    warning('R package tinytex not found, trying to check packages directly with tlmgr')
    pkgs <- system2('tlmgr', c('info',  '--list', '--only-installed', '--data', 'name'),
      stdout = TRUE)
    pkgs <- gsub('[.].*', '', pkgs)
  }
  if (all(ld %in% pkgs)) {
    if (! quiet) message('All LaTeX packages found.')
    return(TRUE)
  } else {
    missing_pkgs <- setdiff(ld, pkgs)
    if (! quiet) message('The following LaTeX packages were not found:\n',
      paste(missing_pkgs, collapse = ', '), '\n',
      'Install them using your latex package manager or via install_latex_dependencies().')
    return(FALSE)
  }
}


#' Tools for LaTeX dependencies
#'
#' `install_latex_dependencies` is a utility function to install the LaTeX packages
#' that huxtable requires. It calls [tinytex::tlmgr_install()] if possible,
#' or `tlmgr install` directly.
#' @return `install_latex_dependencies` returns `TRUE` if `tlmgr` returns 0.
#' @export
#' @rdname report_latex_dependencies
#' @examples
#'
#' \dontrun{
#'   install_latex_dependencies()
#' }
install_latex_dependencies <- function () {
  ld <- report_latex_dependencies(quiet = TRUE)
  ld <- vapply(ld, `[[`, character(1), 'name')
  ld <- setdiff(ld, c('graphicx', 'calc', 'array'))
  message('Trying to install packages: ', paste(ld, collapse = ', '))
  message('If this fails, try running the following on the command line ',
    '(you may need admin permissions):')
  message('  tlmgr install ', paste(ld, collapse = ' '))
  if (requireNamespace('tinytex', quietly = TRUE)) {
    tinytex::tlmgr_install(ld) == 0
  } else {
    warning('R package tinytex not found, trying to install packages directly with tlmgr')
    message(paste('tlmgr', 'install', ld))
    system2('tlmgr', c('install', ld)) == 0
  }
}
