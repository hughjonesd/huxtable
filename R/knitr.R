
# knitr-related functions --------------------------------------------------------------------------


#' Print a huxtable within knitr
#'
#' @param x A huxtable.
#' @param options Not used.
#' @param ... Not used.
#'
#' @details
#' knitr calls [knitr::knit_print()] on objects when they are printed in a knitr (or RMarkdown) document.
#' The method for `huxtable` objects guesses the appropriate output format and
#' prints itself out appropriately. You can override the output format by setting
#' `options("huxtable.knitr_output_format")`.
#'
#' @family knit_print
#' @seealso [huxtable-options]
knit_print.huxtable <- function (x, options, ...) {
  # guess... runs assert_package for knitr
  of <- getOption("huxtable.knitr_output_format", guess_knitr_output_format())
  call_name <- switch(of,
        latex  = "to_latex",
        html   = "to_html",
        pptx   = ,
        docx   = "as_flextable",
        md     = "to_md",
        screen = "to_screen",
        rtf    = "to_rtf",
        { # default
        warning(glue::glue(
            'Unrecognized output format "{of}". Using `to_screen` to print huxtables.\n',
            'Set options("huxtable.knitr_output_format") manually to ',
            '"latex", "html", "rtf", "docx", "pptx", "md" or "screen".'))
          "to_screen"
        })

  res <- do.call(call_name, list(x))

  res <- switch(of,
            latex = {
              latex_deps <- report_latex_dependencies(quiet = TRUE)
              tenv <- tabular_environment(x)
              if (tenv %in% c("tabulary", "longtable")) latex_deps <- c(latex_deps,
                list(rmarkdown::latex_dependency(tenv)))
              knitr::asis_output(res, meta = latex_deps)
            },
            html = knitr::asis_output(htmlPreserve(res)),
            rtf  = knitr::raw_output(res),
            pptx = ,
            docx = knitr::knit_print(res),
            knitr::asis_output(res)
          )

  return(res)
}

# see zzz.R
#' Print data frames in knitr using huxtable
#'
#' @inherit knit_print.huxtable params
#'
#' @details
#' `huxtable` defines a `knit_print` method for `data.frame`s. This converts the data frame
#' to a huxtable, with `add_colnames = TRUE`, themes it using [theme_plain()] and prints it.
#' It also tries to set a few intelligent defaults, e.g. wrapping long columns and setting
#' an appropriate width.
#' To turn this behaviour off, set `options(huxtable.knit_print_df = FALSE)`. To change the theme, set
#' `options("huxtable.knit_print_df_theme")` to a one-argument function which should return the huxtable.
#'
#' @family knit_print
#' @seealso [huxtable-options]
#' @examples
#' \dontrun{
#' # in your knitr document
#' mytheme <- function (ht) {
#'   ht <- set_all_borders(ht, 0.4)
#'   ht <- set_all_border_colors(ht,
#'         "darkgreen")
#'   ht <- set_background_color(ht,
#'         evens, odds, "salmon")
#'   ht
#' }
#'
#' options(huxtable.knit_print_df_theme
#'       = mytheme)
#' # groovy!
#' data.frame(
#'         a = 1:5,
#'         b = 1:5
#'       )
#' }
knit_print.data.frame <- function(x, options, ...) {
  if (! isTRUE(getOption("huxtable.knit_print_df", TRUE))) {
    NextMethod() # probably calls knit_print.default
  } else {
    ht <- smart_hux_from_df(x)
    df_theme <- getOption("huxtable.knit_print_df_theme", theme_plain)
    assert_that(is.function(df_theme))
    ht <- df_theme(ht)
    # we are now jumping down the class hierarchy, so do this rather than NextMethod():
    knitr::knit_print(ht)
  }
}


#' Guess knitr output format
#'
#' Convenience function which tries to guess the ultimate output from knitr and rmarkdown.
#'
#' @return "html", "latex", or something else. If we are not in a knitr document, returns an empty
#'   string.
#' @export
#'
#' @examples
#' \dontrun{
#' # in a knitr document
#' guess_knitr_output_format()
#' }
guess_knitr_output_format <- function() {
  # this is on hold until I'm sure I want 'markdown' to be interpreted as HTML
  # if (utils::packageVersion("knitr") >= "1.17.8") {
  #   # delegate to knitr
  #   if (knitr::is_latex_output()) return("latex")
  #   if (knitr::is_html_output()) return("html")
  #   return("")
  # }
  assert_package("guess_knitr_output_format", "knitr")
  assert_package("guess_knitr_output_format", "rmarkdown")
  of <- knitr::opts_knit$get("out.format")
  if (is.null(of) || of == "markdown") {
    of <- knitr::opts_knit$get("rmarkdown.pandoc.to")
    if (is.null(of)) {
      knit_in <- knitr::current_input()
      if (is.null(knit_in)) return("")
      of <- rmarkdown::default_output_format(knit_in)
      of <- of$name
    }
  }
  if (of == "tufte_handout") of <- "latex"
  if (of == "tufte_html") of <- "html"
  of <- sub("_.*", "", of)
  if (of == "html4") of <- "html" # bookdown
  if (of %in% c("ioslides", "revealjs", "slidy")) of <- "html"
  if (of %in% c("beamer", "pdf")) of <- "latex"

  of
}


# copied from htmltools. Works in the context of knitr and rmarkdown
htmlPreserve <- function (x) {
  x <- paste(x, collapse = "\r\n")

  if (nzchar(x)) sprintf("<!--html_preserve-->%s<!--/html_preserve-->", x) else x
}
