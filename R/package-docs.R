
#' Quick introduction to huxtable
#'
#' Huxtable is a package for creating HTML and LaTeX tables. It provides similar
#' functionality to xtable, with a simpler interface.
#'
#' @section Quick start:
#'
#' To create a huxtable object, use [huxtable()] or [as_huxtable()]:
#'
#' ```
#'  library(huxtable)
#'  employees <- huxtable(
#'          Names    = c("Hadley", "Yihui", "Dirk"),
#'          Salaries = c(1e5, 1e5, 1e5),
#'          add_colnames = TRUE
#'        )
#'  car_hux <- as_hux(mtcars)
#'
#' ```
#'
#' You can then set properties which affect how the huxtable is displayed:
#'
#' ```
#'  # make the first row bold:
#'  bold(employees)[1, ] <- TRUE
#'
#'  # change the font size everywhere:
#'  font_size(employees) <- 10
#' ```
#'
#' Or you can use a tidyverse style with the pipe operator:
#'
#' ```
#' library(magrittr)
#' employees <- employees %>%
#'       set_font_size(10) %>%
#'       set_bold(1, everywhere, TRUE)
#'
#' ```
#'
#' For more information, see \href{https://hughjonesd.github.io/huxtable/}{the website} or
#' read the vignette with `vignette('huxtable')`.
#'
#' See [huxtable-FAQ] for frequently asked questions, including ways to get
#' help.
#'
#' To report a bug, or suggest an enhancement, visit
#' \href{https://github.com/hughjonesd/huxtable/issues}{github}.
#'
#' @name huxtable-package
#' @docType package
NULL


#' Package options
#'
#' Huxtable has several options.
#'
#' * `options('huxtable.add_colnames')` sets the default value for
#'   `add_colnames` in [huxtable()] and [as_huxtable()]. As of version 5.0.0, this
#'   defaults to `TRUE`.
#'
#' * `options('huxtable.print')` sets the print method for
#'   huxtable objects. See [print.huxtable()].
#'
#' * `options('huxtable.knitr_output_format')` overrides the default output format
#'   when huxtable objects are printed by knitr. Set to "html", "latex", "md" or
#'   "screen". If `NULL` (the default), huxtable guesses the format using
#'   [guess_knitr_output_format()].
#'
#' * `options('huxtable.autolabel')`. If `TRUE`, (the default) automatically
#'   sets `label()` from the knitr chunk label, if there is one.
#'
#' * `options('huxtable.color_screen')`. If `TRUE` and package `crayon` is
#'   available, huxtables will be printed in color on screen.
#'
#' * `options('huxtable.bookdown')`. Set to `TRUE` within a bookdown document to
#'   automatically print bookdown-style labels. If unset, huxtable will try to
#'   guess if we are in a bookdown document.
#'
#' * `options('huxtable.knit_print_df')`. If `TRUE`, data frames
#'   in knitr will be pretty-printed using huxtable. This option defaults to
#'   `TRUE` only if huxtable is attached to the search path using
#'   [library()]; not if huxtable is merely loaded (e.g. imported by another
#'   package).
#'
#' * `options('huxtable.knit_print_df_theme')`. A function applied to data frames
#'   before printing in knitr. The function should take one argument (a data
#'   frame) and return a huxtable. Defaults to [theme_plain()].
#'
#' * `options('huxtable.autoformat')` sets the default value for `autoformat` in
#'   [huxtable()] and [as_huxtable()]. It defaults to `TRUE`.
#'
#' * `options('huxtable.latex_use_fontspec')`. If `TRUE`, use the "fontspec"
#'   package, which allows you to use the same font names in TeX and HTML. This
#'   requires the the xetex or xelatex engine, which can be set using an .rmd
#'   header option. Note that [quick_pdf()] may use pdflatex. The default is
#'   `FALSE`.
#'
#' * `options('huxtable.autoformat_number_format')` and
#'   `options('huxtable.autoformat_align')` are lists. The list names are base R
#'   classes. [huxtable()] with `autoformat = TRUE` will set `number_format()` and
#'   `align()` for data columns according to the corresponding list values. For
#'   example, to center-align `Date` objects you could set
#'   `"huxtable.autoformat_align"` to something like
#'   `list(..., Date = "center", ...)`.
#'
#' @name huxtable-options
#' @docType package
NULL



#' Frequently Asked Questions, including how to get help
#'
#' A FAQ of common issues.
#'
#' * LaTeX output isn't working.
#'
#'   Have you installed the LaTeX packages you need? LaTeX packages are different
#'   from R packages. Run [check_latex_dependencies()] to find out if you are
#'   missing any. Then install them using your system's LaTeX management
#'   application. Or you can try [install_latex_dependencies()].
#'
#' * Numbers in my cells look weird!
#'
#'   You can change numeric formatting using [number_format()]. Base R options
#'   like [`scipen`][base::options()] usually have no effect.
#'
#' * I ran `caption(ht) <- "Something"` and got an error message:
#'
#'   ```
#'   Error in UseMethod("caption<-") :
#'   no applicable method for 'caption<-' applied to an object of class "c('huxtable',   'data.frame')"
#'   ```
#'
#'   You may have loaded another package with a `caption` method, e.g. "xtable".
#'   Try loading huxtable after xtable.
#'
#' * How can I change the font size, font etc. of captions?
#'
#'   There are no direct commands for this. You have to use raw HTML/TeX/other
#'   commands within the caption itself. For example to have a bold caption in
#'   HTML, you might do something like:
#'
#'   ```
#'   set_caption(jams, "<b>Jam Prices</b>")
#'   ```
#'
#' * How do I refer to tables in bookdown?
#'
#'   As of version 4.3.0, this is handled automatically for you. Just
#'   set the label using [label()], then in markdown text do e.g.:
#'
#'   ```
#'   \\@ref(tab:my-table-label).
#'
#'   ```
#'
#' * I have another problem.
#'
#'   If you have a bug - i.e. a problem with the software - or have a feature
#'   request, please report it to
#'   <https://github.com/hughjonesd/huxtable/issues>.
#'   Otherwise, ask a question on [StackOverflow](https://stackoverflow.com) or
#'   <https://community.rstudio.com>. That way, other people will benefit from
#'   the answers you get.
#'
#' * Can I email you directly?
#'
#'   I'd rather you asked on a public website. If you then email me a link, I
#'   may be able to help.
#'
#' @name huxtable-FAQ
#' @docType package
NULL



#' Changes to the huxtable package
#'
#' This help page simply gives the contents of NEWS.md.
#'
#' @includeRmd NEWS.md
#' @name huxtable-news
#' @docType package
NULL
