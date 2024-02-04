
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
#' read the vignette with `vignette("huxtable")`.
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
#' * `options("huxtable.add_colnames")` sets the default value for
#'   `add_colnames` in [huxtable()] and [as_huxtable()]. As of version 5.0.0, this
#'   defaults to `TRUE`.
#'
#' * `options("huxtable.print")` sets the print method for
#'   huxtable objects. See [print.huxtable()].
#'
#' * `options("huxtable.knitr_output_format")` overrides the default output format
#'   when huxtable objects are printed by knitr. Set to "html", "latex", "md" or
#'   "screen". If `NULL` (the default), huxtable guesses the format using
#'   [guess_knitr_output_format()].
#'
#' * `options("huxtable.autolabel")`. If `TRUE`, (the default) automatically
#'   sets `label()` from the knitr chunk label, if there is one.
#'
#' * `options("huxtable.color_screen")`. If `TRUE` and package `crayon` is
#'   available, huxtables will be printed in color on screen.
#'
#' * `options("huxtable.bookdown")`. Set to `TRUE` within a bookdown document to
#'   automatically print bookdown-style labels. If unset, huxtable will try to
#'   guess if we are in a bookdown document.
#'
#' * `options("huxtable.knit_print_df")`. If `TRUE`, data frames
#'   in knitr will be pretty-printed using huxtable. This option defaults to
#'   `TRUE` only if huxtable is attached to the search path using
#'   [library()]; not if huxtable is merely loaded (e.g. imported by another
#'   package).
#'
#' * `options("huxtable.knit_print_df_theme")`. A function applied to data frames
#'   before printing in knitr. The function should take one argument (a data
#'   frame) and return a huxtable. Defaults to [theme_plain()].
#'
#' * `options("huxtable.autoformat")` sets the default value for `autoformat` in
#'   [huxtable()] and [as_huxtable()]. It defaults to `TRUE`.
#'
#' * `options("huxtable.latex_use_fontspec")`. If `TRUE`, use the "fontspec"
#'   package, which allows you to use the same font names in TeX and HTML. This
#'   requires the the xetex or xelatex engine, which can be set using an .rmd
#'   header option. Note that [quick_pdf()] may use pdflatex. The default is
#'   `FALSE`.
#'
#' * `options("huxtable.long_minus")`. If `TRUE`, prints long minus signs
#'   for numbers. The default is `FALSE`. In LaTeX output, this option is
#'   overridden by `options("huxtable.latex_siunitx_align")`.
#'
#' * `options("huxtable.latex_siunitx_align")`. If `TRUE`, uses the `\tablenum`
#'   macro from the "siunitx" package to align numbers when `align(ht)` is `"."`
#'   or similar. See [align()] for details. The default is `FALSE`.
#'
#'   `options("huxtable.quarto_process")`. If `TRUE`, enables quarto processing
#'   of HTML tables. This overrides some huxtable styles, but may allow quarto
#'   to do other things, e.g. process citations correctly. The default is `FALSE`.
#'
#' * `options("huxtable.autoformat_number_format")` and
#'   `options("huxtable.autoformat_align")` are lists. The list names are base R
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
#' * I get a LaTeX error when I try to compile my document!
#'
#'   Have you installed the LaTeX packages you need? LaTeX packages are different
#'   from R packages. Run [check_latex_dependencies()] to find out if you are
#'   missing any. Then install them using your system's LaTeX management
#'   application. Or you can try [install_latex_dependencies()].
#'
#'   In some rmarkdown and LaTeX formats, you also need to add LaTeX dependencies
#'   manually. Run [report_latex_dependencies()] and add
#'   the output to your LaTeX preamble, or in Rmarkdown formats, add it to the
#'   rmarkdown header like this:
#'
#'   ```
#'   header-includes:
#'     - \usepackage{array}
#'     - \usepackage{caption}
#'     ... et cetera
#'   ```
#'
#' * Huxtable isn't working in my Rmarkdown `beamer_presentation` slides.
#'
#'   You may need to set the beamer "fragile" option, like this:
#'
#'   ```
#'   # Slide title {.fragile}
#'   ```
#'
#' * Numbers in my cells look weird!
#'
#'   You can change numeric formatting using [number_format()]. Base R options
#'   like [`scipen`][base::options()] usually have no effect.
#
#' * How can I use HTML, TeX etc. in my table?
#'
#'    Use [escape_contents()]:
#'
#'    ```r
#'    jams |>
#'         add_footnote("These jams are <i>tasty</i>!") |>
#'         set_escape_contents(final(1), everywhere, FALSE) |>
#'         quick_html()
#'    ```
#'
#'    Alternatively you might consider using markdown in cells, with
#'    [set_markdown_contents()].
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
#' * How can I get line breaks in my cells?
#'
#'   Just insert a line break `"\n"` in the cell contents. Then make sure that
#'   [width()] is set and [wrap()] is `TRUE` (it is by default).
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
#'   \@ref(tab:my-table-label).
#'   ```
#'
#' * How do I refer to tables in quarto?
#'
#'   In quarto versions up to 1.3, or when compiling to HTML and
#'   other formats, simply use quarto cell labels
#'   like `label: tbl-foo` and refer to them via `@tbl-foo`.
#'
#'   In quarto versions 1.4 and above, when compiling to PDF,
#'   quarto cross-referencing no longer works, and labels starting with
#'   `tbl-` will cause an error. (This is a quarto
#'   issue.) Instead, set labels within huxtable using [label()] or
#'   [set_label()] and refer to them with TeX-only referencing using
#'   `\ref{label}`. You must also set a caption, either via quarto or via
#'   huxtable.
#'
#'   Here's an example:
#'
#'   ````
#'   A reference to Table \ref{tab:jams}.
#'
#'   ```{r}
#'   label(jams) <- "tab:jams"
#'   caption(jams) <- "Some jams"
#'   jams
#'   ```
#'   ````
#'
#'  If you really need cross-referencing for both PDF and other output
#'  formats, either downgrade to quarto 1.3, use a different package,
#'  or write code to emit appropriate references.
#'
#' * I called `library(huxtable)` and now my `data.table` objects are getting
#'   printed!
#'
#'   Set `options(huxtable.knit_print_df = FALSE)`.
#'
#' * How can I set a property on an arbitrary group of cells?
#'
#'   If you can't use the [mapping-functions] interface, and you want to
#'   set a property for multiple cells that aren't all in the same rows
#'   and/or columns, you could use a little-known fact about R subsetting.
#'   If you subset `ht[x]` where `x` is two-column numeric matrix, then
#'   each row of `x` indexes a single `(row, column)` cell. So, for example,
#'   here's how to set the background color of cells `(2,1)`, `(1, 3)` and
#'   `(4, 2)` of a huxtable:
#'
#'   ```
#'   indices <- matrix(c(2, 1, 1, 3, 4, 2), ncol = 2, byrow = TRUE)
#'   background_color(jams)[indices] <- "orange"
#'   ```
#'
#'   Another useful trick sets properties on the diagonal, using [diag()]:
#'
#'   ```
#'   diag(background_color(jams)) <- "grey"
#'   ```
#'
#' * I have another problem.
#'
#'   If you have a bug - i.e. there is something wrong with the software -
#'   or a feature request, please report it to
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
