# Frequently Asked Questions, including how to get help

A FAQ of common issues.

## Details

- I get a LaTeX error when I try to compile my document!

  Have you installed the LaTeX packages you need? LaTeX packages are
  different from R packages. Run
  [`check_latex_dependencies()`](report_latex_dependencies.md) to find
  out if you are missing any. Then install them using your system's
  LaTeX management application. Or you can try
  [`install_latex_dependencies()`](report_latex_dependencies.md).

  In some rmarkdown and LaTeX formats, you also need to add LaTeX
  dependencies manually. Run
  [`report_latex_dependencies()`](report_latex_dependencies.md) and add
  the output to your LaTeX preamble, or in Rmarkdown formats, add it to
  the rmarkdown header like this:

      header-includes:
        - \usepackage{array}
        - \usepackage{caption}
        ... et cetera

- Huxtable isn't working in my Rmarkdown `beamer_presentation` slides.

  You may need to set the beamer "fragile" option, like this:

      # Slide title {.fragile}

- Numbers in my cells look weird!

  You can change numeric formatting using
  [`number_format()`](number_format.md). Base R options like
  [`scipen`](https://rdrr.io/r/base/options.html) usually have no
  effect.

- How can I use HTML, TeX etc. in my table?

  Use [`escape_contents()`](escape_contents.md):

      jams |>
           add_footnote("These jams are <i>tasty</i>!") |>
           set_escape_contents(final(1), everywhere, FALSE) |>
           quick_html()

  Alternatively you might consider using markdown in cells, with
  [`set_markdown_contents()`](set_markdown_contents.md).

- I ran `caption(ht) <- "Something"` and got an error message:

      Error in UseMethod("caption<-") :
      no applicable method for 'caption<-' applied to an object of class "c('huxtable',   'data.frame')"

  You may have loaded another package with a `caption` method, e.g.
  "xtable". Try loading huxtable after xtable.

- How can I get line breaks in my cells?

  Just insert a line break `"\n"` in the cell contents. Then make sure
  that [`width()`](width.md) is set and [`wrap()`](wrap.md) is `TRUE`
  (it is by default).

- How can I change the font size, font etc. of captions?

  There are no direct commands for this. You have to use raw
  HTML/TeX/other commands within the caption itself. For example to have
  a bold caption in HTML, you might do something like:

      set_caption(jams, "<b>Jam Prices</b>")

- How do I refer to tables in bookdown?

  As of version 4.3.0, this is handled automatically for you. Just set
  the label using [`label()`](label.md), then in markdown text do e.g.:

      \@ref(tab:my-table-label).

- How do I refer to tables in quarto?

  In quarto versions up to 1.3, or when compiling to HTML and other
  formats, simply use quarto cell labels like `label: tbl-foo` and refer
  to them via `@tbl-foo`.

  In quarto versions 1.4 and above, when compiling to PDF, quarto
  cross-referencing no longer works. Instead, set labels within huxtable
  using [`label()`](label.md) or [`set_label()`](label.md) and refer to
  them with TeX-only referencing using `\ref{label}`. You must also set
  a caption.

  Here's an example:

      A reference to Table \ref{tbl-jams}.

      ```{r}
      label(jams) <- "tbl-jams"
      caption(jams) <- "Some jams"
      jams
      ```

If you really need cross-referencing for both PDF and other output
formats, either downgrade to quarto 1.3, use a different package, or
write code to emit appropriate references.

- I called [`library(huxtable)`](https://hughjonesd.github.io/huxtable/)
  and now my `data.table` objects are getting printed!

  Set `options(huxtable.knit_print_df = FALSE)`.

- How can I set a property on an arbitrary group of cells?

  If you can't use the [mapping-functions](mapping-functions.md)
  interface, and you want to set a property for multiple cells that
  aren't all in the same rows and/or columns, you could use a
  little-known fact about R subsetting. If you subset `ht[x]` where `x`
  is two-column numeric matrix, then each row of `x` indexes a single
  `(row, column)` cell. So, for example, here's how to set the
  background color of cells `(2,1)`, `(1, 3)` and `(4, 2)` of a
  huxtable:

      indices <- matrix(c(2, 1, 1, 3, 4, 2), ncol = 2, byrow = TRUE)
      background_color(jams)[indices] <- "orange"

  Another useful trick sets properties on the diagonal, using
  [`diag()`](https://rdrr.io/r/base/diag.html):

      diag(background_color(jams)) <- "grey"

- I have another problem.

  If you have a bug - i.e. there is something wrong with the software -
  or a feature request, please report it to
  <https://github.com/hughjonesd/huxtable/issues>. Otherwise, ask a
  question on [StackOverflow](https://stackoverflow.com) or
  <https://forum.posit.co>. That way, other people will benefit from the
  answers you get.

- Can I email you directly?

  I'd rather you asked on a public website. If you then email me a link,
  I may be able to help.

## See also

Useful links:

- <https://hughjonesd.github.io/huxtable/>

- Report bugs at <https://github.com/hughjonesd/huxtable/issues>

## Author

**Maintainer**: David Hugh-Jones <davidhughjones@gmail.com>
