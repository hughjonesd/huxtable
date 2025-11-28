# Quick introduction to huxtable

Huxtable is a package for creating HTML and LaTeX tables. It provides
similar functionality to xtable, with a simpler interface.

## Quick start

To create a huxtable object, use [`huxtable()`](huxtable.md) or
[`as_huxtable()`](as_huxtable.md):

     library(huxtable)
     employees <- huxtable(
             Names    = c("Hadley", "Yihui", "Dirk"),
             Salaries = c(1e5, 1e5, 1e5),
             add_colnames = TRUE
           )
     car_hux <- as_hux(mtcars)

You can then set properties which affect how the huxtable is displayed:

     # make the first row bold:
     bold(employees)[1, ] <- TRUE

     # change the font size everywhere:
     font_size(employees) <- 10

Or you can use a tidyverse style with the pipe operator:

    library(magrittr)
    employees <- employees %>%
          set_font_size(10) %>%
          set_bold(1, everywhere, TRUE)

For more information, see [the
website](https://hughjonesd.github.io/huxtable/) or read the vignette
with [`vignette("huxtable")`](../articles/huxtable.md).

See [huxtable-FAQ](huxtable-FAQ.md) for frequently asked questions,
including ways to get help.

To report a bug, or suggest an enhancement, visit
[github](https://github.com/hughjonesd/huxtable/issues).

## See also

Useful links:

- <https://hughjonesd.github.io/huxtable/>

- Report bugs at <https://github.com/hughjonesd/huxtable/issues>

## Author

**Maintainer**: David Hugh-Jones <davidhughjones@gmail.com>
