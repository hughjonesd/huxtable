
#' Prices of 3 jams
#'
#' A huxtable of jams.
#'
#' @format A huxtable with 4 rows and 2 columns ("Type" and "Price").
"jams"


.onLoad <- function(libname, pkgname) {
  set_default_option("huxtable.add_colnames", TRUE)
  set_default_option("huxtable.autoformat", TRUE)
  set_default_option("huxtable.autolabel", TRUE)
  set_default_option("huxtable.bookdown", NULL)
  set_default_option("huxtable.color_screen",
        requireNamespace("crayon", quietly = TRUE))
  set_default_option("huxtable.knit_print_df_theme", theme_plain)
  set_default_option("huxtable.print", print_screen)
  set_default_option("huxtable.latex_use_fontspec", FALSE)
  set_default_option("huxtable.latex_siunitx_align", FALSE)
  set_default_option("huxtable.long_minus", FALSE)
  set_default_option("huxtable.quarto_process", FALSE)
  set_default_option("huxtable.autoformat_number_format", list(
          integer = 0,
          numeric = "%.3g",
          complex = "%.3g"
        ))
  set_default_option("huxtable.autoformat_align", list(
          numeric = getOption("OutDec", "."),
          complex = getOption("OutDec", "."),
          integer = "right",
          Date    = "right",
          POSIXct = "right",
          POSIXlt = "right"
        ))

  if (requireNamespace("knitr", quietly = TRUE)) {
    register_s3_method("knitr", "knit_print")
    register_s3_method("knitr", "knit_print", class = "data.frame")
  }

  if (requireNamespace("dplyr", quietly = TRUE)) {
    register_s3_method("dplyr", "arrange")
    register_s3_method("dplyr", "filter")
    register_s3_method("dplyr", "mutate")
    register_s3_method("dplyr", "slice")
    register_s3_method("dplyr", "transmute")
    if (utils::packageVersion("dplyr") <= "0.8.5") {
      register_s3_method("dplyr", "rename")
      register_s3_method("dplyr", "select")
    }
  }
}


.onAttach <- function (libname, pkgname) {
  set_default_option("huxtable.knit_print_df", TRUE)
}


set_default_option <- function (opt, value) {
  ol <- list(getOption(opt, value))
  names(ol) <- opt
  options(ol)
}

# This is for a mocked binding in test-print.R
requireNamespace <- NULL
