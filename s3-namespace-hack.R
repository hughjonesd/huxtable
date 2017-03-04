


register_s3_exports <- function() {
  cat('Running hack to register s3 exports with NAMESPACE\n')
  library(huxtable)
  NAMESPACE <- file.path('.', "NAMESPACE")
  nsl <- readLines(NAMESPACE)

  for (attr in c(huxtable:::huxtable_cell_attrs, huxtable:::huxtable_col_attrs,
        huxtable:::huxtable_row_attrs, huxtable:::huxtable_table_attrs)) {
    s3s <- c(attr, paste0('"', attr, '<-"'), paste0('set_', attr))


    f <- function (x) ! any(grepl(paste('S3method(', x), nsl, fixed = TRUE))
    s3s <- Filter(f, s3s)
    for (x in s3s) {
      nsl <- c(nsl, roxygen2:::export_s3_method(c(x, 'huxtable')))
    }
  }
  writeLines(nsl, NAMESPACE)
}


