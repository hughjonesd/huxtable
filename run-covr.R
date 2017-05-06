
devtools::install_github('hughjonesd/huxtable') # paradoxically, travis doesn't install huxtable
Sys.setenv(COVERAGE="yes")
all_created <- c(huxtable:::huxtable_cell_attrs, huxtable:::huxtable_col_attrs,
  huxtable:::huxtable_row_attrs, huxtable:::huxtable_table_attrs)
all_created <- paste0("^", all_created)
cov <- covr::package_coverage(function_exclusions = all_created)
covr::codecov(coverage = cov)
Sys.unsetenv("COVERAGE")