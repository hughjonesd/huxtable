
devtools::install_github('hughjonesd/huxtable') # paradoxically, travis doesn't install huxtable
Sys.setenv(COVERAGE="yes")
all_created <- c(huxtable:::huxtable_cell_attrs, huxtable:::huxtable_col_attrs,
  huxtable:::huxtable_row_attrs, huxtable:::huxtable_table_attrs, 'make_getter_setters')
all_created <- paste0("^", all_created)
cov_result <- covr::package_coverage(function_exclusions = all_created)

covr::codecov(coverage = cov_result)
# to do it manually, run the below, with token = "your token from https://codecov.io/gh/hughjonesd/huxtable/settings"; then make sure you delete the token!
# covr::codecov(coverage = cov_result, token = "")
Sys.unsetenv("COVERAGE")
