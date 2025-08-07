skip_without_pandoc <- function() {
  if (!rmarkdown::pandoc_available("1.12.3")) skip("Not testing, pandoc >= 1.12.3 is not available")
}


skip_on_R_CMD_check <- function() {
  skip_if(exists("we_are_in_R_CMD_check"), "Not testing, code doesn't play well with R CMD check")
  skip_if(Sys.getenv("COVERAGE") != "", "Not testing, code doesn't play well with R CMD check")
}


require_temp_artefacts_dir <- function() {
  if (dir.exists("temp-artefacts")) {
    return(TRUE)
  }
  if (dir.create("temp-artefacts", showWarnings = FALSE)) {
    return(TRUE)
  } else {
    skip("Couldn't create temp-artefacts dir")
  }
}
