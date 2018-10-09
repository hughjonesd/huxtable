

#' Create huxtable of descriptive statistics
#'
#' `skim_hux` uses [skimr::skim()] to create a table of descriptive statistics.
#'
#' @param x A data frame or skimmable object.
#' @param ... Not currently used.
#'
#' @return A pre-formatted huxtable.
#' @export
#'
#' @examples
#' if (! requireNamespace("skimr")) {
#'   stop("Please install \"skimr\" to run this example")
#' }
#'
#' skim_hux(mtcars)
#' skim_hux(iris)
skim_hux <- function (x, ...) {
  assert_package("skim_hux", "skimr")

  current <- skimr::get_skimmers()
  skimr::skim_with_defaults()
  skimr::skim_with(
          factor = list(ordered = NULL),
          numeric = list(hist = NULL)
        )

  x <- skimr::skim_to_wide(x)

  skimr::skim_with(.list = current)

  title_case <- function(x) {
    s <- strsplit(x, " ")[[1]]
    paste(toupper(substring(s, 1, 1)), substring(s, 2),
          sep = "", collapse = " ")
  }

  new_type_idx <- which(! duplicated(x$type))

  x <- x[sort(c(seq_len(nrow(x)), new_type_idx)), ] # duplicate first rows with new type
  new_type_idx <- which(! duplicated(x$type)) # recalculate indexes

  new_colnames <- gsub("_", " ", colnames(x))
  new_colnames <- sapply(new_colnames, title_case)

  # Convert rows into title rows:
  x[new_type_idx, 2] <- sapply(x[new_type_idx, ]$type, title_case) # $ to "pull"
  x[new_type_idx, -2] <- ""
  x <- x[, -1]

  x <- as_hux(x)
  colspan(x)[new_type_idx, 1]       <- ncol(x)
  bottom_border(x)[new_type_idx, 1] <- 0.4
  italic(x)[new_type_idx, 1]        <- TRUE
  top_padding(x)[new_type_idx, 1]   <- 20

  real_cn <- colnames(x)
  x <- rbind(new_colnames[-1], x, copy_cell_props = FALSE)
  colnames(x) <- real_cn

  bottom_border(x)[1, ] <- 0.4
  bold(x)[1, ]          <- TRUE

  x
}
