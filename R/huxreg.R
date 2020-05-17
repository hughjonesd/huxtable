
#' @import assertthat
#' @importFrom stats nobs
NULL


#' @importFrom generics tidy
#' @export
generics::tidy


#' @importFrom generics glance
#' @export
generics::glance


#' Create a huxtable to display model output
#'
#' @param ... Models, or a single list of models. Names will be used as column headings.
#' @param error_format How to display uncertainty in estimates. See below.
#' @param error_pos Display uncertainty "below", to the "right" of, or in the "same" cell as estimates.
#' @param number_format Format for numbering. See [number_format()] for details.
#' @param align Alignment for table cells. Set to a single character to align on this character.
#' @param ci_level Confidence level for intervals. Set to `NULL` to not calculate confidence intervals.
#' @param tidy_args List of arguments to pass to [broom::tidy()]. You can also pass a list of lists;
#'   if so, the nth element will be used for the nth column.
#' @param stars Levels for p value stars. Names of `stars` are symbols to use. Set to `NULL` to not show stars.
#' @param bold_signif Where p values are below this number, cells will be displayed in bold. Use `NULL` to turn off
#'   this behaviour.
#' @param borders Thickness of inner horizontal borders. Set to 0 for no borders.
#' @param outer_borders Thickness of outer (top and bottom) horizontal borders. Set to 0 for no borders.
#' @param note Footnote for bottom cell, which spans all columns. \code{{stars}} will be replaced by a note about
#'   significance stars. Set to `NULL` for no footnote.
#' @param statistics A vector of summary statistics to display. Set to `NULL` to show all available
#'   statistics. To change display names, name the `statistics` vector:
#'   `c("Displayed title" = "statistic_name", ...)`
#' @param coefs A vector of coefficients to display. Overrules `omit_coefs`. To change display names,
#'   name the `coef` vector: `c("Displayed title" = "coefficient_name", ...)`
#' @param omit_coefs Omit these coefficients.
#'
#' @details
#' Models must have a [generics::tidy()] method defined, which should return "term", "estimate",
#' "std.error", "statistic" and "p.value". The `"broom"` package provides methods for many model
#' objects. If the `tidy` method does not have a `conf.int` option,
#' `huxreg` will calculate confidence intervals itself, using a normal approximation.
#'
#' If `...` has names or contains a single named list, the names will be used for column headings.
#' Otherwise column headings will be automatically created.
#'
#' If the `coef` and/or `statistics` vectors have names, these will be used for row headings. If
#' different values of `coef` have the same name, the corresponding rows will be merged in the
#' output.
#'
#' `statistics` should be column names from [generics::glance()]. You can also use `"nobs"` for the
#' number of observations. If `statistics` is `NULL` then all columns from `glance` will be used. To
#' use no columns, set `statistics = character(0)`.
#'
#' `error_format` is a string to be interpreted by [glue::glue()]. Terms in parentheses will be
#' replaced by computed values. You can use any columns returned
#' by `tidy`: typical columns include `statistic`, `p.value`, `std.error`, as well as `conf.low`
#' and `conf.high` if you have set `ci_level`. For example, to show confidence intervals, you
#' could write \code{error_format = "{conf.low} to {conf.high}"}.
#'
#' @section Fixing p values manually:
#'
#' If you wish to use e.g. robust standard errors, you can pass results from e.g.
#' [lmtest::coeftest()] into `huxreg`, since these objects have `tidy` methods.
#' Alternatively, to manually insert your own statistics, see [tidy_override()].
#'
#' @return A huxtable object.
#' @export
#'
#' @examples
#' if (! requireNamespace("broom")) {
#'   stop("Please install 'broom' to run this example.")
#' }
#'
#' lm1 <- lm(mpg ~ cyl, mtcars)
#' lm2 <- lm(mpg ~ cyl + hp, mtcars)
#' glm1 <- glm(I(mpg > 20) ~ cyl, mtcars,
#'       family = binomial)
#'
#' huxreg(lm1, lm2, glm1)
#'
huxreg <- function (
        ...,
        error_format    = "({std.error})",
        error_pos       = c("below", "same", "right"),
        number_format   = "%.3f",
        align           = ".",
        ci_level        = NULL,
        tidy_args       = NULL,
        stars           = c("***" = 0.001, "**" = 0.01, "*" = 0.05),
        bold_signif     = NULL,
        borders         = 0.4,
        outer_borders   = 0.8,
        note            = if (is.null(stars)) NULL else "{stars}.",
        statistics      = c("N" = "nobs", "R2" = "r.squared", "logLik", "AIC"),
        coefs           = NULL,
        omit_coefs      = NULL
      ) {
  requireNamespace("broom", quietly = TRUE)
  suppressPackageStartupMessages(requireNamespace("broom.mixed", quietly = TRUE))
  if (utils::packageVersion("broom") < package_version("0.5.1")) {
    warning("`huxreg` requires `broom` version 0.5.1 or greater.")
  }

  # prepare parameters
  if (! missing(bold_signif)) assert_that(is.number(bold_signif))
  if (! missing(ci_level)) assert_that(is.number(ci_level))
  assert_that(is.null(stars) || is.numeric(stars))
  models <- list(...)
  if (inherits(models[[1]], "list")) models <- models[[1]]
  mod_col_headings <- names_or(models, paste0("(", seq_along(models), ")"))
  error_pos <- match.arg(error_pos)
  if (! is.null(tidy_args) && ! is.list(tidy_args[[1]])) tidy_args <- rep(list(tidy_args), length(models))

  # create list of tidy data frames, possibly with confidence intervals
  my_tidy <- function (n, ci_level = NULL) {
    # pre-tidied models are returned as is
    if (class(models[[n]])[[1]] == "tbl_df") return(models[[n]])
    args <- if (! is.null(tidy_args)) tidy_args[[n]] else list()
    args$x <- models[[n]]
    if (! is.null(ci_level)) {
      args$conf.int <- TRUE
      args$conf.level <- ci_level
    }
    do.call(tidy, args)
  }
  tidy_with_ci <- function (n) {
    if (has_builtin_ci(models[[n]])) return(my_tidy(n, ci_level = ci_level))
    tidied <- my_tidy(n) # should return "estimate" and "std.error"
    cbind(tidied, make_ci(tidied[, c("estimate", "std.error")], ci_level))
  }
  tidied <- lapply(seq_along(models), if (is.null(ci_level)) my_tidy else tidy_with_ci)

  # select coefficients
  my_coefs <- unique(unlist(lapply(tidied, function (x) {
    if (! "term" %in% names(x)) stop("No 'terms' column in result returned from `tidy()`")
    x$term
  })))

  if (! missing(omit_coefs)) my_coefs <- setdiff(my_coefs, omit_coefs)
  if (! missing(coefs)) {
    if (! all(coefs %in% my_coefs)) stop("Unrecognized coefficient names: ",
          paste(setdiff(coefs, my_coefs), collapse = ", "))
    my_coefs <- coefs
  }
  coef_names <- names_or(my_coefs, my_coefs)

  # select appropriate rows
  tidied <- lapply(tidied, merge, x = data.frame(term = my_coefs, stringsAsFactors = FALSE), all.x = TRUE, by = "term",
        sort = FALSE)
  tidied <- lapply(tidied, function (x) {
    x$term[! is.na(match(x$term, my_coefs))] <- coef_names[match(x$term, my_coefs)]
    x <- x[match(unique(coef_names), x$term), ]
  })
  coef_names <- unique(coef_names)

  # add stars to estimates
  if (! is.null(stars)) {
    names(stars) <- paste0(" ", names(stars))
    stars <- sort(stars)
    cutpoints <- c(0, stars, 1)
    symbols   <- c(names(stars), "")
    tidied <- lapply(tidied, function (x) {
      if (is.null(x$p.value)) {
        warning("tidy() does not return p values for models of class ", class(x)[1],
              "; significance stars not printed.")
        return (x)
      }
      x$estimate[ !is.na(x$estimate) ] <- with (x[! is.na(x$estimate), ],
              paste0(estimate,
              symnum(as.numeric(p.value), cutpoints = cutpoints, symbols = symbols, na = ""))
            )
      x
    })
  }

  # create error cells
  tidied <- lapply(tidied, function (x) {
    x$error_cell <- glue::glue_data(.x = x, error_format)
    x$error_cell[is.na(x$estimate)] <- ""
    x$estimate[is.na(x$estimate)] <- ""
    x
  })

  # cbind tidy data into a single data frame
  coef_col <- switch(error_pos,
    same  = paste,
    below = interleave,
    right = cbind
  )
  cols <- lapply(tidied, function (mod) coef_col(mod$estimate, mod$error_cell))
  cols <- Reduce(cbind, cols)

  # make the data frame a huxtable
  coef_hux <- huxtable(cols, add_colnames = FALSE)
  number_format(coef_hux) <- number_format
  if (! is.null(bold_signif)) {
    bold_cols <- lapply(tidied, function (mod) mod$p.value <= bold_signif)
    bold_cols <- switch(error_pos,
      same  = bold_cols,
      below = lapply(bold_cols, rep, each = 2),
      right = lapply(bold_cols, function (x) cbind(x, x))
    )
    bold_cols <- Reduce(cbind, bold_cols)
    bold(coef_hux) <- bold_cols
  }

  # create list of summary statistics
  all_sumstats <- lapply(models, function(m) {
    bg <- try(glance(m), silent = TRUE)
    bg <- if (inherits(bg, "try-error")) {
      warning(sprintf("Error calling `glance` on object of class %s:",
            class(m)[1]))
      warning(bg)
      NULL
    } else t(bg)
    nobs <- tryCatch(nobs(m, use.fallback = TRUE), error = function (e) NA)
    x <- as.data.frame(rbind(nobs = nobs, bg), stringsAsFactors = FALSE)
    colnames(x) <- "value" # some glance objects have a rowname
    x$stat  <- rownames(x)
    x$class <- c(class(nobs), sapply(bg, class))
    x
  })

  # select summary statistics and cbind into a single data frame
  stat_names <- unique(unlist(lapply(all_sumstats, function (x) x$stat)))
  if (! is.null(statistics)) {
    if (! all(statistics %in% stat_names)) warning("Unrecognized statistics: ",
          paste(setdiff(statistics, stat_names), collapse = ", "),
          "\nTry setting `statistics` explicitly in the call to `huxreg()`")
    stat_names <- statistics[statistics %in% stat_names] # intersect would remove names
  }
  sumstats <- lapply(all_sumstats, merge, x = data.frame(stat = stat_names), by = "stat", all.x = TRUE, sort = FALSE)
  sumstats <- lapply(sumstats, function (x) x[match(stat_names, x$stat), ])
  ss_classes <- lapply(sumstats, function (x) x$class)
  sumstats <- lapply(sumstats, function (x) x$value)
  sumstats <- Reduce(cbind, sumstats)
  ss_classes <- Reduce(cbind, ss_classes)

  # create huxtable of summary statistics
  sumstats <- huxtable(sumstats, add_colnames = FALSE)
  number_format(sumstats) <- number_format
  number_format(sumstats)[ss_classes == "integer"] <- 0

  if (error_pos == "right") {
    sumstats2 <- as_hux(matrix("", nrow(sumstats), ncol(sumstats) * 2), add_colnames = FALSE)
    for (i in seq_len(ncol(sumstats))) {
      sumstats2[, i * 2 - 1] <- sumstats[, i]
    }
    sumstats <- sumstats2
  }
  coef_hux <- cbind(if (error_pos == "below") interleave(coef_names, "") else coef_names, coef_hux,
        copy_cell_props = FALSE)
  sumstats <- cbind(names_or(stat_names, stat_names), sumstats, copy_cell_props = FALSE)

  # create single huxtable from coefficients and summary statistics
  if (error_pos == "right") mod_col_headings <- interleave(mod_col_headings, "")
  mod_col_headings <- c("", mod_col_headings)
  result <- rbind(mod_col_headings, coef_hux, sumstats, copy_cell_props = FALSE)
  result <- set_header_rows(result, 1, TRUE)
  result <- set_header_cols(result, 1, TRUE)
  result <- set_bottom_border(result, final(), everywhere, outer_borders)
  result <- set_top_border(result, 1, everywhere, outer_borders)
  result <- set_bottom_border(result, c(1, nrow(coef_hux) + 1), -1, borders)
  colnames(result) <- c("names", names_or(models, paste0("model", seq_along(models))))
  if (error_pos == "right") result <- set_colspan(result, 1, evens, 2)
  align(result)[1, ]    <- "center"
  align(result)[-1, -1] <- align
  number_format(result)[, 1]  <- NA
  number_format(result)[1, ]  <- NA

  # add a table note
  if (! is.null(note)) {
    stars <- if (is.null(stars)) "" else paste0(names(stars), " p < ", stars, collapse = "; ")
    note <- gsub("%stars%", stars, note)
    note <- glue::glue(note)
    result <- add_footnote(result, note, border = NULL)
    result <- set_wrap(result, final(), 1, TRUE)
    result <- set_align(result, final(), 1, "left")
  }

  return(result)
}


names_or <- function (obj, alts) {
  nms <- names(obj)
  if (is.null(nms)) return(alts)
  return(ifelse(nzchar(nms), nms, alts))
}


interleave <- function (a, b) ifelse(seq_len(length(a) * 2) %% 2, rep(a, each = 2), rep(b, each = 2))


make_ci <- function(tidied, ci_level) {
  a <- c( (1 - ci_level) / 2, 1 - (1 - ci_level) / 2)
  fac <- stats::qnorm(a)
  ci <- tidied$estimate + tidied$std.error %o% fac
  colnames(ci) <- c("conf.low", "conf.high")
  ci
}


has_builtin_ci <- function (x) {
  objs <- sapply(class(x), function (y) {
    try(utils::getS3method("tidy", y,
        envir = getNamespace("broom")), silent = TRUE)
  })
  obj <- Find(function(x) class(x) == "function", objs)
  if (is.null(obj)) return(FALSE)
  argnames <- names(formals(obj))
  all(c("conf.int", "conf.level") %in% argnames)
}


#' Change a model's `tidy` output
#'
#' Use `tidy_override` and `tidy_replace` to provide your own p values,
#' confidence intervals etc. for a model.
#'
#' @param x A model with methods defined for [generics::tidy()] and/or [generics::glance()].
#' @param ... In `tidy_override`, columns of statistics to replace `tidy` output. In
#'   `tidy` and `glance` methods, arguments passed on to the underlying model.
#' @param glance A list of summary statistics for `glance`.
#' @param extend Logical: allow adding new columns to `tidy(x)`?
#' @param object A `tidy_override` object.
#'
#' @details
#' `tidy_override` allows you to replace some columns of `tidy(x)` with your own data.
#'
#' @return An object that can be passed in to `huxreg`.
#'
#' @export
#'
#' @examples
#' if (! requireNamespace("broom", quietly = TRUE)) {
#'   stop("Please install 'broom' to run this example.")
#' }
#'
#' lm1 <- lm(mpg ~ cyl, mtcars)
#' fixed_lm1 <- tidy_override(lm1,
#'       p.value = c(.04, .12),
#'       glance = list(r.squared = 0.99))

#' huxreg(lm1, fixed_lm1)
#'
#' if (requireNamespace("nnet", quietly = TRUE)) {
#'   mnl <- nnet::multinom(gear ~ mpg, mtcars)
#'   tidied <- broom::tidy(mnl)
#'   mnl4 <- tidy_replace(mnl, tidied[tidied$y.level == 4, ])
#'   mnl5 <- tidy_replace(mnl, tidied[tidied$y.level == 5, ])
#'   huxreg(mnl4, mnl5, statistics = "nobs")
#' }
#'
tidy_override <- function (x, ..., glance = list(), extend = FALSE) {
  assert_that(is.flag(extend), is.list(glance))
  tidy_cols <- data.frame(..., stringsAsFactors = FALSE)
  structure(list(
          model = x,
          tidy_cols = tidy_cols,
          glance_elems = glance,
          extend = extend
        ),
        class = "tidy_override")
}

#' @export
#' @param tidied Data frame to replace the result of `tidy(x)`.
#' @rdname tidy_override
#' @details
#' `tidy_replace` allows you to replace the result of `tidy(x)` entirely.
tidy_replace <- function (x, tidied, glance = list()) {
  structure(list(
          model = x,
          tidy_data = tidied,
          glance_elems = glance(),
          extend = FALSE
        ),
        class = "tidy_override")
}


#' @export
#' @rdname tidy_override
tidy.tidy_override <- function (x, ...) {
  if (! is.null(x$tidy_data)) return(x$tidy_data)

  tidied <- try(tidy(x$model, ...), silent = TRUE)
  if (inherits(tidied, "try-error")) tidied <- data.frame()[seq_along(x$tidy_cols[[1]]), ]
  for (cn in names(x$tidy_cols)) {
    if (! x$extend && ! cn %in% names(tidied)) stop(glue::glue(
          "Column \"{cn}\" not found in results of `tidy()`"))
    tidied[[cn]] <- x$tidy_cols[[cn]]
  }

  return(tidied)
}


#' @export
#' @rdname tidy_override
glance.tidy_override <- function (x, ...) {
  sumstats <- try(glance(x$model, ...), silent = TRUE)
  if (inherits(sumstats, "try-error")) sumstats <- data.frame()[1, ] # 1 row no cols

  for (elem in names(x$glance_elems)) {
    if (! x$extend && ! elem %in% names(sumstats)) stop(glue::glue(
          "Element \"{elem}\" not found in results of `glance()`"))
    sumstats[[elem]] <- x$glance_elems[[elem]]
  }

  return(sumstats)
}

#' @export
#' @rdname tidy_override
nobs.tidy_override <- function (object, ...) {
  if ("nobs" %in% names(object$glance_elems)) return(object$glance_elems[["nobs"]])
  nobs(object$model)
}


