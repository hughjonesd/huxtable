
#' Create a huxtable to display model output
#'
#' @param ... Models, or a single list of models.
#' @param error_style How to display uncertainty in estimates. One or more of 'stderr', 'ci' (confidence interval), 'statistic' or 'pvalue'.
#' @param error_pos Display uncertainty 'below', to the 'right' of, or in the 'same' cell as estimates.
#' @param number_format Format for numbering. See \code{\link{number_format}} for details.
#' @param pad_decimal Character for decimal point; columns will be right-padded to align these.
#'   Set to \code{NA} to turn off padding. See \code{\link{pad_decimal}} for details.
#' @param ci_level Confidence level for intervals.
#' @param stars Levels for p value stars. Names of \code{stars} are symbols to use.
#' @param bold_signif Where p values are below this number, cells will be displayed in bold. Use \code{NULL} to turn off
#'   this behaviour.
#' @param borders Logical: add horizontal borders in appropriate places?
#' @param note Footnote for bottom cell, which spans all columns. \code{\%stars\%} will be replaced by a note about
#'   significance stars. Set to \code{NULL} for no footnote.
#' @param statistics Summary statistics to display.
#' @param coefs Display only these coefficients. Overrules \code{omit_coef}.
#' @param omit_coefs Omit these coefficients.
#'
#' @details
#' Models must have a \code{\link[broom]{tidy}} method defined, which should return 'term', 'estimate', 'std.error',
#' 'statistic' and 'p.value'. If the \code{tidy} method does not have a \code{conf.int} option, \code{huxreg} will
#' calculate confidence intervals itself, using a normal approximation.
#'
#' If \code{...} is a named list, the names will be used for column headings. If the \code{coef} and/or
#'  \code{statistics} vectors have names, these will be used for row headings. If different values of \code{coef}
#'  have the same name, the corresponding rows will be merged in the output.
#'
#' Each element of \code{statistics} should be a column name from \code{\link[broom]{glance}}. You can also
#' use 'nobs' for the number of observations.
#'
#' @return A huxtable object.
#' @export
#'
#' @examples
#' set.seed(27101975)
#' dfr <- data.frame(a = rnorm(100), b = rnorm(100))
#' dfr$y <- dfr$a + rnorm(100)
#' lm1 <- lm(y ~ a, dfr)
#' lm2 <- lm(y ~ a + b, dfr)
#' glm1 <- glm(I(y > 0) ~ a, dfr, family = binomial)
#' huxreg(lm1, lm2, glm1)
huxreg <- function (
        ...,
        error_style     = c('stderr', 'ci', 'statistic', 'pvalue'),
        error_pos       = c('below', 'same', 'right'),
        number_format   = '%.3f',
        pad_decimal     = '.',
        ci_level        = 0.95,
        stars           = c('***' = 0.001, '**' = 0.01, '*' = 0.05),
        bold_signif     = NULL,
        borders         = TRUE,
        note            = '%stars%.',
        statistics      = c('N' = 'nobs', 'R2' = 'r.squared', 'logLik', 'AIC'),
        coefs           = NULL,
        omit_coefs      = NULL
      ) {
  if (! requireNamespace('broom', quietly = TRUE)) stop('huxreg requires the "broom" package. To install, type:\n',
        'install.packages("broom")')
  models <- list(...)
  if (inherits(models[[1]], 'list')) models <- models[[1]]
  mod_names <- names_or(models, bracket(seq_along(models)))
  if (missing(error_style)) error_style <- 'stderr'
  error_style <- sapply(error_style, match.arg, choices = eval(formals(huxreg)$error_style))
  error_pos <- match.arg(error_pos)

  tidy_with_ci <- function (obj) {
    if (has_builtin_ci(obj)) return(broom::tidy(obj, conf.int = TRUE, conf.level = ci_level))
    tidied <- broom::tidy(obj) # should return 'estimate' and 'std.error'
    cbind(tidied, make_ci(tidied[, c('estimate', 'std.error')], ci_level))
  }
  tidied <- lapply(models, if ('ci' %in% error_style) tidy_with_ci else broom::tidy)

  my_coefs <- unique(unlist(lapply(tidied, function (x) x$term)))
  if (! missing(omit_coefs)) my_coefs <- setdiff(my_coefs, omit_coefs)
  if (! missing(coefs)) {
    if (! all(coefs %in% my_coefs)) stop('Unrecognized coefficient names: ',
          paste(setdiff(coefs, my_coefs), collapse = ', '))
    my_coefs <- coefs
  }
  coef_names <- names_or(my_coefs, my_coefs)

  tidied <- lapply(tidied, merge, x = data.frame(term = my_coefs, stringsAsFactors = FALSE), all.x = TRUE, by = 'term',
        sort = FALSE)
  tidied <- lapply(tidied, function (x) {
    x$term[! is.na(match(x$term, my_coefs))] <- coef_names[match(x$term, my_coefs)]
    x <- x[match(unique(coef_names), x$term), ]
  })
  coef_names <- unique(coef_names)
  tidied <- lapply(tidied, function (x) {
    numcols <- sapply(x, is.numeric)
    x[, numcols] <- sapply(x[, numcols], format_number, number_format)
    x
  })
  if (! is.null(stars)) tidied <- lapply(tidied, function (x) {
    stars_arg <- c(0, stars, ' ' = 1)
    x$estimate[ !is.na(x$estimate) ] <- with (x[! is.na(x$estimate), ], paste(estimate, symnum(as.numeric(p.value),
          cutpoints = stars_arg, symbols = names(stars_arg)[-1], na = ' ')))
    x
  })

  tidied <- lapply(tidied, function (x) {
    x$error_cell <- make_error_cells(x, error_style)
    x
  })

  # now we cbind the models
  coef_col <- switch(error_pos,
    same  = function (est, se) ifelse(is.na(est), NA, paste(est, se)),
    below = interleave,
    right = cbind
  )
  cols <- lapply(tidied, function (mod) coef_col(mod$estimate, mod$error_cell))
  cols <- Reduce(cbind, cols)
  cols <- hux(cols)
  number_format(cols) <- number_format
  if (! is.null(bold_signif)) {
    bold_cols <- lapply(tidied, function (mod) mod$p.value <= bold_signif)
    bold_cols <- switch(error_pos,
      same  = bold_cols,
      below = lapply(bold_cols, rep, each = 2),
      right = lapply(bold_cols, function (x) cbind(x, x))
    )
    bold_cols <- Reduce(cbind, bold_cols)
    bold(cols) <- bold_cols
  }

  all_sumstats <- lapply(models, function(m) {
    bg <- try(broom::glance(m), silent = TRUE)
    bg <- if (class(bg) == 'try-error') {
      warning('No `glance` method for model of class ', class(m)[1])
      NULL
    } else t(bg)
    nobs <- nobs(m, use.fallback = TRUE)
    x <- as.data.frame(rbind(nobs = nobs, bg))
    x$stat  <- rownames(x)
    x$class <- c(class(nobs), sapply(bg, class))
    x
  })

  stat_names <- unique(unlist(lapply(all_sumstats, function (x) x$stat)))
  if (! is.null(statistics)) {
    if (! all(statistics %in% stat_names)) stop('Unrecognized statistics:',
      paste(setdiff(statistics, stat_names)), collapse = ', ')
    stat_names <- statistics
  }
  sumstats <- lapply(all_sumstats, merge, x = data.frame(stat = stat_names), by = 'stat', all.x = TRUE, sort = FALSE)
  sumstats <- lapply(sumstats, function (x) x[match(stat_names, x$stat), ])
  ss_classes <- lapply(sumstats, function (x) x$class)
  sumstats <- lapply(sumstats, function (x) x$V1)
  sumstats <- Reduce(cbind, sumstats)
  ss_classes <- Reduce(cbind, ss_classes)

  sumstats <- hux(sumstats)
  number_format(sumstats) <- number_format
  number_format(sumstats)[ss_classes == 'integer'] <- 0

  if (error_pos == 'right') {
    sumstats2 <- as_hux(matrix('', nrow(sumstats), ncol(sumstats) * 2))
    for (i in seq_len(ncol(sumstats))) {
      sumstats2[, i * 2 - 1] <- sumstats[, i]
    }
    sumstats <- sumstats2
  }
  cols <- cbind(if (error_pos == 'below') interleave(coef_names, '') else coef_names, cols,
        copy_cell_props = FALSE)
  sumstats <- cbind(names_or(stat_names, stat_names), sumstats, copy_cell_props = FALSE)

  if (error_pos == 'right') mod_names <- interleave(mod_names, '')
  mod_names <- c('', mod_names)
  result <- rbind(mod_names, cols, sumstats, copy_cell_props = FALSE)
  if (isTRUE(borders)) result <- set_bottom_border(result, c(1, 1 + nrow(cols), nrow(result)), everywhere, 1)
  colnames(result) <- mod_names # may fail
  if (error_pos == 'right') result <- set_colspan(result, 1, evens, 2)
  align(result)[1, ]    <- 'center'
  align(result)[-1, -1] <- 'right'
  pad_decimal(result)[-1, -1] <- pad_decimal

  if (! is.null(note)) {
    stars_note <- paste0(names(stars), ' p < ', stars, collapse = '; ')
    note <- gsub('%stars%', stars_note, note)
    result <- rbind(result, c(note, rep('', ncol(result) - 1)))
    result <- set_colspan(result, final(), 1, ncol(result))
    result <- set_wrap(result, final(), 1, TRUE)
    result <- set_align(result, final(), 1, 'left')
    result <- set_bottom_border(result, final(), everywhere, 0)
  }

  result
}


names_or <- function (obj, alts) {
  nms <- names(obj)
  if (is.null(nms)) return(alts)
  return(ifelse(nzchar(nms), nms, alts))
}


interleave <- function (a, b) ifelse(seq_len(length(a) * 2) %% 2, rep(a, each = 2), rep(b, each = 2))


bracket <-  function (x) if (length(x) > 0) paste0('(', x, ')') else character(0)


bracket2 <- function (x) if (length(x) > 0) paste0('[', x, ']') else character(0)


make_ci <- function(tidied, ci_level) {
  a <- c( (1 - ci_level) / 2, 1 - (1 - ci_level) / 2)
  fac <- stats::qnorm(a)
  ci <- tidied$estimate + tidied$std.error %o% fac
  colnames(ci) <- c('conf.low', 'conf.high')
  ci
}


has_builtin_ci <- function (x) {
  objs <- sapply(class(x), function (y) try(utils::getS3method('tidy', y), silent = TRUE))
  obj <- Find(function(x) class(x) == 'function', objs)
  if (is.null(obj)) return(FALSE)
  argnames <- names(formals(obj))
  all(c('conf.int', 'conf.level') %in% argnames)
}


make_error_cells <- function (tidied, error_style) {
  stderr    <- function (td) td$std.error
  statistic <- function (td) td$statistic
  pvalue    <- function (td) td$p.value
  ci        <- function (td) paste(td$conf.low, ' -- ', td$conf.high)
  error_funs <- lapply(error_style, as.symbol)
  strings <- lapply(error_funs, function (x) eval(bquote(.(x)(tidied))))
  names(strings) <- error_style
  strings <- do.call(cbind, strings)
  strings[, 1] <- bracket(strings[, 1])
  strings[, -1] <- bracket2(strings[, -1])
  strings <- apply(strings, 1, paste, collapse = ' ')
  strings[is.na(tidied$estimate)] <- NA

  strings
}
