
#' Create a huxtable to display model output
#'
#' @param ... Models, or a single list of models. Names will be used as column headings.
#' @param error_format How to display uncertainty in estimates. See below.
#' @param error_style Deprecated. One or more of 'stderr', 'ci' (confidence interval), 'statistic' or 'pvalue'.
#' @param error_pos Display uncertainty 'below', to the 'right' of, or in the 'same' cell as estimates.
#' @param number_format Format for numbering. See \code{\link{number_format}} for details.
#' @param pad_decimal Character for decimal point; columns will be right-padded to align these.
#'   Set to \code{NA} to turn off padding. See \code{\link{pad_decimal}} for details.
#' @param ci_level Confidence level for intervals. Set to \code{NULL} to not calculate confidence intervals.
#' @param stars Levels for p value stars. Names of \code{stars} are symbols to use. Set to \code{NULL} to not show stars.
#' @param bold_signif Where p values are below this number, cells will be displayed in bold. Use \code{NULL} to turn off
#'   this behaviour.
#' @param borders Thickness of inner horizontal borders. Set to 0 for no borders.
#' @param outer_borders Thickness of outer (top and bottom) horizontal borders. Set to 0 for no borders.
#' @param note Footnote for bottom cell, which spans all columns. \code{{stars}} will be replaced by a note about
#'   significance stars. Set to \code{NULL} for no footnote.
#' @param statistics Summary statistics to display. Set to \code{NULL} to show all available statistics.
#' @param coefs Display only these coefficients. Overrules \code{omit_coef}.
#' @param omit_coefs Omit these coefficients.
#'
#' @details
#' Models must have a \code{\link[broom]{tidy}} method defined, which should return 'term', 'estimate', 'std.error',
#' 'statistic' and 'p.value'. If the \code{tidy} method does not have a \code{conf.int} option, \code{huxreg} will
#' calculate confidence intervals itself, using a normal approximation.
#'
#' If \code{...} has names or contains a single named list, the names will be used for column headings. Otherwise column
#' headings will be automatically created. If the \code{coef} and/or \code{statistics} vectors have names, these will be
#' used for row headings. If different values of \code{coef} have the same name, the corresponding rows will be merged
#' in the output.
#'
#' Each element of \code{statistics} should be a column name from \code{\link[broom]{glance}}. You can also
#' use 'nobs' for the number of observations. If \code{statistics} is \code{NULL} then all columns of from \code{glance}
#' will be used. To use no columns, set \code{statistics = character(0)}.
#'
#' \code{error_format} is a string to be interpreted by \code{\link[glue]{glue}}. Terms in parentheses will be
#' replaced by computed values. You can use any columns returned
#' by \code{tidy}: typical columns include \code{statistic}, \code{p.value}, \code{std.error}, as well as \code{conf.low}
#' and \code{conf.high} if you have set \code{ci_level}. For example, to show confidence intervals, you
#' could do \code{error_format = "{conf.low} to {conf.high}"}
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
        error_format    = '({std.error})',
        error_style     = c('stderr', 'ci', 'statistic', 'pvalue'),
        error_pos       = c('below', 'same', 'right'),
        number_format   = '%.3f',
        pad_decimal     = '.',
        ci_level        = NULL,
        stars           = c('***' = 0.001, '**' = 0.01, '*' = 0.05),
        bold_signif     = NULL,
        borders         = 0.4,
        outer_borders   = 0.8,
        note            = '{stars}.',
        statistics      = c('N' = 'nobs', 'R2' = 'r.squared', 'logLik', 'AIC'),
        coefs           = NULL,
        omit_coefs      = NULL
      ) {
  if (! requireNamespace('broom', quietly = TRUE)) stop('huxreg requires the "broom" package. To install, type:\n',
        'install.packages("broom")')
  models <- list(...)
  if (inherits(models[[1]], 'list')) models <- models[[1]]
  mod_col_headings <- names_or(models, paste0("(", seq_along(models), ")"))
  error_pos <- match.arg(error_pos)
  if (! missing(error_style)) error_style <- sapply(error_style, match.arg, choices = eval(formals(huxreg)$error_style))

  tidy_with_ci <- function (obj) {
    if (has_builtin_ci(obj)) return(broom::tidy(obj, conf.int = TRUE, conf.level = ci_level))
    tidied <- broom::tidy(obj) # should return 'estimate' and 'std.error'
    cbind(tidied, make_ci(tidied[, c('estimate', 'std.error')], ci_level))
  }
  tidied <- lapply(models, if (is.null(ci_level)) broom::tidy else tidy_with_ci)

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
  if (! is.null(stars)) {
    tidied <- lapply(tidied, function (x) {
      stars_arg <- c(0, sort(stars), ' ' = 1)
      if (is.null(x$p.value)) {
        warning("tidy() does not return p values for models of class ", class(x)[1], "; significance stars not printed.")
        return (x)
      }
      x$estimate[ !is.na(x$estimate) ] <- with (x[! is.na(x$estimate), ], paste(estimate, symnum(as.numeric(p.value),
            cutpoints = stars_arg, symbols = names(stars_arg)[-1], na = ' ')))
      x
    })
  }

  if (! missing(error_style)) {
    formats <- list(stderr = '{std.error}', ci = '{conf.low} -- {conf.high}', statistic = '{statistic}',
          pvalue = '{p.value}')
    lbra <- rep('[', length(error_style))
    rbra <- rep(']', length(error_style))
    lbra[1] <- '('
    rbra[1] <- ')'
    error_format <- paste(lbra, formats[error_style], rbra, sep = '', collapse = ' ')
    warning(glue::glue("`error_style` is deprecated, please use `error_format = \"{error_format}\"` instead."))
  }
  tidied <- lapply(tidied, function (x) {
    x$error_cell <- glue::glue_data(.x = x, error_format)
    x$error_cell[is.na(x$estimate)] <- ''
    x$estimate[is.na(x$estimate)] <- ''
    x
  })

  # now we cbind the models
  coef_col <- switch(error_pos,
    same  = paste,
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
    colnames(x) <- 'value' # some glance objects have a rowname
    x$stat  <- rownames(x)
    x$class <- c(class(nobs), sapply(bg, class))
    x
  })

  stat_names <- unique(unlist(lapply(all_sumstats, function (x) x$stat)))
  if (! is.null(statistics)) {
    if (! all(statistics %in% stat_names)) stop('Unrecognized statistics: ',
          paste(setdiff(statistics, stat_names), collapse = ', '),
          '\nTry setting "statistics" explicitly in the call to huxreg()')
    stat_names <- statistics
  }
  sumstats <- lapply(all_sumstats, merge, x = data.frame(stat = stat_names), by = 'stat', all.x = TRUE, sort = FALSE)
  sumstats <- lapply(sumstats, function (x) x[match(stat_names, x$stat), ])
  ss_classes <- lapply(sumstats, function (x) x$class)
  sumstats <- lapply(sumstats, function (x) x$value)
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

  if (error_pos == 'right') mod_col_headings <- interleave(mod_col_headings, '')
  mod_col_headings <- c('', mod_col_headings)
  result <- rbind(mod_col_headings, cols, sumstats, copy_cell_props = FALSE)
  result <- set_bottom_border(result, final(), everywhere, outer_borders)
  result <- set_top_border(result, 1, everywhere, outer_borders)
  result <- set_bottom_border(result, c(1, nrow(cols) + 1), -1, borders)
  colnames(result) <- c('names', names_or(models, paste0("model", seq_along(models))))
  if (error_pos == 'right') result <- set_colspan(result, 1, evens, 2)
  align(result)[1, ]    <- 'center'
  align(result)[-1, -1] <- 'right'
  pad_decimal(result)[-1, -1] <- pad_decimal

  if (! is.null(note)) {
    stars <- if (is.null(stars)) '' else paste0(names(stars), ' p < ', stars, collapse = '; ')
    note <- gsub('%stars%', stars, note)
    note <- glue::glue(note)
    result <- add_footnote(result, note, border = 0) # borders handled above
    result <- set_wrap(result, final(), 1, TRUE)
    result <- set_align(result, final(), 1, 'left')
  }

  result
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
