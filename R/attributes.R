
#' @import assertthat
NULL

huxtable_cell_attrs <- c('align', 'valign', 'rowspan', 'colspan', 'background_color', 'text_color',
  'top_border', 'left_border', 'right_border', 'bottom_border',
  'top_border_color', 'left_border_color', 'right_border_color', 'bottom_border_color',
  'top_padding', 'left_padding', 'right_padding', 'bottom_padding', 'wrap',
  'escape_contents', 'na_string', 'bold', 'italic', 'font_size', 'rotation', 'number_format',
  'font', 'pad_decimal')
huxtable_col_attrs <- c('col_width')
huxtable_row_attrs <- c('row_height')
huxtable_table_attrs <- c('width', 'height', 'position', 'caption', 'caption_pos', 'tabular_environment', 'label',
  'latex_float')

huxtable_env <- new.env()
huxtable_env$huxtable_default_attrs <- list(
        rowspan             = 1,
        colspan             = 1,
        align               = 'left',
        valign              = 'top',
        width               = 0.5,
        height              = NA,
        col_width           = NA,
        row_height          = NA,
        background_color    = NA,
        text_color          = NA,
        left_border         = 0,
        right_border        = 0,
        top_border          = 0,
        bottom_border       = 0,
        left_border_color   = NA,
        right_border_color  = NA,
        top_border_color    = NA,
        bottom_border_color = NA,
        left_padding        = 4,
        right_padding       = 4,
        top_padding         = 4,
        bottom_padding      = 4,
        wrap                = FALSE,
        caption             = NA,
        caption_pos         = 'top',
        position            = 'center',
        tabular_environment = 'tabularx',
        label               = NA,
        latex_float         = 'h',
        escape_contents     = TRUE,
        na_string           = '',
        bold                = FALSE,
        italic              = FALSE,
        font_size           = NA,
        rotation            = 0,
        number_format       = list('%.3g'),
        pad_decimal         = NA,
        font                = NA
      )

make_getter_setters <- function(attr_name, attr_type = c('cell', 'row', 'col', 'table'), check_fun = NULL,
  check_values = NULL, extra_code = NULL) {
  attr_type <- match.arg(attr_type)
  funs <- list()

  funs[[attr_name]] <- eval(bquote(
    function(ht) UseMethod(.(attr_name))
  ))
  funs[[paste0(attr_name, '.huxtable')]] <- eval(bquote(
    function(ht) attr(ht, .(attr_name))
  ))

  setter <- paste0(attr_name, '<-')
  funs[[setter]] <- eval(bquote(
    function(ht, value) UseMethod(.(setter))
  ))

  check_fun <- if (! missing(check_fun)) bquote(assert_that(.(check_fun)(value)))
  check_dims <- switch(attr_type,
    table = quote(stopifnot(length(value) == 1))
  )
  check_values <- if (! missing(check_values)) bquote(
    assert_that(all(na.omit(value) %in% .(check_values)))
  )
  extra_code <- if (! missing(extra_code)) substitute(extra_code)
  funs[[paste0(setter, '.huxtable')]] <- eval(bquote(
    function(ht, value) {
      if (! all(is.na(value))) .(check_fun)
      .(check_dims)
      .(check_values)
      .(extra_code)
      value[is.na(value)] <- huxtable_env$huxtable_default_attrs[[.(attr_name)]]
      attr(ht, .(attr_name))[] <- value
      ht
    }
  ))

  alt_setter <- paste0('set_', attr_name)
  attr_symbol <- as.symbol(attr_name)
  funs[[alt_setter]] <- switch(attr_type,
        cell = eval(bquote(
          function(ht, row, col, value, byrow = FALSE) {
            assert_that(is_huxtable(ht))
            nargs <- nargs()
            if (! missing(byrow)) nargs <- nargs - 1
            if (nargs == 3) {
              if (missing(value)) value <- col
              if (! is.matrix(row)) stop('No columns specified, but `row` argument did not evaluate to a matrix')
              if (byrow) stop('`byrow = TRUE` makes no sense if `row` is a matrix')
              .(attr_symbol)(ht)[row] <- value
            } else {
              if (nargs == 2) {
                if (missing(value)) value <- row
                row <- seq_len(nrow(ht))
                col <- seq_len(ncol(ht))
              }
              rc <- list()
              rc$row <- get_rc_spec(ht, row, 1)
              rc$col <- get_rc_spec(ht, col, 2)
              if (byrow) {
                nrc <- lapply(rc, function (x) if (is.logical(x)) sum(x) else length(x))
                value <- matrix(value, nrc$row, nrc$col, byrow = TRUE)
              }
              .(attr_symbol)(ht)[rc$row, rc$col] <- value
            }

            ht
          }
        )),
        row = eval(bquote(
          function(ht, row, value) {
            if (nargs() == 2) {
              if (missing(value)) value <- row
              row <- seq_len(nrow(ht))
            }
            row <- get_rc_spec(ht, row, 1)
            .(as.name(attr_name))(ht)[row] <- value
            ht
          }
        )),
        col = eval(bquote(
          function(ht, col, value) {
            if (nargs() == 2) {
              if (missing(value)) value <- col
              col <- seq_len(ncol(ht))
            }
            col <- get_rc_spec(ht, col, 2)
            .(as.name(attr_name))(ht)[col] <- value
            ht
          }
        )),
        table = eval(bquote(
          function(ht, value) {
            .(as.name(attr_name))(ht) <- value
            ht
          }
        ))
  ) # end switch

  lapply(names(funs), function (x) {
    assign(x, funs[[x]], envir = parent.frame(3)) # 3: 1 for function(x), 2 for lapply, 3 for the caller!
  })

  NULL
}


#' Set default huxtable properties
#'
#' Defaults are used for new huxtables, and also when a property is set to `NA`.
#'
#' @param ... Properties specified by name, or a single named list.
#'
#' @return A list of the previous property values, invisibly.
#' @export
#' @seealso [get_default_properties()]
#' @examples
#' old <- set_default_properties(left_border = 1)
#' hux(a = 1:2, b = 1:2)
#' set_default_properties(old)
set_default_properties <- function(...) {
  defaults <- list(...)
  if (is.list(defaults[[1]]) && is.null(names(defaults))) defaults <- defaults[[1]]

  if (length(unrec <- setdiff(names(defaults), names(huxtable_env$huxtable_default_attrs))) > 0)
        stop('Unrecognized huxtable property name(s): ', paste(unrec, collapse = ', '),
          '; to see all names, use get_default_properties()')
  old <- huxtable_env$huxtable_default_attrs[names(defaults)]
  huxtable_env$huxtable_default_attrs[names(defaults)] <- defaults

  invisible(old)
}

#' Get default huxtable properties
#'
#' @param names Vector of property names. If `NULL`, all properties are returned.
#'
#' @return List of default properties.
#' @export
#'
#' @examples
#' get_default_properties('bold')
#' get_default_properties()
#' @seealso [set_default_properties()]
get_default_properties <- function (names = NULL) {
  names <- names %||% names(huxtable_env$huxtable_default_attrs)
  if (length(unrec <- setdiff(names, names(huxtable_env$huxtable_default_attrs))) > 0) stop(
      'Unrecognized property name(s): ', paste(unrec, collapse = ', '),
        '; to see all names, use get_default_properties()')
  huxtable_env$huxtable_default_attrs[names]
}

#' Set multiple cell properties
#'
#' @param ht A huxtable.
#' @param row A row specification.
#' @param col A column specification.
#' @param ... Named list of property values.
#'
#' @return The modified huxtable object.
#' @export
#'
#' @examples
#' ht <- hux(a = 1:3, b = 1:3)
#' ht <- set_cell_properties(ht, 1, 1, italic = TRUE, text_color = 'red')
#' text_color(ht)
#' ht
set_cell_properties <- function (ht, row, col, ...) {
  props <- list(...)
  if (! all(names(props) %in% huxtable_cell_attrs)) stop('Unrecognized properties: ', paste(setdiff(names(props),
        huxtable_cell_attrs), collapse = ', '))
  call <- match.call(expand.dots = FALSE)
  call[['...']] <- NULL
  for (prop_name in names(props)) {
    call[[1]] <- as.symbol(paste0('set_', prop_name))
    call$value <- props[[prop_name]]
    ht <- eval(call, list(ht = ht), parent.frame())
  }

  ht
}


#' @template getset-cell
#' @templateVar attr_name valign
#' @templateVar attr_desc Vertical alignment
#' @templateVar value_param_desc A character vector or matrix which may be 'top', 'middle', 'bottom' or `NA`.
#' @template getset-example
#' @templateVar attr_val 'bottom'
#' @template getset-rowspec-example
#' @templateVar attr_val2 'bottom'
#' @details
#' Vertical alignment may not work for short text in LaTeX. Defining row heights with [row_height()]
#' may help.
#' @export valign valign<- set_valign valign.huxtable valign<-.huxtable
#' @S3method valign huxtable
#' @S3method valign<- huxtable
NULL
make_getter_setters('valign', 'cell', check_fun = is.character, check_values = c('top', 'middle', 'bottom'))


check_align_value <- function (x) {
  x <- na.omit(x)
  is.character(x) && all(x %in% c('left', 'centre', 'center', 'right') | ncharw(x) == 1)
}


#' @template getset-cell
#' @templateVar attr_name align
#' @templateVar attr_desc Alignment
#' @templateVar value_param_desc A character vector or matrix which may be 'left', 'center', 'right' , `NA` or a single character.
#' @template getset-example
#' @templateVar attr_val 'right'
#' @template getset-visible-rowspec-example
#' @templateVar attr_val2 'left'
#' @details This sets the horizontal alignment of the cell. If `value` is a single character (e.g.
#' a decimal point), then the cell is aligned on this character.
#' @export align align<- set_align align.huxtable align<-.huxtable
#' @S3method align huxtable
#' @S3method align<- huxtable
NULL
make_getter_setters('align', 'cell', check_fun = check_align_value,
      extra_code = value[value == 'centre'] <- 'center')


#' @template getset-rowcol
#' @templateVar attr_name col_width
#' @templateVar rowcol col
#' @templateVar attr_desc Column widths
#' @templateVar value_param_desc A vector. If numeric, they are treated as proportions of the table width. If character, they must be valid CSS or LaTeX lengths.
#' @details
#' In LaTeX, if you specify a column width, but set `wrap` to `FALSE` and have cells which
#' overrun, then you may have problems with table position and with background colours in other cells.
#' The workaround is to adjust the width, so that your cells no longer overrun.
#' @family row/column heights
#' @template getset-example
#' @templateVar attr_val c(.2, .8)
#' @templateVar extra print_screen(ht)
#' @export col_width col_width<- set_col_width col_width.huxtable col_width<-.huxtable
#' @S3method col_width huxtable
#' @S3method col_width<- huxtable
NULL
make_getter_setters('col_width', 'col')


#' @template getset-rowcol
#' @templateVar attr_name row_height
#' @templateVar rowcol row
#' @templateVar attr_desc Row heights
#' @templateVar value_param_desc A vector.
#' @family row/column heights
#' @details
#' If character, `value` must contain valid CSS or LaTeX lengths. If numeric, in HTML, values are scaled to 1 and treated as proportions of the table height. In LaTeX, they are
#' treated as proportions of the text height (\\\\textheight).
#' @template getset-example
#' @templateVar attr_val c(.2, .1, .1)
#' @export row_height row_height<- set_row_height row_height.huxtable row_height<-.huxtable
#' @S3method row_height huxtable
#' @S3method row_height<- huxtable
NULL
make_getter_setters('row_height', 'row')


#' @template getset-cell
#' @templateVar attr_name rowspan
#' @templateVar attr_desc Row span
#' @templateVar value_param_desc An integer vector or matrix of integers.
#' @template getset-example
#' @noMd
#' @templateVar subscript [1, 1]
#' @templateVar attr_val 2
#' @templateVar extra print_screen(ht)
#' @export rowspan rowspan<- set_rowspan rowspan.huxtable rowspan<-.huxtable
#' @S3method rowspan huxtable
#' @S3method rowspan<- huxtable
NULL
make_getter_setters('rowspan', 'cell', check_fun = is.numeric, extra_code = {
      if (any(na.omit( row(ht) + value - 1 > nrow(ht) ))) stop('rowspan would extend beyond bottom of table')
      check_span_shadows(ht, 'row', value)
    }
)


#' @template getset-cell
#' @templateVar attr_name colspan
#' @templateVar attr_desc Column span
#' @templateVar value_param_desc An integer vector or matrix of integers.
#' @template getset-example
#' @noMd
#' @templateVar subscript [1, 1]
#' @templateVar attr_val 2
#' @templateVar extra print_screen(ht)
#' @export colspan colspan<- set_colspan colspan.huxtable colspan<-.huxtable
#' @S3method colspan huxtable
#' @S3method colspan<- huxtable
NULL
make_getter_setters('colspan', 'cell', check_fun = is.numeric, extra_code = {
      if (any(na.omit( col(ht) + value - 1 > ncol(ht) ))) stop('rowspan would extend beyond bottom of table')
      check_span_shadows(ht, 'col', value)
    }
)


check_span_shadows <- function (ht, rc, value) {
  value[is.na(value)] <- 1L
  dcells <- if (rc == 'row') display_cells(ht, new_rowspan = value) else display_cells(ht, new_colspan = value)
  candidates <- as.matrix(dcells[dcells$shadowed, c('row', 'col')])
  problems <- value[candidates] # span of cells which would be shadowed;
  if (any(problems > 1)) {
    candidates <- candidates[problems > 1, , drop = FALSE]
    candidates <- paste(apply(candidates, 1, paste, collapse = ','), collapse = '; ')
    stop('New rowspan/colspan would cut up existing multirow/multicol cells at ', candidates)
  }
}


#' @template getset-cell
#' @templateVar attr_name background_color
#' @templateVar attr_desc Background color
#' @templateVar value_param_desc A character vector or matrix of valid R color names.
#' @template getset-example
#' @templateVar attr_val grey(.95)
#' @template getset-visible-rowspec-example
#' @templateVar attr_val2 'yellow'
#' @family formatting functions
#' @export background_color background_color<- set_background_color background_color.huxtable background_color<-.huxtable
#' @S3method background_color huxtable
#' @S3method background_color<- huxtable
NULL
make_getter_setters('background_color', 'cell')


#' @template getset-cell
#' @templateVar attr_name text_color
#' @templateVar attr_desc Text color
#' @templateVar value_param_desc A character vector or matrix of valid R color names.
#' @template getset-example
#' @templateVar attr_val 'blue'
#' @template getset-visible-rowspec-example
#' @templateVar attr_val2 'red'
#' @family formatting functions
#' @export text_color text_color<- set_text_color text_color.huxtable text_color<-.huxtable
#' @S3method text_color huxtable
#' @S3method text_color<- huxtable
NULL
make_getter_setters('text_color', 'cell')


#' @template getset-cell
#' @templateVar attr_name left_border
#' @templateVar attr_desc Borders
#' @templateVar value_param_desc A numeric vector or matrix giving border widths in points. Set to 0 for no border.
#' @templateVar morealiases right_border top_border bottom_border
#' @details
#' Currently in LaTeX, all non-zero border widths on a given line must be the same, and vertical border widths
#' can only be present (if `value > 0`) or absent.
#' @seealso [set_all_borders()]
#' @template getset-example
#' @templateVar attr_val 1
#' @templateVar extra print_screen(ht)
#' @template getset-visible-rowspec-example
#' @templateVar attr_val2 2
#' @export left_border left_border<- set_left_border left_border.huxtable left_border<-.huxtable
#' @S3method left_border huxtable
#' @S3method left_border<- huxtable
NULL
make_getter_setters('left_border', 'cell', check_fun = is.numeric)


#' @name right_border
#' @rdname left_border
#' @return Similarly for the other functions.
#' @usage
#' right_border(ht)
#' right_border(ht) <- value
#' set_right_border(ht, row, col, value, byrow = FALSE)
#' @export right_border right_border<- set_right_border right_border.huxtable right_border<-.huxtable
#' @S3method right_border huxtable
#' @S3method right_border<- huxtable
NULL
make_getter_setters('right_border', 'cell', check_fun = is.numeric)


#' @name top_border
#' @rdname left_border
#' @usage
#' top_border(ht)
#' top_border(ht) <- value
#' set_top_border(ht, row, col, value, byrow = FALSE)
#' @export top_border top_border<- set_top_border top_border.huxtable top_border<-.huxtable
#' @S3method top_border huxtable
#' @S3method top_border<- huxtable
NULL
make_getter_setters('top_border', 'cell', check_fun = is.numeric)

#' @name bottom_border
#' @rdname left_border
#' @usage
#' bottom_border(ht)
#' bottom_border(ht) <- value
#' set_bottom_border(ht, row, col, value, byrow = FALSE)
#' @export bottom_border bottom_border<- set_bottom_border bottom_border.huxtable bottom_border<-.huxtable
#' @S3method bottom_border huxtable
#' @S3method bottom_border<- huxtable
NULL
make_getter_setters('bottom_border', 'cell', check_fun = is.numeric)

#' Set all borders
#'
#' This is a convenience function which sets left, right, top and bottom borders
#' for the specified cells.
#'
#' @inheritParams left_border
#'
#' @return The modified huxtable.
#' @export
#'
#' @seealso [left_border()], [set_outer_borders()]
#' @examples
#' ht <- huxtable(a = 1:3, b = 1:3)
#' set_all_borders(ht, 1:3, 1:2, 1)
set_all_borders <- function(ht, row, col, value, byrow = FALSE) {
  call <- sys.call()
  for (set_b in paste0('set_', c('top', 'bottom', 'left', 'right'), '_border')) {
    call[[1]] <- as.symbol(set_b)
    call[[2]] <- quote(ht)
    ht <- eval(call, list(ht = ht), parent.frame())
  }

  ht
}


#' Set outer borders round a rectangle of cells
#'
#' This is a convenience function to set a border round the top,
#' bottom, left and right of a group of cells.
#'
#' @param ht A huxtable
#' @param row A row specifier. See [rowspecs()] for details.
#' @param col A column specifier.
#' @param value A numeric value for the border width. Set to 0 for no border.
#'
#' @return The modified huxtable.
#' @export
#'
#' @seealso [left_border()], [set_all_borders()]
#' @examples
#' ht <- huxtable(a = 1:3, b = 1:3)
#' set_outer_borders(ht, 1)
#' set_outer_borders(ht, 2:3, 1:2, 1)
#'
set_outer_borders <- function(ht, row, col, value) {
  if (missing(col) && missing(value)) {
    value <- row
    row <- seq_len(nrow(ht))
    col <- seq_len(ncol(ht))
  } else if (missing(value)) {
    value <- col
    if (!is.matrix(row)) stop("No columns specified, but `row` argument did not evaluate to a matrix")
    # row is a 2-matrix of row, col vectors;
    col <- seq(min(row[, 2]), max(row[, 2]))
    row <- seq(min(row[, 1]), max(row[, 1]))
  }
  row <- get_rc_spec(ht, row, 1)
  col <- get_rc_spec(ht, col, 2)
  if (is.logical(row)) row <- which(row)
  if (is.logical(col)) col <- which(col)

  left_border(ht)[row, min(col)]    <- value
  right_border(ht)[row, max(col)]   <- value
  top_border(ht)[min(row), col]     <- value
  bottom_border(ht)[max(row), col]  <- value

  ht
}


get_all_borders <- function(ht, row, col) {
  list(
    left   = left_border(ht)[row, col],
    right  = right_border(ht)[row, col],
    top    = top_border(ht)[row, col],
    bottom = bottom_border(ht)[row, col]
  )
}


#' @template getset-cell
#' @templateVar attr_name left_border_color
#' @templateVar attr_desc Border colors
#' @templateVar value_param_desc A vector or matrix of colors. Set to `NA` for the default.
#' @templateVar morealiases right_border_color top_border_color bottom_border_color
#' @template getset-example
#' @templateVar attr_val 'red'
#' @details
#' Both LaTeX and HTML collapse borders. If results are not what you expected, try setting the
#' adjoining border of the previous cell to width 0 (e.g. for a left border color, unset the right border
#' of the cell on the left).
#' @seealso [set_all_border_colors()]
#' @template getset-visible-rowspec-example
#' @templateVar attr_val2 'blue'
#' @export left_border_color left_border_color<- set_left_border_color left_border_color.huxtable left_border_color<-.huxtable
#' @S3method left_border_color huxtable
#' @S3method left_border_color<- huxtable
NULL
make_getter_setters('left_border_color', 'cell')


#' @name right_border_color
#' @rdname left_border_color
#' @return Similarly for the other functions.
#' @usage
#' right_border_color(ht)
#' right_border_color(ht) <- value
#' set_right_border_color(ht, row, col, value, byrow = FALSE)
#' @export right_border_color right_border_color<- set_right_border_color right_border_color.huxtable right_border_color<-.huxtable
#' @S3method right_border_color huxtable
#' @S3method right_border_color<- huxtable
NULL
make_getter_setters('right_border_color', 'cell')


#' @name top_border_color
#' @rdname left_border_color
#' @usage
#' top_border_color(ht)
#' top_border_color(ht) <- value
#' set_top_border_color(ht, row, col, value, byrow = FALSE)
#' @export top_border_color top_border_color<- set_top_border_color top_border_color.huxtable top_border_color<-.huxtable
#' @S3method top_border_color huxtable
#' @S3method top_border_color<- huxtable
NULL
make_getter_setters('top_border_color', 'cell')

#' @name bottom_border_color
#' @rdname left_border_color
#' @usage
#' bottom_border_color(ht)
#' bottom_border_color(ht) <- value
#' set_bottom_border_color(ht, row, col, value, byrow = FALSE)
#' @export bottom_border_color bottom_border_color<- set_bottom_border_color bottom_border_color.huxtable bottom_border_color<-.huxtable
#' @S3method bottom_border_color huxtable
#' @S3method bottom_border_color<- huxtable
NULL
make_getter_setters('bottom_border_color', 'cell')


#' Set all border colors
#'
#' This is a convenience function which sets left, right, top and bottom border
#' colors for the specified cells.
#'
#' @inheritParams left_border_color
#'
#' @return The modified huxtable.
#' @seealso [left_border_color()]
#'
#' @export
#'
#' @examples
#' ht <- huxtable(a = 1:3, b = 1:3)
#' ht <- set_all_border_colors(ht, 'red')
set_all_border_colors <- function(ht, row, col, value, byrow = FALSE) {
  call <- sys.call()
  for (set_b in paste0('set_', c('top', 'bottom', 'left', 'right'), '_border_color')) {
    call[[1]] <- as.symbol(set_b)
    call[[2]] <- quote(ht)
    ht <- eval(call, list(ht = ht), parent.frame())
  }

  ht
}


get_all_border_colors <- function(ht, row, col, drop = TRUE) {
  list(
    left   = left_border_color(ht)[row, col, drop = drop],
    right  = right_border_color(ht)[row, col, drop = drop],
    top    = top_border_color(ht)[row, col, drop = drop],
    bottom = bottom_border_color(ht)[row, col, drop = drop]
  )
}


#' @name left_padding
#' @template getset-cell
#' @templateVar attr_name left_padding
#' @templateVar attr_desc Cell padding
#' @templateVar value_param_desc
#' A vector or matrix. Characters must be valid CSS or LaTeX lengths. Numbers will be interpreted as lengths in points.
#' @templateVar morealiases right_padding top_padding bottom_padding
#' @template getset-example
#' @templateVar attr_val 20
#' @template getset-rowspec-example
#' @templateVar attr_val2 10
#' @export left_padding left_padding<- set_left_padding left_padding.huxtable left_padding<-.huxtable
#' @S3method left_padding huxtable
#' @S3method left_padding<- huxtable
NULL
for (val in paste0(c('left', 'right', 'top', 'bottom'), '_padding')) make_getter_setters(val, 'cell')


#' @name right_padding
#' @rdname left_padding
#' @return Similarly for the other functions.
#' @usage
#' right_padding(ht)
#' right_padding(ht) <- value
#' set_right_padding(ht, row, col, value, byrow = FALSE)
#' @export right_padding right_padding<- set_right_padding right_padding.huxtable right_padding<-.huxtable
#' @S3method right_padding huxtable
#' @S3method right_padding<- huxtable
NULL

#' @name bottom_padding
#' @rdname left_padding
#' @usage
#' bottom_padding(ht)
#' bottom_padding(ht) <- value
#' set_bottom_padding(ht, row, col, value, byrow = FALSE)
#' @export bottom_padding bottom_padding<- set_bottom_padding bottom_padding.huxtable bottom_padding<-.huxtable
#' @S3method bottom_padding huxtable
#' @S3method bottom_padding<- huxtable
NULL

#' @name top_padding
#' @rdname left_padding
#' @usage
#' top_padding(ht)
#' top_padding(ht) <- value
#' set_top_padding(ht, row, col, value, byrow = FALSE)
#' @export top_padding top_padding<- set_top_padding top_padding.huxtable top_padding<-.huxtable
#' @S3method top_padding huxtable
#' @S3method top_padding<- huxtable
NULL


#' @name set_all_padding
#'
#' @rdname left_padding
#' @usage
#' set_all_padding(ht, row, col, value, byrow = FALSE)
#'
#' @details
#' `set_all_padding` is a convenience function which sets left, right, top and bottom cell padding
#' for the specified cells.
#'
#' @examples
#' ht <- huxtable(a = 1:3, b = 1:3)
#' ht <- set_all_padding(ht, 1:3, 1:2, "20px")
#' left_padding(ht)
#' right_padding(ht)
#' @export
set_all_padding <- function(ht, row, col, value, byrow = FALSE) {
  call <- sys.call()
  for (set_p in paste0('set_', c('top', 'bottom', 'left', 'right'), '_padding')) {
    call[[1]] <- as.symbol(set_p)
    call[[2]] <- quote(ht)
    ht <- eval(call, list(ht = ht), parent.frame())
  }

  ht
}



#' @template getset-cell
#' @templateVar attr_name wrap
#' @templateVar attr_desc Text wrapping
#' @templateVar value_param_desc
#' A logical vector or matrix. If `TRUE`, long cell contents will be wrapped into multiple lines. Set to `NA` for the default.
#' @examples
#' ht <- huxtable(a = rep('Some long text', 2))
#' wrap(ht)[1,] <- TRUE
#' print_html(ht)
#' @template getset-rowspec-example
#' @templateVar attr_val TRUE
#' @templateVar attr_val2 FALSE
#'
#' @export wrap wrap<- set_wrap wrap.huxtable wrap<-.huxtable
#' @S3method wrap huxtable
#' @S3method wrap<- huxtable
NULL
make_getter_setters('wrap', 'cell', check_fun = is.logical)


#' @template getset-cell
#' @templateVar attr_name escape_contents
#' @templateVar attr_desc Escape cell contents
#' @templateVar value_param_desc
#' A logical vector or matrix. If `TRUE`, cell contents will be HTML or LaTeX escaped.
#' @examples
#' ht <- huxtable(Exponent = 2:4, Example = paste0('$x^', 2:4, '$'))
#' escape_contents(ht)[,2] <- FALSE
#' @template getset-rowspec-example
#' @templateVar attr_val TRUE
#' @templateVar attr_val2 FALSE
#' @export escape_contents escape_contents<- set_escape_contents escape_contents.huxtable escape_contents<-.huxtable
#' @S3method escape_contents huxtable
#' @S3method escape_contents<- huxtable
NULL
make_getter_setters('escape_contents', 'cell', check_fun = is.logical)


#' @template getset-cell
#' @templateVar attr_name na_string
#' @templateVar attr_desc NA string
#' @templateVar value_param_desc
#' A character string. This will be used to replace NA values in the display. Set to `NA` for the default, which is the empty string. To get literal "NA", set to "NA".
#' @template getset-example
#' @templateVar attr_val '--'
#' @noMd
#' @templateVar extra ht[2,2] <- NA ## print_screen(ht)
#' @template getset-rowspec-example
#' @templateVar attr_val2 ''
#' @family formatting functions
#' @export na_string na_string<- set_na_string na_string.huxtable na_string<-.huxtable
#' @S3method na_string huxtable
#' @S3method na_string<- huxtable
NULL
make_getter_setters('na_string', 'cell', check_fun = is.character)


#' @template getset-cell
#' @templateVar attr_name bold
#' @templateVar attr_desc Cell text style
#' @templateVar value_param_desc
#' A logical vector or matrix
#' @templateVar morealiases italic italic<- set_italic italic.huxtable italic<-.huxtable
#' @template getset-example
#' @templateVar attr_val TRUE
#' @templateVar extra print_screen(ht)
#' @template getset-visible-rowspec-example
#' @templateVar attr_val2 FALSE
#' @family formatting functions
#' @export bold bold<- set_bold bold.huxtable bold<-.huxtable
#' @S3method bold huxtable
#' @S3method bold<- huxtable
NULL
make_getter_setters('bold', 'cell', check_fun = is.logical)


#' @name italic
#' @rdname bold
#' @usage
#' italic(ht)
#' italic(ht) <- value
#' set_italic(ht, row, col, value, byrow = FALSE)
#' @export italic italic<- set_italic italic.huxtable italic<-.huxtable
#' @S3method italic huxtable
#' @S3method italic<- huxtable
NULL
make_getter_setters('italic', 'cell', check_fun = is.logical)


#' @template getset-cell
#' @templateVar attr_name font_size
#' @templateVar attr_desc Font size
#' @templateVar value_param_desc
#' A numeric vector. This sets the font size in points.
#' @template getset-example
#' @templateVar attr_val 14
#' @template getset-rowspec-example
#' @templateVar attr_val2 12
#' @family formatting functions
#' @export font_size font_size<- set_font_size font_size.huxtable font_size<-.huxtable
#' @S3method font_size huxtable
#' @S3method font_size<- huxtable
NULL
make_getter_setters('font_size', 'cell', check_fun = is.numeric)


#' @template getset-cell
#' @templateVar attr_name rotation
#' @templateVar attr_desc Text rotation
#' @templateVar value_param_desc
#' A numeric vector. Clockwise from the x axis, so 0 is left to right, 90 is going up, etc.
#' @template getset-example
#' @templateVar attr_val 90
#' @template getset-rowspec-example
#' @templateVar attr_val2 270
#' @details
#' You will probably need to set [col_width()] and [row_height()] explicitly
#' to achieve a nice result, in both HTML and LaTeX.
#' @export rotation rotation<- set_rotation rotation.huxtable rotation<-.huxtable
#' @S3method rotation huxtable
#' @S3method rotation<- huxtable
NULL
make_getter_setters('rotation', 'cell', check_fun = is.numeric)

#' @template getset-cell
#' @templateVar attr_name number_format
#' @templateVar attr_desc Number format
#' @templateVar value_param_desc
#' A vector or list which may be character, numeric or function. See below.
#'
#' @details
#' Number formatting is applied to any parts of cells that look like numbers (defined as an optional minus sign,
#' followed by
#' numerals, followed by an optional decimal point and further numerals). The exception is exponents in
#' scientific notation; huxtable attempts to detect and ignore these.
#'
#' If `value` is numeric,
#' numbers will be rounded to that many decimal places.  If `value` is
#' character, it will be taken as an argument to [sprintf()]. If `value` is a
#' function it will be applied to the numbers and should return a string. If `value` is `NA`, then numbers
#' will be unchanged. Note that setting `value` to `NA` does not reset to the default.
#'
#' The default value is "\%.3g" which rounds numbers if they have more than 3 significant
#' digits, and which may use an exponent for large numbers.
#'
#' To set number_format to a function, enclose the function in `list`.
#' See the examples.
#'
#' Versions of huxtable before 2.0.0 applied `number_format` only to cells that looked like
#' numbers in their entirety. The default value was "\%5.2f".
#'
#' @family formatting functions
#'
#' @examples
#' ht <- huxtable(a = 10^(3:6) + (5 * 10^(-2:-5)), b = 10^(3:6) + (5* 10^(-2:-5)))
#' number_format(ht)[1,] <- 2
#' number_format(ht)[2,] <- '%5.2f'
#' number_format(ht)[3,] <- list(function(x) prettyNum(x, big.mark = ','))
#' number_format(ht)[4,] <- list(function(x) if(x>0) '+' else '-')
#' ht
#' print_screen(ht)
#' ht_bands <- huxtable("10000 Maniacs")
#' ht_bands # probably not what you want
#' number_format(ht_bands) <- NA
#' ht_bands
#' @template getset-visible-rowspec-example
#' @templateVar attr_val 2
#' @templateVar attr_val2 3
#' @export number_format number_format<- set_number_format number_format.huxtable number_format<-.huxtable
#' @S3method number_format huxtable
#' @S3method number_format<- huxtable
NULL
make_getter_setters('number_format', 'cell')

# override the default
`number_format<-.huxtable` <- function(ht, value) {
  stopifnot(all(sapply(value, function (x) is.numeric(x) || is.character(x) || is.function(x) || is.na(x) )))
  attr(ht, 'number_format')[] <- value
  ht
}


#' @name pad_decimal
#' @aliases pad_decimal<- set_pad_decimal pad_decimal.huxtable pad_decimal<-.huxtable
#' @rdname align
#' @export pad_decimal pad_decimal<- set_pad_decimal pad_decimal.huxtable pad_decimal<-.huxtable
#' @S3method pad_decimal huxtable
#' @S3method pad_decimal<- huxtable
NULL
make_getter_setters('pad_decimal', 'cell', extra_code = {
  stopifnot(all(nchar(na.omit(value)) == 1))
  warning('pad_decimal is deprecated. Instead, use e.g. align(x) <- "."')
})


#' @template getset-cell
#' @templateVar attr_name font
#' @templateVar attr_desc Font
#' @templateVar value_param_desc
#' A character vector of font names. NB that LaTeX and HTML use different font names.
#' @template getset-example
#' @templateVar attr_val 'times'
#' @template getset-rowspec-example
#' @templateVar attr_val2 'arial'
#' @family formatting functions
#' @export font font<- set_font font.huxtable font<-.huxtable
#' @S3method font huxtable
#' @S3method font<- huxtable
NULL
make_getter_setters('font', 'cell', check_fun = is.character)


#' @template getset-table
#' @templateVar attr_name position
#' @templateVar attr_desc Table position
#' @templateVar value_param_desc
#' A length-one character vector which may be 'left', 'center', 'right' or `NA`.
#' @details
#' If your tables are too far to the right under LaTeX, try setting their [width()]
#' explicitly.
#' @template getset-example
#' @templateVar attr_val 'right'
#' @export position position<- set_position position.huxtable position<-.huxtable
#' @S3method position huxtable
#' @S3method position<- huxtable
NULL
make_getter_setters('position', 'table', check_values = c('left', 'center', 'centre', 'right'),
      extra_code = value[value == 'centre'] <- 'center')

#' @template getset-table
#' @templateVar attr_name caption_pos
#' @templateVar attr_desc Caption position
#' @templateVar value_param_desc
#' A length-one character vector, one of 'top', 'bottom', 'topleft', 'topcenter', 'topright', 'bottomleft', 'bottomcenter', 'bottomright', or `NA` for the default.
#' @details
#' If `caption_pos` is 'top' or 'bottom', then the horizontal position ('left', 'center' or 'right')
#' will be determined by the huxtable's [position()].
#' @template getset-example
#' @templateVar attr_val 'bottom'
#' @export caption_pos caption_pos<- set_caption_pos caption_pos.huxtable caption_pos<-.huxtable
#' @S3method caption_pos huxtable
#' @S3method caption_pos<- huxtable
#' @seealso [caption()]
NULL
make_getter_setters('caption_pos', 'table', check_values = c('top', 'bottom', 'topleft', 'topcenter', 'topcentre',
      'topright', 'bottomleft', 'bottomcenter', 'bottomcentre', 'bottomright'), extra_code = {
        value[value == 'topcentre'] <- 'topcenter'
        value[value == 'bottomcentre'] <- 'bottomcenter'
      })


#' @template getset-table
#' @templateVar attr_name width
#' @templateVar attr_desc Table width
#' @templateVar value_param_desc
#' A length-one vector. If numeric, `value` is treated as a proportion of the surrounding block width (HTML) or text width (LaTeX). If character, it must be a valid CSS or LaTeX width. Set to `NA` for the default.
#' @template getset-example
#' @templateVar attr_val 0.8
#' @family table measurements
#' @export width width<- set_width width.huxtable width<-.huxtable
#' @S3method width huxtable
#' @S3method width<- huxtable
NULL
make_getter_setters('width', 'table')


#' @template getset-table
#' @templateVar attr_name height
#' @templateVar attr_desc Table height
#' @templateVar value_param_desc
#' A length-one vector. If numeric, it is treated as a proportion of the containing block height for HTML, or of text height (\\\\textheight) for LaTeX. If character, it must be a valid CSS or LaTeX width. Set to `NA` for the default, which is to leave height unset.
#' @template getset-example
#' @templateVar attr_val 0.4
#' @family table measurements
#' @export height height<- set_height height.huxtable height<-.huxtable
#' @S3method height huxtable
#' @S3method height<- huxtable
NULL
make_getter_setters('height', 'table')

#' @template getset-table
#' @templateVar attr_name caption
#' @templateVar attr_desc Caption
#' @templateVar value_param_desc
#' A length-one character vector. Set to `NA` for no caption.
#' @details
#' Captions are not escaped. See the example for a workaround.
#' @template getset-example
#' @templateVar attr_val 'An example table'
#' @templateVar extra print_screen(ht)
#' @seealso [caption_pos()]
#' @examples
#' ht <- hux(a = 1:2, b = 1:2)
#' caption(ht) <- sanitize('Make $$$ with us', type = 'latex') # escape caption characters
#' @export caption caption<- set_caption caption.huxtable caption<-.huxtable
#' @S3method caption huxtable
#' @S3method caption<- huxtable
NULL
make_getter_setters('caption', 'table', check_fun = is.character)


#' @template getset-table
#' @templateVar attr_name tabular_environment
#' @templateVar attr_desc Tabular environment
#' @templateVar value_param_desc
#' A length-one character vector. Set to `NA` for the default, 'tabularx'.
#' @template getset-example
#' @templateVar attr_val 'longtable'
#' @details No features are guaranteed to work if you set this to a non-default value. Use at your own risk!
#' @export tabular_environment tabular_environment<- set_tabular_environment tabular_environment.huxtable tabular_environment<-.huxtable
#' @S3method tabular_environment huxtable
#' @S3method tabular_environment<- huxtable
NULL
make_getter_setters('tabular_environment', 'table', check_fun = is.character)





#' @template getset-table
#' @templateVar attr_name label
#' @templateVar attr_desc Table label
#' @templateVar value_param_desc
#' A length-one character vector to be used as a table label in LaTeX, or as an ID for the table in HTML. Set to `NA` to remove any label.
#' @template getset-example
#' @templateVar attr_val 'tab:mytable'
#' @details
#' LaTeX table labels typically start with "tab:", and they must do so if you want table numbering
#' in \href{http://bookdown.org}{bookdown}.
#' @export label label<- set_label label.huxtable label<-.huxtable
#' @S3method label huxtable
#' @S3method label<- huxtable
NULL
make_getter_setters('label', 'table', check_fun = is.character)


#' @template getset-table
#' @templateVar attr_name latex_float
#' @templateVar attr_desc Float position for LaTeX
#' @templateVar value_param_desc
#' A length-one character vector, used by LaTeX for positioning the float. Set to `NA` for the default, 'h'.
#' @template getset-example
#' @templateVar attr_val 'tab:mytable'
#' @details Quick reference: 'h' here, 'h!' definitely here, 't' top of page, 'b' bottom of page, 'p' page of
#' floats. See LaTeX documentation for more details. If you use 'H' (definitely here), you must require the
#' `float` package.
#' @export latex_float latex_float<- set_latex_float latex_float.huxtable latex_float<-.huxtable
#' @S3method latex_float huxtable
#' @S3method latex_float<- huxtable
NULL
make_getter_setters('latex_float', 'table', check_fun = is.character)
