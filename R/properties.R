
#' @import assertthat
#' @importFrom stats na.omit
NULL


huxtable_cell_attrs <- c('align', 'valign', 'rowspan', 'colspan', 'background_color', 'text_color',
  'top_border', 'left_border', 'right_border', 'bottom_border',
  'top_border_color', 'left_border_color', 'right_border_color', 'bottom_border_color',
  'top_border_style', 'left_border_style', 'right_border_style', 'bottom_border_style',
  'top_padding', 'left_padding', 'right_padding', 'bottom_padding', 'wrap',
  'escape_contents', 'na_string', 'bold', 'italic', 'font_size', 'rotation', 'number_format',
  'font', 'pad_decimal')
huxtable_col_attrs <- c('col_width')
huxtable_row_attrs <- c('row_height')
huxtable_table_attrs <- c('width', 'height', 'position', 'caption', 'caption_pos',
  'tabular_environment', 'label', 'latex_float')


#' @evalNamespace make_namespace_S3_entries(huxtable_cell_attrs)
#' @evalNamespace make_namespace_S3_entries(huxtable_col_attrs)
#' @evalNamespace make_namespace_S3_entries(huxtable_row_attrs)
#' @evalNamespace make_namespace_S3_entries(huxtable_table_attrs)
#' @evalNamespace make_exports(huxtable_col_attrs)
#' @evalNamespace make_exports(huxtable_row_attrs)
#' @evalNamespace make_exports(huxtable_table_attrs)
#' @evalNamespace make_exports(huxtable_cell_attrs, with_by = TRUE)
NULL

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
        left_border_style   = 'solid',
        right_border_style  = 'solid',
        top_border_style    = 'solid',
        bottom_border_style = 'solid',
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
      mode(attr(ht, .(attr_name))) <- mode(value)
      ht
    }
  ))

  alt_setter <- paste0('set_', attr_name)
  set_by_fun <- paste0('set_', attr_name, '_by')
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
              .Deprecated(msg = c('The 3 argument form of set_* functions is deprecated.\n',
                     'Use set_*_by instead.\n'), package = 'huxtable')
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

  funs[[set_by_fun]] <- eval(bquote(
    function (ht, row, col, fn) {
      assert_that(is_huxtable(ht))
      nargs <- nargs()

      if (nargs() == 2) {
        if (missing(fn)) fn <- row
        row <- seq_len(nrow(ht))
        col <- seq_len(ncol(ht))
      }
      rc <- list()
      rc$row <- get_rc_spec(ht, row, 1)
      rc$col <- get_rc_spec(ht, col, 2)

      current <- .(as.name(attr_name))(ht)[rc$row, rc$col, drop = FALSE]
      .(as.name(attr_name))(ht)[rc$row, rc$col] <- fn(ht, rc$row, rc$col, current)

      ht
    }
  ))

  hux_ns <- getNamespace('huxtable')
  lapply(names(funs), function (x) {
    environment(funs[[x]]) <- hux_ns
    assign(x, funs[[x]], envir = hux_ns)
  })

  NULL
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
#' treated as proportions of the text height (`\\textheight`).
#' @template getset-example
#' @templateVar attr_val c(.2, .1, .1)
NULL
make_getter_setters('row_height', 'row')


#' @template getset-cell
#' @templateVar attr_name rowspan
#' @templateVar attr_desc Row and column span
#' @templateVar value_param_desc An integer vector or matrix.
#' @details
#' The rowspan and colspan of a cell determine its height and width, in rows and columns.
#' A cell with rowspan of 2 covers the cell directly below it. A cell with rowspan of 2
#' and colspan of 2 covers a 2 x 2 square, hiding three other cells.
#' @template getset-example
#' @noMd
#' @templateVar subscript [1, 1]
#' @templateVar attr_val 2
#' @templateVar extra ht <- set_all_borders(ht, 1) ## ht
NULL
make_getter_setters('rowspan', 'cell', check_fun = is.numeric, extra_code = {
      if (any(na.omit( row(ht) + value - 1 > nrow(ht) ))) stop('rowspan would extend beyond bottom of table')
      check_span_shadows(ht, 'row', value)
    }
)


#' @name colspan
#' @rdname rowspan
#' @templateVar attr_name colspan
#' @template cell-property-usage
#' @aliases colspan<- set_colspan set_colspan_by
NULL
make_getter_setters('colspan', 'cell', check_fun = is.numeric, extra_code = {
      if (any(na.omit( col(ht) + value - 1 > ncol(ht) ))) stop(
            'colspan would extend beyond right edge of table')
      check_span_shadows(ht, 'col', value)
    }
)


check_span_shadows <- function (ht, rc, value) {
  value[is.na(value)] <- 1L
  dcells <- if (rc == 'row') display_cells(ht, new_rowspan = value) else display_cells(ht, new_colspan = value)
  candidates <- as.matrix(dcells[dcells$shadowed, c('row', 'col')])
  problems <- value[candidates] > 1 # span of cells which would be shadowed;
  if (any(problems)) {
    candidates <- candidates[problems, , drop = FALSE]
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
NULL
make_getter_setters('background_color', 'cell')


#' @template getset-cell
#' @templateVar attr_name text_color
#' @templateVar attr_desc Text color
#' @templateVar value_param_desc A character vector or matrix of valid R colors.
#' @details
#' Colors can be in any format understood by R, e.g. `"red"`, `"#FF0000"` or `rgb(1, 0, 0)`.
#' @template getset-example
#' @templateVar attr_val 'blue'
#' @template getset-visible-rowspec-example
#' @templateVar attr_val2 'red'
#' @family formatting functions
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
#'
#' @seealso [set_all_borders()]
#' @template getset-example
#' @templateVar attr_val 1
#' @templateVar extra print_screen(ht)
#' @templateVar attr_val2 2
#' @template getset-visible-rowspec-example
#' @template border-warning
#' @template cell-property-usage
NULL
for (val in paste0(c('left', 'right', 'top', 'bottom'), '_border')) make_getter_setters(val, 'cell',
      check_fun = is.numeric)


#' @name right_border
#' @rdname left_border
#' @return Similarly for the other functions.
#' @templateVar attr_name right_border
#' @template cell-property-usage
NULL


#' @name top_border
#' @rdname left_border
#' @templateVar attr_name top_border
#' @template cell-property-usage
NULL


#' @name bottom_border
#' @rdname left_border
#' @templateVar attr_name bottom_border
#' @template cell-property-usage
NULL


#' @template getset-cell
#' @templateVar attr_name left_border_color
#' @templateVar attr_desc Border colors
#' @templateVar value_param_desc A vector or matrix of colors.
#' @templateVar morealiases right_border_color top_border_color bottom_border_color
#' @templateVar attr_val 'red'
#' @details
#' Huxtable collapses borders and border colors. Right borders take priority over left borders, and
#' top borders take priority over bottom borders.
#'
#' @seealso [set_all_border_colors()]
#' @templateVar attr_val2 'blue'
#' @examples
#' ht <- huxtable(a = 1:3, b = 3:1)
#' ht <- set_all_borders(ht, 1)
#' set_left_border_color(ht, 'red')
#' set_left_border_color(ht, 1:2, 1, 'red')
#' set_left_border_color(ht, 1:2, 1:2, c('red', 'blue'), byrow = TRUE)
#' @template border-warning
#' @template cell-property-usage
#'
NULL
for (val in paste0(c('left', 'right', 'top', 'bottom'), '_border_color')) make_getter_setters(val,
  'cell')


#' @name right_border_color
#' @rdname left_border_color
#' @return Similarly for the other functions.
#' @templateVar attr_name right_border_color
#' @template cell-property-usage
NULL


#' @name top_border_color
#' @rdname left_border_color
#' @templateVar attr_name top_border_color
#' @template cell-property-usage
NULL


#' @name bottom_border_color
#' @rdname left_border_color
#' @templateVar attr_name bottom_border_color
#' @template cell-property-usage
NULL


#' @templateVar attr_name left_border_style
#' @templateVar attr_desc Border styles
#' @templateVar value_param_desc A character vector or matrix of styles, which may be "solid", "double", "dashed" or "dotted".
#' @templateVar morealiases right_border_style top_border_style bottom_border_style
#' @templateVar attr_val 'solid'
#' @template getset-cell
#' @template cell-property-usage
#' @details
#' Huxtable collapses borders and border colors. Right borders take priority over left borders, and
#' top borders take priority over bottom borders.
#'
#' Border styles only apply if the border width is greater than 0.
#'
#' @section Quirks:
#'
#' * In HTML, you will need to set a width of at least 3 to get a double border.
#' * Only "solid" and "double" styles are currently implemented in LaTeX.
#'
#' @templateVar attr_val2 'double'
#' @examples
#' ht <- huxtable(a = 1:3, b = 3:1)
#' ht <- set_all_borders(ht, 1)
#' set_left_border_style(ht, 'double')
#' set_left_border_style(ht, 1:2, 1, 'double')
#' set_left_border_style(ht, 1:2, 1:2, c('solid', 'double'), byrow = TRUE)
#' @template border-warning
#' @template cell-property-usage
NULL
for (val in paste0(c('left', 'right', 'top', 'bottom'), '_border_style')) make_getter_setters(val,
      'cell', check_values = c('solid', 'double', 'dashed', 'dotted'))


#' @name right_border_style
#' @rdname left_border_style
#' @return Similarly for the other functions.
#' @templateVar attr_name right_border_style
#' @template cell-property-usage
NULL


#' @name top_border_style
#' @rdname left_border_style
#' @templateVar attr_name top_border_style
#' @template cell-property-usage
NULL


#' @name bottom_border_style
#' @rdname left_border_style
#' @templateVar attr_name bottom_border_style
#' @template cell-property-usage
NULL


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
#' @template cell-property-usage
NULL
for (val in paste0(c('left', 'right', 'top', 'bottom'), '_padding')) make_getter_setters(val, 'cell')


#' @name right_padding
#' @rdname left_padding
#' @return Similarly for the other functions.
#' @templateVar attr_name right_padding
#' @template cell-property-usage
NULL


#' @name top_padding
#' @rdname left_padding
#' @templateVar attr_name top_padding
#' @template cell-property-usage
NULL


#' @name bottom_padding
#' @rdname left_padding
#' @templateVar attr_name bottom_padding
#' @template cell-property-usage
NULL


#' @template getset-cell
#' @templateVar attr_name wrap
#' @templateVar attr_desc Text wrapping
#' @templateVar value_param_desc
#' A logical vector or matrix. If `TRUE`, long cell contents will be wrapped into multiple lines.
#' Set to `NA` for the default.
#' @examples
#' ht <- huxtable(a = rep('Some long text', 2))
#' wrap(ht)[1,] <- TRUE
#' print_html(ht)
#' @template getset-rowspec-example
#' @templateVar attr_val TRUE
#' @templateVar attr_val2 FALSE
#'
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
NULL
make_getter_setters('escape_contents', 'cell', check_fun = is.logical)


#' @template getset-cell
#' @templateVar attr_name na_string
#' @templateVar attr_desc NA string
#' @templateVar value_param_desc
#' A character string. This will be used to replace NA values in the display.
#' @template getset-example
#' @templateVar attr_val '--'
#' @noMd
#' @templateVar extra ht[2,2] <- NA ## print_screen(ht)
#' @template getset-rowspec-example
#' @templateVar attr_val2 ''
#' @family formatting functions
NULL
make_getter_setters('na_string', 'cell', check_fun = is.character)


#' @template getset-cell
#' @templateVar attr_name bold
#' @templateVar attr_desc Cell text style
#' @templateVar value_param_desc
#' A logical vector or matrix. `TRUE` for bold/italic, `FALSE` for plain.
#' @templateVar morealiases italic
#' @template getset-example
#' @templateVar attr_val TRUE
#' @templateVar extra print_screen(ht)
#' @template getset-visible-rowspec-example
#' @templateVar attr_val2 FALSE
#' @family formatting functions
#' @template cell-property-usage
NULL
make_getter_setters('bold', 'cell', check_fun = is.logical)


#' @name italic
#' @rdname bold
#' @templateVar attr_name italic
#' @template cell-property-usage
#' @return
#' Similarly for \code{italic} and friends.
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
NULL
make_getter_setters('font_size', 'cell', check_fun = is.numeric)


#' @template getset-cell
#' @templateVar attr_name rotation
#' @templateVar attr_desc Text rotation
#' @templateVar value_param_desc
#' A numeric vector. Anti-clockwise from the x axis, so 0 is left to right, 90 is going up, etc.
#' @template getset-example
#' @templateVar attr_val 90
#' @template getset-rowspec-example
#' @templateVar attr_val2 270
#' @details
#' You will probably need to set [col_width()] and [row_height()] explicitly
#' to achieve a nice result, in both HTML and LaTeX.
NULL
make_getter_setters('rotation', 'cell', check_fun = is.numeric, extra_code = {value <- value %% 360})


#' @template getset-cell
#' @templateVar attr_name number_format
#' @templateVar attr_desc Number format
#' @templateVar value_param_desc A character or integer vector, or a list containing a function, or \code{NA}.
#' @templateVar NA_does_not_reset TRUE
#' @details
#' Number formatting is applied to any parts of cells that look like numbers (defined as an optional minus sign,
#' followed by
#' numerals, followed by an optional decimal point and further numerals). The exception is exponents in
#' scientific notation; huxtable attempts to detect and ignore these.
#'
#' If `value` is
#' * numeric, numbers will be rounded to that many decimal places;
#' * character, it will be taken as an argument to [sprintf()];
#' * a function, the function will be applied to the numbers;
#' * `NA`, then numbers will not be formatted (except maybe by conversion with `as.character`).
#'
#' Note that if your cells are of type numeric, a number format of `NA` doesn't guarantee you get
#' back what you typed in, since R's default conversion may apply scientific notation and
#' rounding.
#'
#' The default value is "\%.3g", which rounds numbers if they have more than 3 significant
#' digits, and which may use scientific notation for large numbers.
#'
#' To set number_format to a function, enclose the function in `list`. The function should
#' take one argument and return a string.
#'
#' Versions of huxtable before 2.0.0 applied `number_format` only to cells that looked like
#' numbers in their entirety. The default value was "\%5.2f".
#'
#' @family formatting functions
#'
#' @examples
#' ht <- huxtable(
#'   number_format = c("Default", "NA", "2", "\"%5.2f\"", "Pretty", "Sign"),
#'   a = rep(1000, 6),
#'   b = rep(1000.005, 6),
#'   c = rep(0.0001, 6),
#'   d = rep(-1, 6),
#'   e = rep("3.2 (s.e. 1.4)", 6),
#'   add_colnames = TRUE
#' )
#' number_format(ht)[3, -1] <- NA
#' number_format(ht)[4, -1] <- 2
#' number_format(ht)[5, -1] <- '%5.2f'
#' number_format(ht)[6, -1] <- list(function(x) prettyNum(x, big.mark = ',', scientific = FALSE))
#' number_format(ht)[7, -1] <- list(function(x) if(x>0) '+' else '-')
#' right_border(ht) <- 1
#' bottom_border(ht)[1, ] <- 1
#' ht
#'
#' ht_bands <- huxtable("10000 Maniacs", autoformat = FALSE)
#' # probably not what you want:
#' ht_bands
#' number_format(ht_bands) <- NA
#' ht_bands
#' # alternatively:
#' huxtable("10000 Maniacs", autoformat = TRUE)
#' @template getset-visible-rowspec-example
#' @templateVar attr_val 2
#' @templateVar attr_val2 3
NULL
make_getter_setters('number_format', 'cell')


# override the default
`number_format<-.huxtable` <- function(ht, value) {
  stopifnot(all(sapply(value, function (x) is.numeric(x) || is.character(x) || is.function(x) || is.na(x) )))
  attr(ht, 'number_format')[] <- value
  ht
}


#' @aliases pad_decimal<- set_pad_decimal set_pad_decimal_by
#' @rdname huxtable-deprecated
#' @name pad_decimal
#' @details
#' To replace `pad_decimal` use [align()], e.g. `align(ht) <- "."`.
NULL
make_getter_setters('pad_decimal', 'cell', extra_code = {
  stopifnot(all(nchar(na.omit(value)) == 1))
  .Deprecated(msg = "'pad_decimal' is deprecated.\nUse e.g. 'align(x) <- \".\"' instead.",
        package = 'huxtable')
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
#' @template cell-property-usage
NULL
make_getter_setters('font', 'cell', check_fun = is.character)


#' @template getset-table
#' @templateVar attr_name position
#' @templateVar attr_desc Table position
#' @templateVar value_param_desc
#' A length-one character vector which may be 'left', 'center' or 'right'.
#' @details
#' If your tables are too far to the right under LaTeX, try setting their [width()]
#' explicitly.
#' @template getset-example
#' @templateVar attr_val 'right'
NULL
make_getter_setters('position', 'table', check_values = c('left', 'center', 'centre', 'right'),
      extra_code = value[value == 'centre'] <- 'center')

#' @template getset-table
#' @templateVar attr_name caption_pos
#' @templateVar attr_desc Caption position
#' @templateVar value_param_desc
#' A length-one character vector, one of 'top', 'bottom', 'topleft', 'topcenter', 'topright', 'bottomleft', 'bottomcenter', 'bottomright'.
#' @details
#' If `caption_pos` is 'top' or 'bottom', then the horizontal position ('left', 'center' or 'right')
#' will be determined by the huxtable's [position()].
#' @template getset-example
#' @templateVar attr_val 'bottom'
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
#' A length-one vector. If numeric, `value` is treated as a proportion of the surrounding block width (HTML) or text width (LaTeX). If character, it must be a valid CSS or LaTeX width.
#' @template getset-example
#' @templateVar attr_val 0.8
#' @family table measurements
NULL
make_getter_setters('width', 'table')


#' @template getset-table
#' @templateVar attr_name height
#' @templateVar attr_desc Table height
#' @templateVar value_param_desc
#' A length-one vector. If numeric, it is treated as a proportion of the containing block height for HTML, or of text height (`\\textheight`) for LaTeX. If character, it must be a valid CSS or LaTeX width.
#' @template getset-example
#' @templateVar attr_val 0.4
#' @family table measurements
NULL
make_getter_setters('height', 'table')

#' @template getset-table
#' @templateVar attr_name caption
#' @templateVar attr_desc Caption
#' @templateVar value_param_desc
#' A length-one character vector.
#' @details
#' Captions are not escaped. See the example for a workaround.
#' @template getset-example
#' @templateVar attr_val 'An example table'
#' @templateVar extra print_screen(ht)
#' @seealso [caption_pos()]
#' @examples
#' ht <- hux(a = 1:2, b = 1:2)
#' caption(ht) <- sanitize('Make $$$ with us', type = 'latex') # escape caption characters
NULL
make_getter_setters('caption', 'table', check_fun = is.character)


#' @template getset-table
#' @templateVar attr_name tabular_environment
#' @templateVar attr_desc Tabular environment
#' @templateVar value_param_desc
#' A length-one character vector.
#' @template getset-example
#' @templateVar attr_val 'longtable'
#' @details No features are guaranteed to work if you set this to a non-default value. Use at your own risk!
NULL
make_getter_setters('tabular_environment', 'table', check_fun = is.character)


#' @template getset-table
#' @templateVar attr_name label
#' @templateVar attr_desc Table label
#' @templateVar value_param_desc
#' A length-one character vector to be used as a table label in LaTeX, or as an ID for the table in HTML.
#' @template getset-example
#' @templateVar attr_val 'tab:mytable'
#' @details
#' LaTeX table labels typically start with "tab:", and they must do so if you want table numbering
#' in \href{http://bookdown.org}{bookdown}.
NULL
make_getter_setters('label', 'table', check_fun = is.character)


#' @template getset-table
#' @templateVar attr_name latex_float
#' @templateVar attr_desc Float position for LaTeX
#' @templateVar value_param_desc
#' A length-one character vector, used by LaTeX for positioning the float.
#' @template getset-example
#' @templateVar attr_val 'h'
#' @details Quick reference: 'h' here, 'h!' definitely here, 't' top of page, 'b' bottom of page, 'p' page of
#' floats. See LaTeX documentation for more details. If you use 'H' (definitely here), you must require the
#' TeX `float` package.
NULL
make_getter_setters('latex_float', 'table', check_fun = is.character)
