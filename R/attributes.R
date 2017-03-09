

huxtable_cell_attrs <- c('align', 'valign', 'rowspan', 'colspan', 'background_color', 'text_color',
  'top_border', 'left_border', 'right_border', 'bottom_border',
  'top_padding', 'left_padding', 'right_padding', 'bottom_padding', 'wrap',
  'escape_contents', 'na_string', 'bold', 'italic', 'font_size', 'rotation', 'number_format',
  'font')
huxtable_col_attrs <- c('col_width')
huxtable_row_attrs <- c('row_height')
huxtable_table_attrs <- c('width', 'height', 'position', 'caption', 'caption_pos', 'tabular_environment', 'label')
huxtable_default_attrs <- list(
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
        escape_contents     = TRUE,
        na_string           = '',
        bold                = FALSE,
        italic              = FALSE,
        font_size           = NA,
        rotation            = 0,
        number_format       = list('%5.2f'),
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

  check_fun <- if (! missing(check_fun)) bquote(stopifnot(.(check_fun)(value)))
  check_dims <- switch(attr_type,
    table = quote(stopifnot(length(value) == 1))
  )
  check_values <- if (! missing(check_values)) bquote(
    stopifnot(all(na.omit(value) %in% .(check_values)))
  )
  extra_code <- if (! missing(extra_code)) substitute(extra_code)
  funs[[paste0(setter, '.huxtable')]] <- eval(bquote(
    function(ht, value) {
      .(check_fun)
      .(check_dims)
      .(check_values)
      .(extra_code)
      value[is.na(value)] <- huxtable_default_attrs[[.(attr_name)]]
      attr(ht, .(attr_name))[] <- value
      ht
    }
  ))

  alt_setter <- paste0('set_', attr_name)

  if (attr_type == 'cell') {
  funs[[alt_setter]] <- eval(bquote(
    function(ht, row, col, value) {
      .(as.name(attr_name))(ht)[row, col] <- value
      ht
    }
  ))
  } else if (attr_type == 'row') {
    funs[[alt_setter]] <- eval(bquote(
      function(ht, row, value) {
        .(as.name(attr_name))(ht)[row] <- value
        ht
      }
    ))
  } else if (attr_type == 'col') {
    funs[[alt_setter]] <- eval(bquote(
      function(ht, col, value) {
        .(as.name(attr_name))(ht)[col] <- value
        ht
      }
    ))
  } else if (attr_type == 'table') {
    funs[[alt_setter]] <- eval(bquote(
      function(ht, value) {
        .(as.name(attr_name))(ht) <- value
        ht
      }
    ))
  }

  lapply(names(funs), function (x) {
    assign(x, funs[[x]], envir = parent.frame(3)) # 3: 1 for function(x), 2 for lapply, 3 for the caller!
  })

  NULL
}


#' @template getset-cell
#' @templateVar attr_name valign
#' @templateVar attr_desc Vertical Alignment
#' @templateVar value_param_desc A character vector or matrix which may be 'top', 'middle', 'bottom' or \code{NA}.
#' @template getset-example
#' @templateVar attr_val 'top'
#' @export valign valign<- set_valign valign.huxtable valign<-.huxtable
NULL
make_getter_setters('valign', 'cell', check_fun = is.character, check_values = c('top', 'middle', 'bottom'))

#' @template getset-cell
#' @templateVar attr_name align
#' @templateVar attr_desc Alignment
#' @templateVar value_param_desc A character vector or matrix which may be 'left', 'center', 'right' or \code{NA}.
#' @template getset-example
#' @templateVar attr_val 'right'
#' @export align align<- set_align align.huxtable align<-.huxtable
NULL
make_getter_setters('align', 'cell', check_fun = is.character, check_values = c('left', 'center', 'right'))

#' @template getset-rowcol
#' @templateVar attr_name col_width
#' @templateVar rowcol col
#' @templateVar attr_desc Column Widths
#' @templateVar value_param_desc A vector. If numeric, they are treated as proportions of the table width. If character, they must be valid CSS or LaTeX lengths.
#' @family row/column heights
#' @template getset-example
#' @templateVar attr_val c(.2, .8)
#' @templateVar extra print_screen(ht)
#' @export col_width col_width<- set_col_width col_width.huxtable col_width<-.huxtable
NULL
make_getter_setters('col_width', 'col')


#' @template getset-rowcol
#' @templateVar attr_name row_height
#' @templateVar rowcol row
#' @templateVar attr_desc Row Heights
#' @templateVar value_param_desc A vector.
#' @family row/column heights
#' @details
#' If character, \code{value} must contain valid CSS or LaTeX lengths. If numeric, in HTML, values are scaled to 1 and treated as proportions of the table height. In LaTeX, they are
#' treated as proportions of the text height (\\textheight).
#' @template getset-example
#' @templateVar attr_val c(.2, .1, .1)
#' @export row_height row_height<- set_row_height row_height.huxtable row_height<-.huxtable
NULL
make_getter_setters('row_height', 'row')

#' @template getset-cell
#' @templateVar attr_name rowspan
#' @templateVar attr_desc Row Span
#' @templateVar value_param_desc An integer vector or matrix of integers.
#' @template getset-example
#' @templateVar subscript [1, 1]
#' @templateVar attr_val 2
#' @templateVar extra print_screen(ht, blank = '.')
#' @export rowspan rowspan<- set_rowspan rowspan.huxtable rowspan<-.huxtable
NULL
make_getter_setters('rowspan', 'cell', check_fun = is.numeric, extra_code =
    {
      if (any(na.omit( row(ht) + value - 1 > nrow(ht) ))) stop('rowspan would extend beyond bottom of table')
      check_span_shadows(ht, 'row', value)
    }
)

#' @template getset-cell
#' @templateVar attr_name colspan
#' @templateVar attr_desc Column Span
#' @templateVar value_param_desc An integer vector or matrix of integers.
#' @template getset-example
#' @templateVar subscript [1, 1]
#' @templateVar attr_val 2
#' @templateVar extra print_screen(ht, blank = '.')
#' @export colspan colspan<- set_colspan colspan.huxtable colspan<-.huxtable
NULL
make_getter_setters('colspan', 'cell', check_fun = is.numeric, extra_code =
    {
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
    candidates <- candidates[problems > 1]
    candidates <- paste(apply(candidates, 1, paste, collapse = ','), collapse = '; ')
    stop('New rowspan/colspan would cut up existing multirow/multicol cells at ', candidates)
  }
}

#' @template getset-cell
#' @templateVar attr_name background_color
#' @templateVar attr_desc Cell Background Color
#' @templateVar value_param_desc A vector or matrix of R colors.
#' @template getset-example
#' @templateVar attr_val grey(.95)
#' @family formatting functions
#' @export background_color background_color<- set_background_color background_color.huxtable background_color<-.huxtable
NULL
make_getter_setters('background_color', 'cell')

#' @template getset-cell
#' @templateVar attr_name text_color
#' @templateVar attr_desc Cell Text Color
#' @templateVar value_param_desc A vector or matrix of R colors.
#' @template getset-example
#' @templateVar attr_val 'navy'
#' @family formatting functions
#' @export text_color text_color<- set_text_color text_color.huxtable text_color<-.huxtable
NULL
make_getter_setters('text_color', 'cell')


#' @template getset-cell
#' @templateVar attr_name left_border
#' @templateVar attr_desc Left Border
#' @templateVar value_param_desc A numeric vector or matrix giving border widths. Set to 0 for no border.
#' @template getset-example
#' @templateVar attr_val 1
#' @templateVar extra print_screen(ht)
#' @family borders
#' @export left_border left_border<- set_left_border left_border.huxtable left_border<-.huxtable
NULL
make_getter_setters('left_border', 'cell', check_fun = is.numeric)

#' @template getset-cell
#' @templateVar attr_name right_border
#' @templateVar attr_desc Right Border
#' @templateVar value_param_desc A numeric vector or matrix giving border widths. Set to 0 for no border.
#' @template getset-example
#' @templateVar attr_val 1
#' @templateVar extra print_screen(ht)
#' @family borders
#' @export right_border right_border<- set_right_border right_border.huxtable right_border<-.huxtable
NULL
make_getter_setters('right_border', 'cell', check_fun = is.numeric)

#' @template getset-cell
#' @templateVar attr_name top_border
#' @templateVar attr_desc Top Border
#' @templateVar value_param_desc A numeric vector or matrix giving border widths. Set to 0 for no border.
#' @template getset-example
#' @templateVar attr_val 1
#' @templateVar extra print_screen(ht)
#' @family borders
#' @export top_border top_border<- set_top_border top_border.huxtable top_border<-.huxtable
NULL
make_getter_setters('top_border', 'cell', check_fun = is.numeric)

#' @template getset-cell
#' @templateVar attr_name bottom_border
#' @templateVar attr_desc Bottom Border
#' @templateVar value_param_desc A numeric vector or matrix giving border widths. Set to 0 for no border.
#' @template getset-example
#' @templateVar attr_val 1
#' @templateVar extra print_screen(ht)
#' A numeric vector or matrix giving border widths. Set to 0 for no border.
#' @family borders
#' @export bottom_border bottom_border<- set_bottom_border bottom_border.huxtable bottom_border<-.huxtable
NULL
make_getter_setters('bottom_border', 'cell', check_fun = is.numeric)

#' Set All Borders
#'
#' @inheritParams left_border
#'
#' @details This is a convenience function which sets left, right, top and bottom borders
#' for the specified cells.
#'
#' @return The modified `ht` object.
#' @export
#'
#' @examples
#' ht <- huxtable(a = 1:3, b = 1:3)
#' ht <- set_all_borders(ht, 1:3, 1:2, 1)
set_all_borders <- function(ht, row, col, value) {
  top_border(ht)[row, col] <- value
  bottom_border(ht)[row, col] <- value
  left_border(ht)[row, col] <- value
  right_border(ht)[row, col] <- value
  ht
}


#' @name left_padding
#' @template getset-cell
#' @templateVar attr_name left_padding
#' @templateVar attr_desc Cell Padding
#' @templateVar value_param_desc
#' A vector or matrix. Characters must be valid CSS or LaTeX lengths. Numbers will be interpreted as lengths in points.
#' @templateVar morealiases right_padding top_padding bottom_padding right_padding<-
#' @template getset-example
#' @templateVar attr_val 20
#' @export left_padding left_padding<- set_left_padding left_padding.huxtable left_padding<-.huxtable
NULL
for (val in paste0(c('left', 'right', 'top', 'bottom'), '_padding')) make_getter_setters(val, 'cell')


#' @name right_padding
#' @rdname left_padding
#' @return Similarly for the other functions.
#' @usage
#' right_padding(ht)
#' right_padding(ht) <- value
#' set_right_padding(ht, row, col, value)
#' @export right_padding right_padding<- set_right_padding right_padding.huxtable right_padding<-.huxtable
NULL

#' @name bottom_padding
#' @rdname left_padding
#' @usage
#' bottom_padding(ht)
#' bottom_padding(ht) <- value
#' set_bottom_padding(ht, row, col, value)
#' @export bottom_padding bottom_padding<- set_bottom_padding bottom_padding.huxtable bottom_padding<-.huxtable
NULL

#' @name top_padding
#' @rdname left_padding
#' @usage
#' top_padding(ht)
#' top_padding(ht) <- value
#' set_top_padding(ht, row, col, value)
#' @export top_padding top_padding<- set_top_padding top_padding.huxtable top_padding<-.huxtable
NULL


#' Set All Padding
#'
#' @inheritParams left_padding
#'
#' @details This is a convenience function which sets left, right, top and bottom cell padding
#' for the specified cells.
#'
#' @return The modified `ht` object.
#' @export
#'
#' @examples
#' ht <- huxtable(a = 1:3, b = 1:3)
#' ht <- set_all_padding(ht, 1:3, 1:2, "20px")
set_all_padding <- function(ht, row, col, value) UseMethod('set_all_padding')

#' @export
set_all_padding.huxtable <- function(ht, row, col, value) {
  top_padding(ht)[row, col] <- value
  bottom_padding(ht)[row, col] <- value
  left_padding(ht)[row, col] <- value
  right_padding(ht)[row, col] <- value
  ht
}


#' @template getset-cell
#' @templateVar attr_name wrap
#' @templateVar attr_desc Cell Wrapping
#' @templateVar value_param_desc
#' A logical vector or matrix. If \code{TRUE}, long cell contents will be wrapped into multiple lines. Set to \code{NA} for the default.
#' @examples
#' ht <- huxtable(a = rep('Some long text', 2))
#' wrap(ht)[1,] <- TRUE
#' print_html(ht)
#' @export wrap wrap<- set_wrap wrap.huxtable wrap<-.huxtable
NULL
make_getter_setters('wrap', 'cell', check_fun = is.logical)


#' @template getset-cell
#' @templateVar attr_name escape_contents
#' @templateVar attr_desc Whether to Escape Cell Contents
#' @templateVar value_param_desc
#' A logical vector or matrix. If \code{TRUE}, cell contents will be HTML or LaTex escaped.
#' @examples
#' ht <- huxtable(Exponent = 2:4, Example = paste0('$x^', 2:4, '$'))
#' escape_contents(ht)[,2] <- FALSE
#'
#' @export escape_contents escape_contents<- set_escape_contents escape_contents.huxtable escape_contents<-.huxtable
NULL
make_getter_setters('escape_contents', 'cell', check_fun = is.logical)


#' @template getset-cell
#' @templateVar attr_name na_string
#' @templateVar attr_desc String to Use For NA
#' @templateVar value_param_desc
#' A character string. This will be used to replace NA values in the display. Set to \code{NA} for the default, which is the empty string. To get literal "NA", set to "NA".
#' @template getset-example
#' @templateVar attr_val '--'
#' @templateVar extra print_screen(ht)
#' @family formatting functions
#' @export na_string na_string<- set_na_string na_string.huxtable na_string<-.huxtable
NULL
make_getter_setters('na_string', 'cell', check_fun = is.character)


#' @template getset-cell
#' @templateVar attr_name bold
#' @templateVar attr_desc Bold or Italic Cell Text
#' @templateVar value_param_desc
#' A logical vector or matrix
#' @templateVar morealiases italic italic<- set_italic italic.huxtable italic<-.huxtable
#' @template getset-example
#' @templateVar attr_val TRUE
#' @templateVar extra print_screen(ht)
#' @family formatting functions
#' @export bold bold<- set_bold bold.huxtable bold<-.huxtable
NULL
make_getter_setters('bold', 'cell', check_fun = is.logical)


#' @name italic
#' @rdname bold
#' @usage
#' italic(ht)
#' italic(ht) <- value
#' set_italic(ht, row, col, value)
#' @export italic italic<- set_italic italic.huxtable italic<-.huxtable
NULL
make_getter_setters('italic', 'cell', check_fun = is.logical)


#' @template getset-cell
#' @templateVar attr_name font_size
#' @templateVar attr_desc Font Size
#' @templateVar value_param_desc
#' A numeric vector. This sets the font size in points.
#' @template getset-example
#' @templateVar attr_val 14
#' @family formatting functions
#' @export font_size font_size<- set_font_size font_size.huxtable font_size<-.huxtable
NULL
make_getter_setters('font_size', 'cell', check_fun = is.numeric)


#' @template getset-cell
#' @templateVar attr_name rotation
#' @templateVar attr_desc Cell Text Rotation
#' @templateVar value_param_desc
#' A numeric vector. 0 is normal direction, 90 is going up, etc.
#' @template getset-example
#' @templateVar attr_val 90
#' @details
#' You will likely need to set \code{\link{col_width}} and \code{\link{row_height}} explicitly
#' to achieve a nice result, in both HTML and LaTeX.
#' @export rotation rotation<- set_rotation rotation.huxtable rotation<-.huxtable
NULL
make_getter_setters('rotation', 'cell', check_fun = is.numeric)

#' @template getset-cell
#' @templateVar attr_name number_format
#' @templateVar attr_desc Number Format
#' @templateVar value_param_desc
#' A vector or list which may be character, numeric or function. See below.
#'
#' @details
#' If \code{value} is numeric, numbers will be rounded to that many decimal places.  If \code{value} is
#' character, it will be taken as an argument to \code{\link{sprintf}}. If \code{value} is a
#' function it will be applied to the cell contents.
#' Number format is applied to any cells that look like numbers (as judged by \code{\link{as.numeric}}), not just to numeric cells. This allows you to do e.g. \code{ht <- huxtable(a = c('Salary', 35000, 32000, 40000))} and still format numbers correctly.
#' To set number_format to a function, enclose the function in \code{list}.
#' See the examples.
#' @export number_format number_format<- set_number_format number_format.huxtable number_format<-.huxtable
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
NULL
make_getter_setters('number_format', 'cell')

# override the default
`number_format<-.huxtable` <- function(ht, value) {
  stopifnot(all(sapply(value, function (x) is.numeric(x) || is.character(x) || is.function(x) )))
  if (is.atomic(value) || is.list(value)) value[is.na(value)] <- huxtable_default_attrs[['number_format']]
  attr(ht, 'number_format')[] <- value
  ht
}



#' @template getset-cell
#' @templateVar attr_name font
#' @templateVar attr_desc Font
#' @templateVar value_param_desc
#' A character vector of font names. NB that LaTeX and HTML use different font names.
#' @template getset-example
#' @templateVar attr_val 'times'
#' @family formatting functions
#' @export font font<- set_font font.huxtable font<-.huxtable
NULL
make_getter_setters('font', 'cell', check_fun = is.character)


#' @template getset-table
#' @templateVar attr_name position
#' @templateVar attr_desc Table Position
#' @templateVar value_param_desc
#' A length-one character vector which may be 'left', 'center', 'right' or \code{NA}.
#' @template getset-example
#' @templateVar attr_val 'right'
#' @export position position<- set_position position.huxtable position<-.huxtable
NULL
make_getter_setters('position', 'table', check_values = c('left', 'center', 'right'))

#' @template getset-table
#' @templateVar attr_name caption_pos
#' @templateVar attr_desc Caption Position
#' @templateVar value_param_desc
#' A length-one character vector which can be 'top', 'bottom' or \code{NA} for the default.
#' @template getset-example
#' @templateVar attr_val 'top'
#' @export tabular_environment tabular_environment<- set_tabular_environment tabular_environment.huxtable tabular_environment<-.huxtable
#' @seealso \code{\link{caption}}
NULL
make_getter_setters('caption_pos', 'table', check_values = c('top', 'bottom'))


#' @template getset-table
#' @templateVar attr_name width
#' @templateVar attr_desc Table Width
#' @templateVar value_param_desc
#' A length-one vector. If numeric, \code{value} is treated as a proportion of the surrounding block width (HTML) or text width (LaTeX). If character, it must be a valid CSS or LaTeX width. Set to \code{NA} for the default.
#' @template getset-example
#' @templateVar attr_val 0.8
#' @family table measurements
#' @export width width<- set_width width.huxtable width<-.huxtable
NULL
make_getter_setters('width', 'table')


#' @template getset-table
#' @templateVar attr_name height
#' @templateVar attr_desc Table Height
#' @templateVar value_param_desc
#' A length-one vector. If numeric, it is treated as a proportion of the containing block height for HTML, or of text height (\\textheight) for LaTeX. If character, it must be a valid CSS or LaTeX width. Set to \code{NA} for the default, which is to leave height unset.
#' @template getset-example
#' @templateVar attr_val 0.4
#' @family table measurements
#' @export height height<- set_height height.huxtable height<-.huxtable
NULL
make_getter_setters('height', 'table')

#' @template getset-table
#' @templateVar attr_name caption
#' @templateVar attr_desc Caption
#' @templateVar value_param_desc
#' A length-one character vector. Set to \code{NA} for no caption.
#' @export caption caption<- set_caption caption.huxtable caption<-.huxtable
#' @template getset-example
#' @templateVar attr_val 'An example table'
#' @templateVar extra print_screen(ht, blank = '.')
#' @seealso \code{\link{caption_pos}}
NULL
make_getter_setters('caption', 'table', check_fun = is.character)


#' @template getset-table
#' @templateVar attr_name tabular_environment
#' @templateVar attr_desc Tabular Environment
#' @templateVar value_param_desc
#' A length-one character vector. Set to \code{NA} for the default, 'tabularx'.
#' @template getset-example
#' @templateVar attr_val 'longtable'
#' @details No features are guaranteed to work if you set this to a non-default value. Use at your own risk!
#' @export caption_pos caption_pos<- set_caption_pos caption_pos.huxtable caption_pos<-.huxtable
NULL
make_getter_setters('tabular_environment', 'table', check_fun = is.character)


#' @template getset-table
#' @templateVar attr_name label
#' @templateVar attr_desc Table Label
#' @templateVar value_param_desc
#' A length-one character vector to be used as a table label in LaTeX, or as an ID for the table in HTML. Set to \code{NA} to remove any label.
#' @template getset-example
#' @templateVar attr_val 'tab:mytable'
#' @export label label<- set_label label.huxtable label<-.huxtable
NULL
make_getter_setters('label', 'table', check_fun = is.character)


# utility functions

# return formatted contents, suitably escaped
clean_contents <- function(ht, row, col, type = c('latex', 'html', 'screen', 'markdown'), ...) {
  mytype <- match.arg(type)
  # stopifnot(length(row) == 1 & length(col) == 1)
  contents <- ht[[row, col]] # just the data and just one element.
  # But we might want to allow more than one element; if so just use `[.data.frame`
  if (! is.na(cnum <- suppressWarnings(as.numeric(contents)))) {
    nf <- number_format(ht)[[row, col]] # a list element
    if (is.function(nf)) contents <- nf(cnum)
    if (is.character(nf)) contents <- sprintf(nf, cnum)
    if (is.numeric(nf)) contents <- formatC(round(cnum, nf), format = 'f', digits = nf)
  }

  if (is.na(contents)) contents <- na_string(ht)[row, col]
  if (escape_contents(ht)[row, col] && type %in% c('latex', 'html')) {
    # xtable::sanitize.numbers would do very little and is buggy
    contents <-  xtable::sanitize(contents, type = mytype)
  }

  contents
}

# return data frame mapping real cell positions to cells displayed
display_cells <- function(ht, new_rowspan = rowspan(ht), new_colspan = colspan(ht)) {
  dcells <- data.frame(row = rep(1:nrow(ht), ncol(ht)), col = rep(1:ncol(ht), each = nrow(ht)),
    rowspan = as.vector(new_rowspan), colspan = as.vector(new_colspan))
  dcells$display_row <- dcells$row
  dcells$display_col <- dcells$col
  dcells$shadowed <- FALSE

  change_cols <- c('display_row', 'display_col', 'rowspan', 'colspan')
  for (i in 1:nrow(dcells)) {
    if (dcells$rowspan[i] == 1 && dcells$colspan[i] == 1) next
    if (dcells$shadowed[i]) next

    dr <- dcells$row[i]
    dc <- dcells$col[i]
    spanned <- dcells$row %in% dr:(dr + dcells$rowspan[i] - 1) & dcells$col %in% dc:(dc + dcells$colspan[i] - 1)
    dcells[spanned, change_cols] <- matrix(as.numeric(dcells[i, change_cols]), sum(spanned), length(change_cols), byrow = TRUE)

    shadowed <- spanned & (1:nrow(dcells)) != i
    dcells$shadowed[shadowed] <- TRUE
  }
  dcells$end_row <- dcells$display_row + dcells$rowspan - 1
  dcells$end_col <- dcells$display_col + dcells$colspan - 1

  dcells
}
