

#' @import assertthat
#' @importFrom stats na.omit
NULL

#' @export
#' @rdname to_screen
print_screen <- function(ht, ...) cat(to_screen(ht, ...))


#' Print a huxtable on screen
#'
#' @param ht A huxtable.
#' @param ... Passed on to `to_screen`.
#' @param min_width Minimum width in on-screen characters of the result.
#' @param max_width Maximum width in on-screen characters of the result. Overrides `min_width`.
#' @param compact Logical. To save space, don't print lines for empty horizontal borders.
#' @param colnames Logical. Whether or not to print colum names.
#' @param color Logical. Whether to print the huxtable in color (requires the `crayon` package).
#'
#' @return `to_screen` returns a string. `print_screen` prints the string and returns `NULL`.
#'
#' @details
#' Screen display shows the following features:
#'
#' * Table and caption positioning
#' * Merged cells
#' * Cell alignment
#' * Borders
#' * Cell background and border color (if the "crayon" package is installed)
#' * Text color, bold and italic (if the "crayon" package is installed)
#'
#' Cell padding, widths and heights are not shown, nor are border styles.
#'
#' @export
#' @family printing functions
#' @examples
#' bottom_border(jams)[1, 1:2] <- 1
#' bold(jams)[1, 1:2] <- TRUE
#' jams <- map_text_color(jams,
#'       by_regex("berry" = "red"))
#'
#' print_screen(jams)
to_screen  <- function (ht, ...) UseMethod("to_screen")


#' @export
#' @rdname to_screen
to_screen.huxtable <- function (
        ht,
        min_width = ceiling(getOption("width") / 6),
        max_width = getOption("width", Inf),
        compact   = TRUE,
        colnames  = TRUE,
        color     = getOption("huxtable.color_screen", default = TRUE),
        ...
      ) {
  assert_that(is.number(min_width), is.number(max_width), is.flag(compact), is.flag(colnames), is.flag(color))
  if (color && ! requireNamespace("crayon", quietly = TRUE)) {
    warning("Cannot print huxtable in color as `crayon` package is not installed. Try `install.packages(\"crayon\")`.",
          "To avoid seeing this message in future, set `options(huxtable.color_screen = FALSE)`.")
    color <- FALSE
  }

  all_colnames <- colnames(ht)
  last_ht_col <- orig_ncol <- ncol(ht)
  if (ncol(ht) > 0 && nrow(ht) > 0) {
    charmat_data <- character_matrix(ht, inner_border_h = 3, outer_border_h = 2, inner_border_v = 1, outer_border_v = 1,
          min_width = min_width, max_width = max_width, color = color, markdown = FALSE)
    charmat <- charmat_data$charmat
    border_rows <- charmat_data$border_rows
    border_cols <- charmat_data$border_cols
    last_ht_col <- charmat_data$last_ht_col
    width_mat <- charmat_data$width_mat
    ht <- ht[, seq_len(last_ht_col)]
    border_cols[-1] <- border_cols[-1] + 1 # middle of 3 for interior, last of 2 for last outer

    borders <- get_visible_borders(ht)
    border_mat <- matrix(1L, nrow = nrow(charmat), ncol = ncol(charmat))
    # converts a row/col number to a sequence of charmat row/col numbers for the relevant *column/row*
    index_rows <- lapply(seq_len(nrow(ht)), function (x) seq(border_rows[x], border_rows[x + 1] - 1))
    index_cols <- lapply(seq_len(ncol(ht)), function (x) seq(border_cols[x], border_cols[x + 1] - 1))
    # borders$vert is row, col+1; $horiz is row+1, col
    for (i in seq_len(nrow(ht) + 1)) for (j in seq_len(ncol(ht) + 1)) {
      if (i <= nrow(ht)) {
        ir <- index_rows[[i]]
        # 1: has a line above:
        border_mat[ ir, border_cols[j] ]     <- border_mat[ ir, border_cols[j] ]     + 1L * (borders$vert[i, j] > 0)
        # 2: has a line below:
        border_mat[ ir + 1, border_cols[j] ] <- border_mat[ ir + 1, border_cols[j] ] + 2L * (borders$vert[i, j] > 0)
      }
      if (j <= ncol(ht)) {
        ic <- index_cols[[j]]
        # 4: a line on right:
        border_mat[ border_rows[i], ic ]    <- border_mat[ border_rows[i], ic ]     + 4L * (borders$horiz[i, j] > 0)
        # 8: a line on left:
        border_mat[ border_rows[i], ic + 1] <- border_mat[ border_rows[i], ic + 1 ] + 8L * (borders$horiz[i, j] > 0)
      }
    }

    pipe_chars <- c(NA,
          "\u2502", "\u2502", "\u2502", "\u2500",
          "\u250c", "\u2514", "\u251c", "\u2500",
          "\u2510", "\u2518", "\u2524", "\u2500",
          "\u252c", "\u2534", "\u253c")
    border_mat[] <- pipe_chars[border_mat]
    charmat[ ! is.na(border_mat) ] <- border_mat[ ! is.na(border_mat) ]

    if (color) {
      bcolors <- collapsed_border_colors(ht)
      unique_cols <- unique(na.omit(unlist(bcolors)))
      col_funs <- lapply(unique_cols, crayon::make_style)
      names(col_funs) <- unique_cols
      for (i in seq_len(nrow(ht) + 1)) for (j in seq_len(ncol(ht) + 1)) {
        if (i <= nrow(ht)) {
          # colour vertical borders:
          ir <- index_rows[[i]]
          color_fun <- col_funs[[ bcolors$vert[i, j] ]]
          if (! is.na(bcolors$vert[i, j])) charmat[ ir, border_cols[j] ] <- color_fun(charmat[ ir, border_cols[j] ])
        }
        if (j <= ncol(ht)) {
          # horizontal borders:
          ic <- c(index_cols[[j]], max(index_cols[[j]]) + 1) # rows extend a little bit to cover ends
          color_fun <- col_funs[[ bcolors$horiz[i, j] ]]
          if (! is.na(bcolors$horiz[i, j])) charmat[ border_rows[i], ic ] <- color_fun(charmat[ border_rows[i], ic ])
        }
      }
    }

    if (compact) {
      empty_borders <- apply(charmat, 1, function (x)
            all(grepl(" ", x, fixed = TRUE) | grepl("\u2502", x, fixed = TRUE)))
      empty_borders <- intersect(border_rows, which(empty_borders))
      # length statement necessary otherwise we end up doing charmat[ - integer(0), ] and getting nothing
      if (length(empty_borders) > 0) {
        charmat <- charmat[ -empty_borders, , drop = FALSE]
      }
    }

    result <- apply(charmat, 1, paste0, collapse = "")
    # we can't use conventional string padding because of colour strings
    # instead this horrible hack uses the uncoloured widths, and adds 1 for
    # any borders we find.
    width_mat <- pmax(width_mat, 1)
    width_mat[charmat == ""] <- 0
    row_char_widths <- rowSums(width_mat)
    pad_width <- min(max_width, getOption("width", 80))
    pad_width <- pad_width - max(row_char_widths)
    pad_width <- switch(position_no_wrap(ht),
            "left"   = 0,
            "right"  = pad_width,
            "center" = floor(pad_width/2)
          )
    result <- paste0(strrep(" ", pad_width), result)
    result <- paste(result, collapse = "\n")
  } else {
    # 0-nrow or 0-ncol huxtable
    result <- glue::glue("<huxtable with {nrow(ht)} rows and {ncol(ht)} columns>")
  }
  if (! is.na(cap <- caption(ht))) {
    poss_pos <- c("left", "center", "right")
    hpos <- if (any(found <- sapply(poss_pos, grepl, x = caption_pos(ht)))) poss_pos[found] else
          position_no_wrap(ht)
    if (ncharw(cap) > max_width) cap <- strwrap(cap, max_width)
    # first we pad to same size as charmat.
    stringr_side <- switch(hpos, left = "right", right = "left", center = "both")
    cap <- stringr::str_pad(cap, ncol(charmat) - 4, stringr_side)
    # We do a little bit of "both" padding to compensate for borders
    cap <- stringr::str_pad(cap, ncol(charmat), "both")
    # then we pad out like charmat.
    # Result: the caption stays within the boundaries of the table if possible.
    cap <- paste0(pad_position(cap, position_no_wrap(ht), max_width), collapse = "\n")
    result <- if (grepl("top", caption_pos(ht))) paste0(cap, "\n", result) else paste0(result, "\n", cap)
  }


  if (colnames && any(nchar(all_colnames) > 0)) {
    colnames_text <- paste0("Column names: ", paste(all_colnames, collapse = ", "))
    colnames_text <- strwrap(colnames_text, max_width)
    colnames_text <- paste0(colnames_text, collapse = "\n")
    result <- paste0(result, "\n\n", colnames_text, "\n")
  }
  if (last_ht_col < orig_ncol) result <- glue::glue(
        "{result}\n{last_ht_col}/{orig_ncol} columns shown.\n")

  result
}


# calculate text column widths, wrap huxtable text accordingly, and return a
# matrix of characters, without borders
character_matrix <- function (
          ht,
          inner_border_h,
          inner_border_v,
          outer_border_h,
          outer_border_v,
          min_width,
          max_width = Inf,
          color = FALSE,
          markdown) {

  if (ncol(ht) == 0) stop("Couldn't display any columns in less than max_width characters.")

  dc <- display_cells(ht, all = FALSE)
  dc <- dc[order(dc$colspan), ]
  contents <- clean_contents(ht, type = if (markdown) "markdown" else "screen")
  drow_mat <- as.matrix(dc[, c("display_row", "display_col")])

  dc$contents <- contents[drow_mat]
  cw <- col_width(ht)
  if (! is.numeric(cw) || anyNA(cw)) cw <- rep(1, ncol(ht))
  cw <- cw / sum(cw)

  min_widths <- ceiling(min_width * cw)
  widths <- min_widths

  content_widths <- ncharw(dc$contents)
  # return 0 for empty cells. We don't use "\\s"; non-breaking spaces, returned by decimal_pad, are excluded
  # this is risky because we might screw up some locales...
  max_word_widths <- sapply(lapply(strsplit(dc$contents, "(\t|\n|\r|\v )"), ncharw),
    function (x)  max(c(0, x))
  )

  ###########################################
  # calculate widths to make room for content
  for (r in seq_len(nrow(dc))) {
    width <- if (wrap(ht)[ dc$display_row[r], dc$display_col[r] ]) {
      max_word_widths[r]
    } else {
      content_widths[r]
    }
    if (markdown && bold(ht)[dc$display_row[r], dc$display_col[r]]) {
      width <- width + 4
    }
    if (markdown && italic(ht)[dc$display_row[r], dc$display_col[r]]) {
      width <- width + 2
    }
    cols <- seq(dc$display_col[r], dc$end_col[r])
    # allows for width of interior borders if a cell spans multiple columns
    if (sum(widths[cols]) + inner_border_h * (dc$colspan[r] - 1) < width) {
      widths[cols] <- pmax(widths[cols], ceiling(width / dc$colspan[r]))
    }
  }

  #############################################################################
  # shrink widths to make content fit into max_width, taking account of borders
  max_widths <- floor(cw * (max_width - 2 * outer_border_h - (ncol(ht) - 1) * inner_border_h))
  if (any(max_widths < 8)) {
    # out of space, try with fewer columns
    # assumption here is that we need say 2 characters for anything sensible; and could have
    # bold+italic which adds *** before and after!
    return(character_matrix(ht[, -ncol(ht)], inner_border_h, inner_border_v, outer_border_h,
          outer_border_v, min_width, max_width, color, markdown = markdown))
  }
  widths <- pmin(widths, max_widths)

  ######################################
  # cut up strings to fit inside widths:
  dc$strings <- vector(nrow(dc), mode = "list")
  for (r in seq_len(nrow(dc))) {
    dcell <- dc[r, ]
    col <- dcell$display_col
    end_col <- dcell$end_col
    width <- sum(widths[col:end_col])
    md_bold <- markdown && bold(ht)[dcell$display_row, dcell$display_col]
    md_italic <- markdown && italic(ht)[dcell$display_row, dcell$display_col]
    eff_width <- width
    if (md_bold) eff_width <- eff_width - 4
    if (md_italic) eff_width <- eff_width - 2
    # strwrap treats non-breaking spaces differently than breaking spaces; this can fuck things up
    # with decimal padding. So all END spaces become non-breaking.
    while (! identical(new <- gsub("( |\u00a0)$", "\u00a0", dcell$contents), dcell$contents)) {
      dcell$contents <- new
    }
    # double newlines split paragraphs:
    dcell$contents <- gsub("\n", "\n\n", dcell$contents, fixed = TRUE)
    strings <- strwrap(dcell$contents, width = eff_width + 1) # for the + 1 see ?strwrap
    # don't use blank line to separate paragraphs:
    strings <- strings[strings != ""]
    # some strings may still be longer than width:
    strings <- unlist(lapply(strings, function (x) {
      while (any(ncharw(x) > eff_width)) {
        lx <- length(x)
        last <- x[lx]
        last <- c(substring(last, 1, eff_width), substring(last, eff_width + 1))
        x[lx:(lx + 1)] <- last
      }
      x
    }))
    if (md_bold) strings[ncharw(strings) > 0] <- paste0("**", strings[ncharw(strings) > 0], "**")
    if (md_italic) strings[ncharw(strings) > 0] <- paste0("*", strings[ncharw(strings) > 0], "*")
    align <- real_align(ht)[ dcell$display_row, dcell$display_col ]
    stringr_align <- switch(align, "left" = "right", "right" = "left", "centre" = "both")
    strings <- col_aware_strpad(strings, width, stringr_align)
    dc$strings[[r]] <- strings
  }

  dc$text_height <- sapply(dc$strings, length)
  # we use nchar(type = "c") because otherwise, when characters have
  # screen width > 1, "cols"
  # in the loop below will be too long, leading to the text being repeated:
  dc$text_width <- sapply(dc$strings, function (x) max(ncharw(x, type = "c")))

  #######################################################################
  # calculate row heights: start at 0 and increase it if it"s too little,
  # sharing equally among relevant columns
  dc <- dc[order(dc$rowspan), ]
  heights <- rep(1, nrow(ht))
  for (r in seq_len(nrow(dc))) {
    dcell <- dc[r, ]
    rows <- seq(dcell$display_row, dcell$end_row)
    if (sum(heights[rows]) + inner_border_v * (dcell$rowspan - 1) < dcell$text_height) {
      heights[rows] <- pmax(heights[rows], ceiling(dcell$text_height / dcell$rowspan))
    }
  }

  border_widths <- c(outer_border_h, rep(inner_border_h, ncol(ht) - 1), outer_border_h)
  # width of outer border, then cells + following border:
  all_widths    <- border_widths + c(0, widths)
  # indices into charmat:
  starting_cols <- cumsum(all_widths[seq_len(ncol(ht))]) + 1
  border_cols   <- c(starting_cols, sum(all_widths) + 1) - border_widths

  border_heights <- c(outer_border_v, rep(inner_border_v, nrow(ht) - 1), outer_border_v)
  all_heights    <- border_heights + c(0, heights)
  # indices into charmat:
  starting_rows  <- cumsum(all_heights[seq_len(nrow(ht))]) + 1
  border_rows    <- c(starting_rows, sum(all_heights) + 1) - border_heights

  charmat <- matrix(" ", sum(all_heights), sum(all_widths))
  width_mat <- matrix(0, sum(all_heights), sum(all_widths))

  for (r in seq_len(nrow(dc))) {
    dcell <- dc[r, ]
    string_letters <- unlist(col_aware_strsplit(dcell$strings[[1]], ""))
    drow <- dcell$display_row
    dcol <- dcell$display_col
    style <- if (color) make_cell_style(ht, drow, dcol) else identity
    rows <- seq(starting_rows[drow], starting_rows[drow] + dcell$text_height - 1)
    cols <- seq(starting_cols[dcol], starting_cols[dcol] + dcell$text_width - 1)
    charmat[rows, cols] <- matrix(style(string_letters), length(rows), length(cols), byrow = TRUE)
    width_mat[rows, cols] <- matrix(ncharw(string_letters), length(rows), length(cols), byrow = TRUE)
  }

  list(
          charmat     = charmat,
          width_mat   = width_mat,
          border_rows = border_rows,
          border_cols = border_cols,
          last_ht_col = ncol(ht)
        )
}


col_aware_strsplit <- function (...) {
  if (requireNamespace("crayon", quietly = TRUE)) {
    crayon::col_strsplit(...)
  } else {
    strsplit(...)
  }
}


col_aware_strpad <- function (string, width, side) {
  if (requireNamespace("crayon", quietly = TRUE)) {
    clean <- crayon::strip_style(string)
    padded <- stringr::str_pad(clean, width, side)
    # returns a matrix. First column is whole match. Next columns are captures:
    pads   <- stringr::str_match(padded, "^( *).*?( *)$")
    paste0(pads[, 2], string, pads[, 3])
  } else {
    stringr::str_pad(string, width, side)
  }
}


pad_position <- function (string, position, max_width) {
  width <- min(max_width, getOption("width", 80))
  assert_that(position %in% c("left", "center", "right"))
  if (position == "left") return(string)
  side <- if (position == "center") "both" else "left"
  stringr::str_pad(string, width, side)
}


make_cell_style <- function (ht, row, col) {
  tc         <- text_color(ht)[row, col]
  bgc        <- background_color(ht)[row, col]
  bold       <- bold(ht)[row, col]
  italic     <- italic(ht)[row, col]

  maybe_combine_style <- function (style, style2) {
    if (is.null(style)) style2 else crayon::combine_styles(style, style2)
  }
  style <- NULL
  if (bold) style <- crayon::bold
  if (italic) style <- maybe_combine_style(style, crayon::italic)
  if (! is.na(tc)) style <- maybe_combine_style(style, crayon::make_style(tc))
  if (! is.na(bgc)) style <- maybe_combine_style(style, crayon::make_style(bgc, bg = TRUE))
  style <- style %||% identity

  style
}
