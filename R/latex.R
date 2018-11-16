
# LaTeX printing -----------------------------------------------------------------------------------

#' @import assertthat
NULL


default_table_width_unit <- "\\textwidth"


#' @export
#' @rdname to_latex
print_latex <- function (ht, ...) {
  cat(to_latex(ht, ...))
}


#' Create LaTeX representing a huxtable
#'
#' @param ht A huxtable.
#' @param tabular_only Return only the LaTeX tabular, not the surrounding float.
#' @param ... Arguments to pass to methods.
#'
#' @details
#' If we appear to be in a rmarkdown document with the Pandoc markdown `+raw_attribute` extension
#' available, `to_latex` will return LaTeX surrounded by a "raw attribute code block" (see
#' https://pandoc.org/MANUAL.html#extension-raw_attribute). This helps protect against pandoc
#' accidentally escaping the TeX code.
#'
#' @return `to_latex` returns a string. `print_latex` prints the string and returns `NULL`.
#' @export
#'
#' @family printing functions
#'
#' @examples
#' ht <- huxtable(
#'         a = 1:3,
#'         b = letters[1:3]
#'       )
#' print_latex(ht)
to_latex <- function (ht, ...) UseMethod("to_latex")


#' @export
#' @rdname to_latex
to_latex.huxtable <- function (ht, tabular_only = FALSE, ...){
  assert_that(is.flag(tabular_only))
  tabular <- build_tabular(ht)
  commands <- "
  \\providecommand{\\huxb}[2]{\\arrayrulecolor[RGB]{#1}\\global\\arrayrulewidth=#2pt}
  \\providecommand{\\huxvb}[2]{\\color[RGB]{#1}\\vrule width #2pt}
  \\providecommand{\\huxtpad}[1]{\\rule{0pt}{\\baselineskip+#1}}
  \\providecommand{\\huxbpad}[1]{\\rule[-#1]{0pt}{#1}}\n"

  if (tabular_only) return(maybe_markdown_fence(paste0(commands, tabular)))

  resize_box <- if (is.na(height <- height(ht))) c("", "") else {
    if (is.numeric(height)) height <- sprintf("%.3g\\textheight", height)
    c(sprintf("\\resizebox*{!}{%s}{", height), "}")
  }

  cap <- if (is.na(cap <- make_caption(ht, "latex"))) "" else {
    hpos <- get_caption_hpos(ht)
    cap_setup <- switch(hpos,
      left   = "raggedright",
      center = "centering",
      right  = "raggedleft"
    )
    sprintf("\\captionsetup{justification=%s,singlelinecheck=off}\n\\caption{%s}\n", cap_setup, cap)
  }
  lab <- if (is.na(lab <- label(ht))) "" else sprintf("\\label{%s}\n", lab)
  if (nzchar(lab) && ! nzchar(cap)) warning("No caption set: LaTeX table labels may not work as expected.")

  pos_text <- switch(position(ht),
    left   = c("\\begin{raggedright}", "\\par\\end{raggedright}\n"),
    center = c("\\centering",   "\n"),
    right  = c("\\begin{raggedleft}",  "\\par\\end{raggedleft}\n")
  )

  res <- if (grepl("top", caption_pos(ht))) paste0(cap, lab, tabular) else paste0(tabular, cap, lab)
  res <- paste0(
          commands,
          sprintf("\n\\begin{table}[%s]\n",latex_float(ht)),
          pos_text[1],
          resize_box[1],
          "\n\\begin{threeparttable}\n",
          res,
          "\\end{threeparttable}\n",
          resize_box[2],
          pos_text[2],
          "\n\\end{table}\n"
        )

  return(maybe_markdown_fence(res))
}


build_tabular <- function(ht) {
  if (! check_positive_dims(ht)) return("")

  ## PREPARE EMPTY PARTS -------
  multirow <- multicol <- bg_color <- inner_cell <- contents <- matrix("", nrow(ht), ncol(ht))

  # Precalculate align, collapsed borders, and width.
  real_align <- real_align(ht)
  display_cells <- display_cells(ht, all = TRUE)
  # Could speed this up by doing it only for display cells.
  start_end_cols <- as.matrix(display_cells[, c("display_col", "end_col")])
  width_spec <- apply(start_end_cols, 1, function (x) compute_width(ht, x[1], x[2]))
  cb <- collapsed_borders(ht)
  cbc <- collapsed_border_colors(ht)
  cbs <- collapsed_border_styles(ht)

  ## PREPARE INDICES -----------
  dc_pos_matrix <- as.matrix(display_cells[, c("display_row", "display_col")])
  dc_map <- matrix(1:length(contents), nrow(ht), ncol(ht))
  # dc_map gives the display cells corresponding to real cells, in as.vector(cell_contents) space
  dc_map <- c(dc_map[dc_pos_matrix])
  dc_idx       <- ! display_cells$shadowed
  left_idx     <- display_cells$col == display_cells$display_col
  right_idx    <- display_cells$col == display_cells$end_col
  bottom_idx   <- display_cells$row == display_cells$end_row
  multirow_idx <- display_cells$rowspan > 1
  bl_idx       <-  bottom_idx & left_idx
  blm_idx      <- bl_idx & multirow_idx
  # lh_dc (bl_dc) gives the display cells corresponding to (bottom) left cells
  bl_dc <- dc_map[bl_idx]
  lh_dc <- dc_map[left_idx]

  ## CALCULATE HHLINES ---------
  #  Done for n+1 rows including "row 0" at the top
  horiz_b <- cb$horiz
  hb_maxes <- apply(horiz_b, 1, max)
  if (any(horiz_b > 0 & horiz_b < hb_maxes[row(horiz_b)])) warning(
        "Multiple horizontal border widths in a single row; using the maximum.")
  # even if a cell"s own border is 0, it still needs a "border" the right width,
  # matching its background color.
  # decide now if border will be printed in foreground or background color:
  has_own_border <- horiz_b > 0
  # set border widths to the row maximum:
  horiz_b[] <- hb_maxes[row(horiz_b)]
  hb_default <- is.na(cbc$horiz)
  hb_colors <- format_color(cbc$horiz, default = "black")
  hb_chars <- ifelse(cbs$horiz == "double", "=", "-")

  # background colors come from shadowing cells
  bg_colors <- background_color(ht)[dc_map]
  dim(bg_colors) <- dim(ht)
  # add a top row for the first hhline
  bg_colors <- rbind(rep(NA, ncol(horiz_b)), bg_colors) # or, should color be taken from cells below?
  bg_colors <- format_color(bg_colors, default = "white")
  hhline_colors <- bg_colors
  hhline_colors[has_own_border] <- hb_colors[has_own_border]
  hhlines_horiz <- sprintf(">{\\huxb{%s}{%.4g}}%s", hhline_colors, horiz_b, hb_chars)
  dim(hhlines_horiz) <- dim(horiz_b)
  no_hborder_in_row <- hb_maxes[row(hhlines_horiz)] == 0
  hhlines_horiz[no_hborder_in_row] <- ""

  # vertical borders in hhlines are an n+1 x n+1 matrix of "corner dots"
  vert_b <- cb$vert # nrow X ncol + 1
  # we add an extra row to match the nrow+1 hhlines
  vert_b <- rbind(vert_b[1, ], vert_b) # we checked positive dims; row 1 exists
  vert_bs <- rbind(cbs$vert[1, ], cbs$vert)
  # vertical dots should have colour from, in order of preference:
  # (a) left horiz border; (b) right horiz border; (c) lower vert border; (d) upper vert border
  # We use lower borders first on the theory that the "top of a square" matters more
  # even if a colour is undefined, we treat it as black so long as there is a positive-length border
  vert_bc <- cbind(NA, cbc$horiz) # horiz border color on left
  no_left_hb <- cbind(0, cb$horiz) == 0
  no_lr_hb <- no_left_hb & cbind(cb$horiz, 0) == 0
  no_lrb_b <- no_lr_hb & rbind(cb$vert, 0) == 0
  vert_bc[no_left_hb] <- cbind(cbc$horiz, NA)[no_left_hb]  # horiz border color on right
  vert_bc[no_lr_hb] <- rbind(cbc$vert, NA)[no_lr_hb] # vert border color below
  vert_bc[no_lrb_b] <- rbind(NA, cbc$vert)[no_lrb_b] # vert border color above
  # if it's still NA, no border on any side had a defined colour
  # At the moment we reset to black. Otherwise maybe we "bleed" along the row from much earlier.

  vert_bc <- format_color(vert_bc, default = "black")
  hhlines_vert <- rep("", length(vert_b))
  has_vert_b <- vert_b > 0

  # here we want the real borders, not the row maxes of `horiz_b`:
  has_horiz_b <- cbind(cb$horiz[, 1], cb$horiz) > 0

  vert_bchars <- rep("", length(vert_bc))
  # Put in | when you have a single meets no border;
  # Put in || where a double meets no border;
  # Otherwise, leave them blank
  # PROBLEM: if you have no border on L, you don't get a horizontal line (of the right colour)
  vert_bchars[! vert_bs == "double" & ! has_horiz_b]  <- "|"
  vert_bchars[vert_bs == "double" & ! has_horiz_b]    <- "||"

  hhlines_vert[has_vert_b] <- sprintf(">{\\huxb{%s}{%.4g}}%s",
        vert_bc[has_vert_b],
        vert_b[has_vert_b],
        vert_bchars[has_vert_b])
  hhlines_vert[vert_bchars == ""] <- ""
  dim(hhlines_vert) <- c(nrow(horiz_b), ncol(horiz_b) + 1) # n+1 x n+1

  # interleave vertical and horizontal lines like: |-|-|-|
  hhlines <- matrix("", nrow(hhlines_horiz), ncol(hhlines_horiz) + ncol(hhlines_vert))
  hhlines[, seq(2, ncol(hhlines), 2)] <- hhlines_horiz
  hhlines[, seq(1, ncol(hhlines), 2)] <- hhlines_vert

  hhlines <- apply(hhlines, 1, paste0, collapse = "")
  hhlines <- sprintf("\n\n\\hhline{%s}\n\\arrayrulecolor{black}\n", hhlines)

  ## CELL CONTENTS -------------
  ## inner_cell is empty except for the *bottom* left of a 'display area' (including 1x1)
  ## this avoids a problem with later cells overpainting borders etc.
  ## - inner_cell has padding, alignment, wrap and row_height TeX added
  ## inner_cell data comes from the 'display cell' at the top left of the display area

  inner_cell_bldc <- clean_contents(ht, type = "latex")[bl_dc]
  fs_bldc <- font_size(ht)[bl_dc]
  line_space_bldc <- round(fs_bldc * 1.2, 2)
  has_fs_bldc <- ! is.na(fs_bldc)
  inner_cell_bldc[has_fs_bldc] <- sprintf("{\\fontsize{%.4gpt}{%.4gpt}\\selectfont %s}",
        fs_bldc[has_fs_bldc], line_space_bldc[has_fs_bldc], inner_cell_bldc[has_fs_bldc])

  tc_bldc <- text_color(ht)[bl_dc]
  tcf_bldc <- format_color(tc_bldc)
  has_tc_bldc <- ! is.na(tc_bldc)
  inner_cell_bldc[has_tc_bldc] <- sprintf("\\textcolor[RGB]{%s}{%s}", tcf_bldc[has_tc_bldc],
        inner_cell_bldc[has_tc_bldc])

  bold_bldc <- bold(ht)[bl_dc]
  italic_bldc <- italic(ht)[bl_dc]
  inner_cell_bldc[bold_bldc]   <- sprintf("\\textbf{%s}", inner_cell_bldc[bold_bldc])
  inner_cell_bldc[italic_bldc] <- sprintf("\\textit{%s}", inner_cell_bldc[italic_bldc])

  font_bldc <- font(ht)[bl_dc]
  has_font_bldc <- ! is.na(font_bldc)
  inner_cell_bldc[has_font_bldc] <- sprintf("{\\fontfamily{%s}\\selectfont %s}",
        font_bldc[has_font_bldc],
        inner_cell_bldc[has_font_bldc])

  rt_bldc <- rotation(ht)[bl_dc]
  has_rt_bldc <- rt_bldc != 0
  inner_cell_bldc[has_rt_bldc] <- sprintf("\\rotatebox{%.4g}{%s}", rt_bldc[has_rt_bldc],
        inner_cell_bldc[has_rt_bldc])

  pad_bldc <- list()
  pad_bldc$left   <- left_padding(ht)[bl_dc]
  pad_bldc$right  <- right_padding(ht)[bl_dc]
  pad_bldc$top    <- top_padding(ht)[bl_dc]
  pad_bldc$bottom <- bottom_padding(ht)[bl_dc]
  align_bldc      <- real_align[bl_dc]
  valign_bldc     <- valign(ht)[bl_dc]
  wrap_bldc       <- wrap(ht)[bl_dc]

  has_pad_bldc <- lapply(pad_bldc, Negate(is.na))
  pad_bldc <- lapply(pad_bldc, function (x) if (is.numeric(x)) sprintf("%.4gpt", x) else x)
  tpad_tex_bldc <- rep("", length(pad_bldc$top))
  # tpad_tex_bldc[has_pad_bldc$top] <- sprintf("\\rule{0pt}{\\baselineskip+%s}",
  #       pad_bldc$top[has_pad_bldc$top])
  tpad_tex_bldc[has_pad_bldc$top] <- sprintf("\\huxtpad{%s}", pad_bldc$top[has_pad_bldc$top])
  bpad_tex_bldc <- rep("", length(pad_bldc$bottom))
  bpad_vals_bldc <- pad_bldc$bottom[has_pad_bldc$bottom]
  bpad_tex_bldc[has_pad_bldc$bottom] <- sprintf("\\huxbpad{%s}", bpad_vals_bldc)
  align_tex_key <- c("left" = "\\raggedright ", "right" = "\\raggedleft ", "center" = "\\centering ")
  align_tex_bldc <- align_tex_key[align_bldc]
  inner_cell_bldc <- paste0(tpad_tex_bldc, align_tex_bldc, inner_cell_bldc, bpad_tex_bldc)

  if (any(wrap_bldc)) {
    # reverse of what you think. "b" aligns the *bottom* of the text with the baseline
    # ... this doesn"t really work for short text!
    valign_tex_key <- c("top" = "b", "middle" = "c", "bottom" = "t")
    valign_bldc <- valign_tex_key[valign_bldc]
    # XXX should be a way to speed up by only doing dc_idx cells. but we run again at some point...
    width_spec_bldc <- width_spec[bl_dc]
    hpad_loss_left_bldc  <- ifelse(has_pad_bldc$left,  paste0("-", pad_bldc$left),  "")
    hpad_loss_right_bldc <- ifelse(has_pad_bldc$right, paste0("-", pad_bldc$right), "")
    inner_cell_bldc[wrap_bldc] <- sprintf("\\parbox[%s]{%s%s%s}{%s}",
            valign_bldc[wrap_bldc],
            width_spec_bldc[wrap_bldc],
            hpad_loss_left_bldc[wrap_bldc],
            hpad_loss_right_bldc[wrap_bldc],
            inner_cell_bldc[wrap_bldc]
          )
  }

  ## ROW HEIGHT--------

  row_height <- row_height(ht)
  # for each display_cell, we "sum" row_heights for each row it covers
  row_height_tex_bldc <- if (all(is.na(row_height))) {
    rep("", sum(dc_idx))
  } else {
    # bl_to_dc for matrices:
    start_end_rows_bldc <- display_cells[dc_map, c("display_row", "end_row")][bl_idx, ]
    row_seqs_bldc <- apply(start_end_rows_bldc, 1, function (x) seq(x[1], x[2]))
    rh_bldc <- sapply(row_seqs_bldc, function (x) {
      rh <- row_height[x]
      # use the LaTeX calc package to sum non-numeric row heights within LaTeX:
      if (is.numeric(rh)) sprintf("%.4g\\textheight", sum(rh)) else paste(rh, collapse = "+")
    })
    sprintf("\\rule{0pt}{%s}", rh_bldc)
  }
  inner_cell_bldc <- paste0(inner_cell_bldc, row_height_tex_bldc)
  # assign all this to bottom left cells:
  inner_cell[bl_idx] <- inner_cell_bldc

  ## CELL BACKGROUND COLORS -------
  ## cell background colors are added to left hand row of a "display area"; these come
  ## from the colors of the "display cell".

  bg_color_lhdc <- background_color(ht)[lh_dc]
  has_bg_color_lhdc <- ! is.na(bg_color_lhdc)
  bg_color_lhdc <- format_color(bg_color_lhdc)
  bg_color_lhdc <- sprintf("\\cellcolor[RGB]{%s}", bg_color_lhdc)
  bg_color_lhdc[! has_bg_color_lhdc] <- "" # NAs don't have a \cellcolor instruction
  bg_color[left_idx] <- bg_color_lhdc


  ## VERTICAL BORDERS AND COLSPEC -------
  ## like cell colors, these are provided for all left hand cells, from display cells
  ## if display cell wrap, is TRUE, set colspec to p/m/b with widthspec
  ## otherwise, set colspec to l/c/r
  ## left borders are blank, except for the first row; we collapse borders into right border position

  colspan_lhdc    <- colspan(ht)[lh_dc]
  wrap_lhdc       <- wrap(ht)[lh_dc]
  valign_lhdc     <- valign(ht)[lh_dc]
  real_align_lhdc <- real_align[lh_dc]
  colspec_tex_key <- c("left" = "l", "center" = "c", "right" = "r")
  real_align_lhdc <- colspec_tex_key[real_align_lhdc]
  colspec_lhdc <- real_align_lhdc # only for non-wrapped cells though
  # as this is calculated for the whole display area, it doesn't matter whether
  # you map to display cells or not:
  width_spec_lhdc <- width_spec[lh_dc]
  colspec_lhdc[wrap_lhdc]   <- {
    pmb <- valign_lhdc[wrap_lhdc]
    pmb_tex_key <- c("top"   = "p", "bottom"  = "b", "middle" = "m")
    pmb <- pmb_tex_key[pmb]
    sprintf("%s{%s}", pmb, width_spec_lhdc[wrap_lhdc])
  }

  # do left and right borders...
  # these are nrow x ncol + 1
  bord <- cb$vert
  bcol <- cbc$vert
  has_bord <- ! is.na(bord)
  has_bcol <- ! is.na(bcol) # if *defined* as black, then we print it. Otherwise not.
  bs_double <- cbs$vert == "double"
  bcol <- format_color(bcol, default = "black")
  bord_tex <- rep("", length(bord))

  # bord_tex[has_bord] <- sprintf("!{%s\\vrule width %.4gpt}", bcol_tex[has_bord], bord[has_bord])
  bord_tex[has_bord] <- sprintf("!{\\huxvb{%s}{%.4g}}", bcol[has_bord], bord[has_bord])
  bord_tex[bs_double] <- paste0(bord_tex[bs_double], bord_tex[bs_double])
  dim(bord_tex) <- dim(cb$vert)
  # the first column is the left border of the left-most cell.
  # subsequent columns become the right border of all cells.
  lborders <- matrix("", nrow(contents), ncol(contents))
  lborders[, 1] <- bord_tex[, 1]
  rborders <- bord_tex[, - 1]
  # lborders and rborders are already in 'correct' positions, as calculated by collapsed_borders
  # we need to have only the rborders that correspond to a display area's right hand border;
  # these should go in the left hand cell position with the other stuff!

  # all left hand cells have borders
  multicol[left_idx] <- sprintf("\\multicolumn{%d}{%s%s%s}{",
          colspan_lhdc,
          lborders[left_idx],
          colspec_lhdc,
          rborders[right_idx]
        )

  ## MULTIROW ---------------------
  rowspan_blm <- rowspan(ht)[dc_map][blm_idx]
  vert_adj_blm <- sprintf("%dex", 0) # start printing on the top row
  # * is "standard width", could be more specific?
  multirow_blm_tex <- sprintf("\\multirow{-%s}{*}[%s]{", rowspan_blm, vert_adj_blm)
  multirow[blm_idx] <- multirow_blm_tex

  ## FINAL ASSEMBLY----------------
  closer <- function (x) ifelse(nchar(x) > 0, "}", "")

  contents <- paste0(
          multicol,
          multirow,
          bg_color,
          inner_cell,
          closer(multirow),
          closer(multicol)
        )
  dim(contents) <- dim(ht)

  content_rows <- apply(contents, 1, function (x) {
    x <- x[nchar(x) > 0]
    row <- paste(x, collapse = " &\n")
    paste(row, "\\tabularnewline[-0.5pt]")
  })

  table_body <- paste(content_rows, hhlines[-1], sep = "\n", collapse = "\n")
  table_body <- paste(hhlines[1], table_body, sep = "\n")

  tenv <- tabular_environment(ht)
  tenv_tex <- paste0(c("\\begin{", "\\end{"), tenv, "}")
  width_spec <- if (tenv %in% c("tabularx", "tabular*", "tabulary")) {
    tw <- width(ht)
    if (is.numeric(tw)) tw <- paste0(tw, default_table_width_unit)
    paste0("{", tw, "}")
  } else {
    ""
  }

  colspec_top <- sapply(seq_len(ncol(ht)), function (mycol) {
          sprintf("p{%s}", compute_width(ht, mycol, mycol))
        })
  colspec_top <- paste0(colspec_top, collapse = " ")
  colspec_top <- sprintf("{%s}\n", colspec_top)

  res <- paste0(tenv_tex[1], width_spec, colspec_top, table_body, tenv_tex[2])
  return(res)
}


compute_width <- function (ht, start_col, end_col) {
  table_width <- width(ht) # always defined, default is 0.5 (of \\textwidth)
  if (is.numeric(table_width)) {
    table_unit  <- default_table_width_unit
    table_width <- as.numeric(table_width)
  } else {
    table_unit  <- gsub("\\d", "", table_width)
    table_width <- as.numeric(gsub("\\D", "", table_width))
  }

  cw <- col_width(ht)[start_col:end_col]
  cw[is.na(cw)] <- 1 / ncol(ht)
  cw <- if (! is.numeric(cw)) {
    paste(cw, collapse = "+")
  } else {
    cw <- sum(as.numeric(cw))
    cw <- cw * table_width
    paste0(cw, table_unit)
  }

  if (end_col > start_col) {
    # need to add some extra tabcolseps, two per column
    extra_seps <- (end_col - start_col) * 2
    cw <- paste0(cw, "+", extra_seps, "\\tabcolsep")
  }

  cw
}

maybe_markdown_fence <- function (text) {
  fence <- FALSE

  if (requireNamespace("knitr", quietly = TRUE)) {
    in_rmarkdown <- ! is.null(knitr::opts_knit$get("rmarkdown.pandoc.to"))
    if (in_rmarkdown && requireNamespace("rmarkdown", quietly = TRUE)) {
      fence <- rmarkdown::pandoc_version() >= "2.0.0"
    }
  }

  if (fence) {
    text <- paste("\n\n```{=latex}\n", text, "\n```\n\n")
  }

  return(text)

}
