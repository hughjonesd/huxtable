#' Prepare cell contents for output
#'
#' Converts a huxtable to a character matrix with numeric formatting,
#' markdown rendering and other cleanups applied.
#'
#' @param ht A huxtable.
#' @param output_type Output format, e.g. "latex" or "html".
#' @param ... Unused.
#' @return A character matrix of processed cell contents.
#' @noRd
clean_contents <- function(
    ht,
    output_type = c("latex", "html", "screen", "markdown", "word", "excel", "rtf", "typst"),
    ...) {
  output_type <- match.arg(output_type)
  contents <- as.matrix(as.data.frame(ht))

  contents <- format_numbers_matrix(contents, ht)
  contents[is.na(contents)] <- na_string(ht)

  contents <- render_markdown_matrix(contents, ht, output_type)

  contents <- align_decimals_matrix(contents, ht, output_type)

  # == lengthen minus signs ==
  if (getOption("huxtable.long_minus", FALSE) &&
    !aligning_with_siunitx(output_type)) {
    long_minus <- switch(output_type,
      latex = "$-$",
      excel = "-",
      "\u2212"
    )
    lengthen_minus <- function(numeral) {
      numeral <- sub("^-", long_minus, numeral)
      numeral <- sub("([eE])-", paste0("\\1", long_minus), numeral)
      numeral
    }
    contents[] <- stringr::str_replace_all(contents, number_regex(), lengthen_minus)
  }

  if (output_type == "rtf") {
    contents <- utf8_to_rtf(contents)
  }

  contents
}

#' Format numbers in a character matrix using each cell's number_format
#'
#' @noRd
format_numbers_matrix <- function(contents, ht) {
  if (any(dim(contents) == 0)) {
    return(contents)
  }

  dims <- dimnames(contents)
  nf <- number_format(ht)
  na_str <- na_string(ht)

  res <- vapply(seq_len(ncol(contents)), function(col) {
    vapply(seq_len(nrow(contents)), function(row) {
      cell <- contents[row, col]
      cell <- format_numbers(cell, nf[[row, col]])
      if (is.na(cell)) cell <- na_str[row, col]
      as.character(cell)
    }, character(1))
  }, character(nrow(contents)))

  res <- matrix(res, nrow = nrow(contents))
  dimnames(res) <- dims
  res
}

#' Render markdown and sanitize a character matrix
#'
#' @noRd
render_markdown_matrix <- function(contents, ht, output_type) {
  if (any(dim(contents) == 0)) {
    return(contents)
  }

  dims <- dimnames(contents)
  md <- markdown(ht)
  esc <- escape_contents(ht)

  res <- vapply(seq_len(ncol(contents)), function(col) {
    column <- contents[, col]
    md_rows <- md[, col]
    column[md_rows] <- render_markdown(column[md_rows], output_type)
    if (output_type %in% c("latex", "html", "rtf", "typst")) {
      to_esc <- esc[, col] & !md_rows
      column[to_esc] <- if (output_type == "typst") {
        typst_escape(column[to_esc])
      } else {
        sanitize(column[to_esc], output_type)
      }
    }
    column
  }, character(nrow(contents)))

  res <- matrix(res, nrow = nrow(contents))
  dimnames(res) <- dims
  res
}

#' Align decimals in a character matrix
#'
#' @noRd
align_decimals_matrix <- function(contents, ht, output_type) {
  if (any(dim(contents) == 0)) {
    return(contents)
  }

  dims <- dimnames(contents)
  al <- align(ht)

  res <- vapply(seq_len(ncol(contents)), function(col) {
    pad_chars <- rep(NA_character_, nrow(contents))
    align_pad <- ncharw(al[, col]) == 1
    pad_chars[align_pad] <- al[align_pad, col]
    handle_decimal_alignment(contents[, col], pad_chars, output_type)
  }, character(nrow(contents)))

  res <- matrix(res, nrow = nrow(contents))
  dimnames(res) <- dims
  res
}


#' Convert UTF-8 strings to RTF escapes
#'
#' @param mx A character matrix.
#' @return The matrix with non-ASCII characters replaced by RTF escapes.
#' @noRd
utf8_to_rtf <- function(mx) {
  utf8_codes <- function(x) utf8ToInt(enc2utf8(x))

  rtf_encode <- function(x) {
    code <- utf8_codes(x)
    x <- strsplit(x, split = "")[[1]]
    x[code > 127L & code <= 32767L] <- code[code > 127L & code <= 32767L]
    x[code > 32767L] <- code[code > 32767L] - 65536L
    x[code > 127L] <- paste0("\\u", x[code > 127L], "?")
    paste0(x, collapse = "")
  }

  needs_conv <- vapply(
    c(mx), function(x) any(utf8_codes(x) > 127L),
    logical(1)
  )
  mx[needs_conv] <- vapply(
    mx[needs_conv], rtf_encode,
    character(1)
  )

  mx
}


#' Create a numeral formatting function
#'
#' Returns a function used to format numbers according to `x`.
#'
#' @param x A number, character string or function describing the format.
#' @return A function that formats numeric input.
#' @noRd
numeral_formatter <- function(x) {
  UseMethod("numeral_formatter")
}


#' @export
numeral_formatter.default <- function(x) {
  stop("Unrecognized number_format. Please use a number, string or function.")
}


# If we are a function then return output from the function
#' @export
numeral_formatter.function <- function(x) {
  return(x)
}


#' @export
numeral_formatter.character <- function(x) {
  return(function(numeral) sprintf(x, numeral))
}


#' @export
numeral_formatter.numeric <- function(x) {
  return(function(numeral) formatC(round(numeral, x), format = "f", digits = x))
}


# Breakdown of
# (                     begin capturing group
# -?                    optional minus sign
#' Regular expression for numeric substrings
#'
#' Matches numbers optionally containing a sign, decimal part or exponent.
#'
#' @return A regular expression string.
#' @noRd
number_regex <- function() {
  paste0("(-?[0-9]*\\.?[0-9]+([eE][+-]?[0-9]+)?)")
}


# find each numeric substring, and replace it:
#' Format numeric substrings within text
#'
#' @param string A character string.
#' @param num_fmt A formatting specification or function.
#' @return The string with numeric portions formatted.
#' @noRd
format_numbers <- function(string, num_fmt) {
  if (is.na(string)) {
    return(NA_character_)
  }

  # ! is.function avoids a warning if num_fmt is a function:
  if (!is.function(num_fmt) && is.na(num_fmt)) {
    return(string)
  }

  format_numeral <- function(str) {
    num <- as.numeric(str)
    result <- numeral_formatter(num_fmt)(num)
    result
  }

  stringr::str_replace_all(string, number_regex(), format_numeral)
}


#' Align decimal points in a vector of strings
#'
#' @param col Character vector of numbers.
#' @param pad_chars Characters used to pad each element.
#' @param output_type Output format string.
#' @return A character vector with aligned decimals.
#' @noRd
handle_decimal_alignment <- function(col, pad_chars, output_type) {
  # where pad_chars is NA we do not pad
  orig_col <- col
  na_pad <- is.na(pad_chars)
  col <- col[!na_pad]
  pad_chars <- pad_chars[!na_pad]
  if (length(col) == 0) {
    return(orig_col)
  }

  col <- if (aligning_with_siunitx(output_type)) {
    add_tablenum(col, pad_chars)
  } else {
    pad_spaces(col, pad_chars, output_type)
  }

  orig_col[!na_pad] <- col
  orig_col
}


#' Pad strings with spaces for alignment
#'
#' @param col Character vector.
#' @param pad_chars Characters indicating where to align.
#' @param output_type Output format string.
#' @return A character vector with added padding.
#' @noRd
pad_spaces <- function(col, pad_chars, output_type) {
  find_pos <- function(string, char) {
    regex <- gregexpr(char, string, fixed = TRUE)[[1]]
    regex[length(regex)]
  }

  pos <- mapply(find_pos, col, pad_chars)
  nchars <- ncharw(col)
  # take the biggest distance from the decimal point
  pos[pos == -1L] <- nchars[pos == -1L] + 1
  chars_after_. <- nchars - pos

  pad_n_spaces <- max(chars_after_.) - chars_after_.
  pad_char <- switch(output_type,
    "html"   = "&nbsp;",
    "latex"  = "\\hphantom{0}",
    "screen" = "\u00a0", # screen non-breaking space
    "rtf"    = "\\~",
    " "
  )
  col <- paste0(col, str_rep(pad_char, pad_n_spaces))

  col
}


#' Insert `\tablenum` commands for siunitx alignment
#'
#' @param col Character vector of numbers.
#' @param pad_chars Decimal markers for each element.
#' @return Modified character vector with `\tablenum` markers.
#' @noRd
add_tablenum <- function(col, pad_chars) {
  tn_options <- rep("", length(pad_chars))
  non_dot <- pad_chars != "."
  tn_options[non_dot] <- sprintf("[output-decimal-marker = {%s}]", pad_chars[non_dot])

  replacements <- paste0("\\\\tablenum", tn_options, "{\\1}")
  stringr::str_replace_all(col, number_regex(), replacements)
}


#' Determine if alignment uses siunitx
#'
#' @param output_type Output format string.
#' @return Logical scalar.
#' @noRd
aligning_with_siunitx <- function(output_type) {
  output_type == "latex" && getOption("huxtable.latex_siunitx_align", FALSE)
}
