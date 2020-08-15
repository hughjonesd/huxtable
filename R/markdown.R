


render_markdown <- function (text, type) {
  switch(type,
    "html"     = vapply(text, commonmark::markdown_html,
                   FUN.VALUE = character(1), extensions = "strikethrough"),
    "latex"    = vapply(text, commonmark::markdown_latex,
                   FUN.VALUE = character(1), extensions = "strikethrough"),
    "rtf"      = vapply(text, markdown_rtf, FUN.VALUE = character(1)),
    "markdown" = text,
    "screen"   = markdown_screen(text),
    vapply(text, commonmark::markdown_text, FUN.VALUE = character(1),
          extensions = "strikethrough")
  )
}


markdown_screen <- function (text) {
  if (! requireNamespace("crayon", quietly = TRUE)) return(text)

  my_italic <- function (x) {
    x <- sub("^\\*(.*)\\*$", "\\1", x)
    x <- sub("^_(.*)_$", "\\1", x)
    crayon::italic(x)
  }
  my_bold <- function (x) {
    x <- sub("^\\*\\*(.*)\\*\\*$", "\\1", x)
    x <- sub("^__(.*)__$", "\\1", x)
    crayon::bold(x)
  }
  my_strikethrough <-  function (x) {
    x <- sub("^~(.*)~$", "\\1", x)
    crayon::strikethrough(x)
  }

  res <- text
  res <- stringr::str_replace_all(res, "\\*\\*(.*?)\\*\\*", my_bold)
  res <- stringr::str_replace_all(res, "__(.*?)__", my_bold)
  res <- stringr::str_replace_all(res, "\\*(.*?)\\*", my_italic)
  res <- stringr::str_replace_all(res, "_(.*?)_", my_italic)
  res <- stringr::str_replace_all(res, "~(.*?)~", my_strikethrough)

  res
}


# text is a string
markdown_rtf <- function (text) {
  md_xml <- commonmark::markdown_xml(text, extensions = "strikethrough")
  md_xml <- xml2::read_xml(md_xml)
  md_xml <- xml2::xml_ns_strip(md_xml)
  # result contains "paragraphs" which may contain:
  #  text nodes;
  # <strikethrough>, <strong>,  <emph>, containing text nodes and ending </strikethrough> etc.
  # <softbreak /> for lines;
  # also various list items etc. which we can maybe ignore...!
  # paragraphs are anything separated by two newlines
  #
  # plan: find each paragraph, then recurse through nodes.
  # * strikethrough: add "\\b" then recurse then "\\b0"
  # * strikethrough or /strikethrough, print:
  # * emph or /emph:
  # * strong or /strong, print:
  # * text, print the text
  # Maybe do bullet points?
  rtf_out <- ""

  for (para in xml2::xml_find_all(md_xml, "//paragraph")) {
    rtf_out <- process_xml(para, rtf_out)
  }

  return(rtf_out)
}


process_xml <- function (node, rtf) UseMethod("process_xml")


#' @export
process_xml.xml_nodeset <- function (node, rtf) {
  for (n in node) {
    rtf <- process_xml(n, rtf)
  }

  rtf
}


#' @export
process_xml.xml_node <- function (node, rtf) {
  force(rtf)
  add <- function (...) rtf <<- paste0(rtf, ...)
  ch <- xml2::xml_contents(node)
  switch(xml2::xml_name(node),
          "strong"        = {add("\\b "); rtf <- process_xml(ch, rtf); add("\\b0 ")},
          "emph"          = {add("\\i "); rtf <- process_xml(ch, rtf); add("\\i0 ")},
          "strikethrough" = {add("\\strike "); rtf <- process_xml(ch, rtf); add("\\strike0 ")},
          "text"          = add(as.character(ch)),
          "softbreak"     = add("\n"),
          "link"          = {add(make_rtf_link(node)); rtf <- process_xml(ch, rtf); add("}}")},
          # we're in a table line, so we never add \\par, even for <paragraph>
          {rtf <- process_xml(ch, rtf)}
        )

  rtf
}


make_rtf_link <- function (node) {
  url <- xml2::xml_attr(node, "destination")
  paste0("{\\field{\\*\\fldinst HYPERLINK \"", url, "\"}{\\fldrslt \\ul ")
}


#' Set cell contents to markdown
#'
#' This convenience function calls [set_contents()] and [set_markdown()].
#'
#' @inherit set_contents params
#' @param value Cell contents, as a markdown string.
#'
#' @return The modified huxtable.
#'
#' @export
#'
#' @seealso [markdown()].
#'
#' @examples
#' set_markdown_contents(jams, 1, 1,
#'       "**Type** of jam")
set_markdown_contents <- function (ht, row, col, value) {
  call <- match.call()
  call[["ht"]] <- quote(ht)
  call[[1]] <- as.symbol("set_contents")
  ht <- eval(call, list(ht = ht), parent.frame())

  call <- match.call()
  call[["ht"]] <- quote(ht)
  if (missing(value)) call[["row"]] <- TRUE else call[["value"]] <- TRUE
  call[[1]] <- as.symbol("set_markdown")
  ht <- eval(call, list(ht = ht), parent.frame())

  ht
}
