


render_markdown <- function (text, type) {
  crayon_installed <- requireNamespace("crayon", quietly = TRUE)
  switch(type,
    "html"     = vapply(text, commonmark::markdown_html,
                   FUN.VALUE = character(1), extensions = "strikethrough"),
    "latex"    = vapply(text, commonmark::markdown_latex,
                   FUN.VALUE = character(1), extensions = "strikethrough"),
    "markdown" = text,
    "rtf"      = vapply(text,
                   translate_fun(MarkdownRTFTranslator),
                   FUN.VALUE = character(1)
                 ),
    "screen"   = if (! crayon_installed) text else {
                   vapply(text,
                     translate_fun(MarkdownScreenTranslator),
                     FUN.VALUE = character(1)
                   )
                 },
    vapply(text, commonmark::markdown_text, FUN.VALUE = character(1),
          extensions = "strikethrough")
  )
}


translate_fun <- function (translator_class) {
  tr_obj <- translator_class$new()

  tr_obj$translate
}

# plan for screen and RTF
# create a generic Visitor which parses the xml and then
# visits each node, recursively.
# Plug into this lists of functions which take a node and return
# start and end text
# the Visitor collects the text and puts it together
# maybe use S3: dispatch on node types, with a default which does nothing?



MarkdownTranslator <- R6::R6Class("MarkdownTranslator",
  public = list(

    methods = function () {
      names(as.list(self))
    },

    translate = function (text) {
      md_xml <- commonmark::markdown_xml(text, extensions = "strikethrough")
      md_xml <- xml2::read_xml(md_xml)
      md_xml <- xml2::xml_ns_strip(md_xml)

      output <- self$process(md_xml)

      output <- paste(output, collapse = "")
      return(output)
    },

    process = function (node) {
      if (inherits(node, "xml_nodeset")) {
        out <- lapply(node, self$process)
        return(unlist(out))
      }

      # "inheritance" via xml names
      method <- xml2::xml_name(node)
      if (! method %in% self$methods()) {
        method <- "default"
      }

      self[[method]](node)
    },

    process_contents = function (node) {
      contents <- xml2::xml_contents(node)
      self$process(contents)
    },

    default = function (node) {
      self$process_contents(node)
    },

    text = function (node) {
      xml2::xml_text(node)
    }
  )
)


MarkdownScreenTranslator <- R6::R6Class("MarkdownScreenTranslator",
  inherit = MarkdownTranslator,

  public = list(

    list_type = "bullet",

    list_digit = 1,

    paragraph = function (node) {
      c(self$process_contents(node), "\n")
    },

    strong = function (node) {
      crayon::bold(self$process_contents(node))
    },

    emph = function (node) {
      crayon::italic(self$process_contents(node))
    },

    strikethrough = function (node) {
        crayon::strikethrough(self$process_contents(node))
    },

    softbreak = function (node) {
      c(self$process_contents(node), "\n")
    },

    link = function (node) {
      crayon::blue(crayon::underline(self$process_contents(node)))
    },

    image = function (node) {
      crayon::red(c("[", self$process_contents(node), "]"))
    },

    list = function (node) {
      self$list_type <- xml2::xml_attr(node, "type")
      if (self$list_type == "ordered") self$list_digit <- 1
      self$process_contents(node)
    },

    item = function (node) {
      if (self$list_type == "ordered") {
        bullet <- paste0(self$list_digit, ". ")
        self$list_digit <- self$list_digit + 1
      } else {
        bullet <- "* "
      }
      c(bullet, self$process_contents(node))
    }
  )
)


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


process_xml_old <- function (node, rtf) {
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
          "image"         = {add(make_rtf_picture(node)); rtf <- process_xml(ch, rtf)},
          # we're in a table line, so we never add \\par, even for <paragraph>
          {rtf <- process_xml(ch, rtf)}
        )

  rtf
}


make_rtf_link <- function (node) {
  url <- xml2::xml_attr(node, "destination")
  paste0("{\\field{\\*\\fldinst HYPERLINK \"", url, "\"}{\\fldrslt \\ul ")
}


make_rtf_picture <- function (node) {
  url <- xml2::xml_attr(node, "destination")
  paste0("{\\field\\fldedit{\\*\\fldinst { INCLUDEPICTURE  \\\\d \"", url,
         "\" \\* MERGEFORMATINET }}{\\fldrslt {  }}}")
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
