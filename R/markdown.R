


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
    },

    code = function (node) {
      xml2::xml_text(node)
    }
  )
)


MarkdownScreenTranslator <- R6::R6Class("MarkdownScreenTranslator",
  inherit = MarkdownTranslator,

  public = list(

    list_type = "bullet",

    list_delim = ".",

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

    code = function (node) {
      crayon::silver(xml2::xml_text(node))
    },

    softbreak = function (node) {
      c(self$process_contents(node), "\n")
    },

    thematic_break = function (node) {
      rep("\u2500", 5)
    },

    link = function (node) {
      crayon::blue(crayon::underline(self$process_contents(node)))
    },

    image = function (node) {
      crayon::red(c("[", self$process_contents(node), "]"))
    },

    heading = function (node) {
      crayon::bold(self$process_contents(node))
    },

    code_block = function (node) {
      self$process_contents(node)
    },

    block_quote = function (node) {
      result <- self$process_contents(node)
      result <- paste(result, collapse = "")
      result <- crayon::col_strsplit(result, "\n", fixed = TRUE)[[1]]
      result <- paste0(">  ", result, "\n")
      result
    },

    list = function (node) {
      old_list_type   <- self$list_type
      old_list_delim  <- self$list_delim
      old_list_digit  <- self$list_digit

      self$list_type <- xml2::xml_attr(node, "type")
      if (self$list_type == "ordered") {
        self$list_digit <- as.integer(xml2::xml_attr(node, "start"))
        self$list_delim <- if (xml2::xml_attr(node, "delim") == "paren") ")" else "."
      }
      self$process_contents(node)

      self$list_type <- old_list_type
      self$list_delim <- old_list_delim
      self$list_digit <- old_list_digit
    },

    item = function (node) {
      if (self$list_type == "ordered") {
        bullet <- paste0(self$list_digit, self$list_delim, " ")
        self$list_digit <- self$list_digit + 1
      } else {
        bullet <- "* "
      }
      c(bullet, self$process_contents(node))
    }
  )
)


MarkdownRTFTranslator <- R6::R6Class("MarkdownRTFTranslator",
  inherit = MarkdownTranslator,

  public = list(

    list_type = "bullet",

    paragraph = function (node) {
      c("{", self$process_contents(node), "\\par}")
    },

    strong = function (node) {
      c("\\b ", self$process_contents(node), "\\b0 ")
    },

    emph = function (node) {
      c("\\i ", self$process_contents(node), "\\i0 ")
    },

    strikethrough = function (node) {
      c("\\strike ", self$process_contents(node), "\\strike0 ")
    },

    softbreak = function (node) {
      c(self$process_contents(node), "\n")
    },

    link = function (node) {
      url <- xml2::xml_attr(node, "destination")
      open_link <- c("{\\field{\\*\\fldinst HYPERLINK \"", url,
            "\"}{\\fldrslt \\ul ")
      c(open_link, self$process_contents(node), "}}")
    },

    image = function (node) {
      url <- xml2::xml_attr(node, "destination")
      open_image <- c("{\\field\\fldedit{\\*\\fldinst{ INCLUDEPICTURE \"",
            url, "\" \\\\* MERGEFORMATINET \\\\d}}{\\fldrslt { }}}")
      c(open_image, self$process_contents(node))
    },

    list = function (node) {
      old_list_type <- self$list_type
      self$list_type <- xml2::xml_attr(node, "type")
      self$process_contents(node)
      self$list_type <- old_list_type
    },

    item = function (node) {
      open_item <- "{{\\li0\\pntext\\pn"
      list_type_rtf <- if (self$list_type == "ordered") {
        "\\pnlvlbody \\pndec \\pnstart1 \\pntxta{. }}{"
      } else {
        "\\pnlvlblt\\pntxtb{\\u8226? }}{"
      }
      close_item <-  "}}"
      c(open_item, list_type_rtf, self$process_contents(node), close_item)
    }
  )
)


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
