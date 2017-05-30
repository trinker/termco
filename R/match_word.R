#' Regular Expression Matching of Words in a Text
#'
#' Extract the words from a text that match a regular expression.
#'
#' @param text.var The text string variable.
#' @param term.list A list of named character vectors.
#' @param \ldots ignored.
#' @return Returns a list of matched word vectors.
#' @export
#' @examples
#' match_word(
#'     text.var = c(
#'         'the dog is todo them funny',
#'         'I\'d wait to eat the sandwich.',
#'         'the dog ate the sandwiches'
#'     ),
#'     term.list = c('the', 'sandwich', 'do', '^do', '\\bdo', 'do$')
#' )
#'
#' match_word(
#'     text.var = c(
#'         'the dog is todo them funny',
#'         'I\'d wait to eat the sandwich.',
#'         'the dog ate the sandwiches'
#'     ),
#'     term.list = list(
#'         'the',
#'         'sandwich',
#'         'do',
#'         '^do',
#'         '\\bdo',
#'         'do$',
#'         '\\b(eat|ate)\\b'
#'     )
#' )
match_word <- function(text.var, term.list, ...) {

    terms <- unlist(lapply(term.list, function(m) paste(paste0("(", m, ")"), collapse = "|")))
    x <- sort(unique(as.character(quanteda::tokens(tolower(text.var), remove_punct = TRUE))))
    out <- lapply(terms, function(y) x[stringi::stri_detect_regex(x, y)])
    names(out) <- terms
    out
}


