#' Coerce to Named List
#'
#' Convenience function to convert a data forms of terms into a named list.  For
#' vectors, names are the same as the terms.
#'
#' @param x A vector of strings or a \pkg{quanteda} \code{dictionary}.
#' @param add.boundary logical.  If \code{TRUE} a word boundary is place at the
#' beginning and end of the strings.  Note this is ignored by
#' \code{as_term_list.list}.
#' @param collapse logical.  If \code{TRUE} vectors of regexes are collapsed with
#' a regex OR (|) symbol and wrapped in parenthesis.
#' @param test.regex logical.  If \code{TRUE} the regular expressions created will
#' be tested for validity in \pkg{stringi}.
#' @param \ldots If \code{as_term_list.dictionary2} other arguments passed to
#' \code{\link[textshape]{flatten}}), otherwise ignored.
#' @return Returns a named list.
#' @export
#' @examples
#' as_term_list(state.name)
#'
#' \dontrun{
#' if (!require("pacman")) install.packages("pacman")
#' pacman::p_load(tidyverse, textshape)
#'
#'
#' x <- presidential_debates_2012[["dialogue"]]
#'
#' ngrams <- frequent_ngrams(x, n=10) %>%
#'     transmute(ngram = collocation) %>%
#'     unlist() %>%
#'     as_term_list()
#'
#'
#' presidential_debates_2012 %>%
#'     with(term_count(dialogue, person, ngrams))
#'
#' ## dictionary from quanteda
#' require(quanteda)
#' mfdict <- textreadr::download("https://provalisresearch.com/Download/LaverGarry.zip") %>%
#'     unzip(exdir = tempdir()) %>%
#'     `[`(1) %>%
#'     dictionary(file = .)
#'
#' as_term_list(mfdict)
#' as_term_list(mfdict, add.boundary = TRUE)
#' as_term_list(mfdict, sep = ' -> ')
#'
#' as_term_list(mfdict) %>%
#'     tidy_list('category', 'regex')
#' }
#'
#' ## Writing term list for non-R .json others to use:
#' \dontrun{
#' as_term_list(mfdict, TRUE) %>%
#'     jsonlite::toJSON(pretty=TRUE) %>%
#'     stringi::stri_unescape_unicode() %>%
#'     cat(file = 'testing.json')
#' }
as_term_list <- function(x, add.boundary = FALSE, collapse = FALSE, test.regex = TRUE, ...){
    UseMethod('as_term_list')
}

#' @export
#' @method as_term_list character
as_term_list.character <- function(x, add.boundary = FALSE, collapse = FALSE, test.regex = TRUE, ...){

    if (isTRUE(add.boundary)){
        x <- paste0('\\b', x, '\\b')
    }
    out <- stats::setNames(as.list(x), gsub('\\s+', '_', x))

    read_term_list(term.list = out, collapse = FALSE, test.regex = test.regex, ...)

}

#' @export
#' @method as_term_list dictionary
as_term_list.dictionary <- function(x, add.boundary = FALSE, collapse = FALSE, test.regex = TRUE, ...){

    out <- stats::setNames(lapply(names(x), function(y){
        out <- x[[y]]
        if (isTRUE(add.boundary)){
            out <- gsub("^|(?<!\\*)$", "\\\\b", out, perl = TRUE)
        }
        gsub("\\*", '[A-Za-z0-9-]*', out)
    }), names(x))

    read_term_list(term.list = out, collapse = FALSE, test.regex = test.regex, ...)
}

#' @export
#' @method as_term_list dictionary2
as_term_list.dictionary2 <- function(x, add.boundary = FALSE, collapse = FALSE, test.regex = TRUE, ...){

    x <- quanteda::as.list(x)
    x <- textshape::flatten(x, ...)

    out <- stats::setNames(lapply(names(x), function(y){
        out <- x[[y]]
        if (isTRUE(add.boundary)){
            out <- gsub("^|(?<!\\*)$", "\\\\b", out, perl = TRUE)
        }
        gsub("\\*", '[A-Za-z0-9-]*', out)
    }), names(x))

    read_term_list(term.list = out, collapse = FALSE, test.regex = test.regex, ...)
}

#' @export
#' @method as_term_list list
as_term_list.list <- function(x, add.boundary = FALSE, collapse = FALSE, test.regex = TRUE, ...){

     read_term_list(term.list = x, collapse = collapse, test.regex = test.regex, ...)

}




## http://www.moralfoundations.org/sites/default/files/files/downloads/moral%20foundations%20dictionary.dic


