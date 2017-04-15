#' Coerce to Named List
#'
#' Convenience function to convert a data forms of terms into a named list.  For
#' vectors, names are the same as the terms.
#'
#' @param x A vector of strings or a \pkg{quanteda} \code{dictionary}.
#' @param add.boundary logical.  If \code{TRUE} a word boundary is place at the
#' beginning and end of the strings.
#' @param \ldots ignored.
#' @return Returns a named list.
#' @export
#' @examples
#' as_term_list(state.name)
#'
#' \dontrun{
#' if (!require("pacman")) install.packages("pacman")
#' pacman::p_load(tidyverse)
#'
#'
#' x <- presidential_debates_2012[["dialogue"]]
#'
#' bigrams <- ngram_collocations(x, n=10) %>%
#'     transmute(bigram = paste(term1, term2)) %>%
#'     unlist() %>%
#'     as_term_list()
#'
#'
#' presidential_debates_2012 %>%
#'     with(term_count(dialogue, person, bigrams))
#'
#' ## dictionary from quanteda
#' require(quanteda)
#' mfdict <- dictionary(
#'     file = "http://ow.ly/VMRkL",
#'     format = "LIWC"
#' )
#'
#' as_term_list(mfdict, TRUE)
#' }
as_term_list <- function(x, add.boundary = FALSE, ...){
    UseMethod('as_term_list')
}

#' @export
#' @method as_term_list character
as_term_list.character <- function(x, add.boundary = FALSE, ...){
    if (isTRUE(add.boundary)){
        x <- paste0('\\b', x, '\\b')
    }
    stats::setNames(as.list(x), gsub('\\s+', '_', x))
}

#' @export
#' @method as_term_list dictionary
as_term_list.dictionary <- function(x, add.boundary = FALSE, ...){

    stats::setNames(lapply(names(x), function(y){
        out <- x[[y]]
        if (isTRUE(add.boundary)){
            out <- gsub("^|(?<!\\*)$", "\\\\b", out, perl = TRUE)
        }
        gsub("\\*", '[A-Za-z0-9-]*', out)
    }), names(x))

}

## http://www.moralfoundations.org/sites/default/files/files/downloads/moral%20foundations%20dictionary.dic


