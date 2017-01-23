#' Convert Vector to Named List
#'
#' Convenience function to convert a vector of terms into a named list.  Names
#' are the same as the terms.
#'
#' @param x A vector of strings.
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
#' }
as_term_list <- function(x, ...){

    stopifnot(is.atomic(x))
    stats::setNames(as.list(x), gsub('\\s+', '_', x))
}

