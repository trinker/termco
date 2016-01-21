#' Generate List of Exploration \code{search_term}  Function Calls
#'
#' The task of determining the regexes used to feed a \code{term_count} object's
#' \code{term.list} requires careful exploration of term use in context.  This
#' function generates a list of function calls for \code{search_term} with a
#' user predefined data set and term list.
#'
#' @param terms A vector of regex terms to explore (often populated from
#' \code{frequent_terms}.
#' @param data.name A character vector of a data set's name that will serve as
#' the search context.
#' @param copy2clip logical.  If code{TRUE} uses \code{\link[clipr]{write_clip}}
#' to copy the output to the clipboard.  This option is most useful when trying
#' to build a list regular expression model for easy pasting between testing
#' a regex and putting it into the model.  This argument can be set globally by
#' setting \code{options(termco.copy2clip = TRUE)}.
#' @return Returns a string with the concatenated function calls.  The print
#' method separates the concatenated string into new line function calls.  If
#' \code{copy2clip = TRUE} the calls are easily pasted for use in exploration
#' of the terms in the text data set.
#' @export
#' @family probe functions
#' @examples
#' probe_list(c("thank", "\\bthe\"", "ee"), "sam_i_am")
#'
#' txt <- presidential_debates_2012[["dialogue"]]
#' terms <- frequent_terms(txt)[["term"]]
#' probe_list(terms, "txt")
#'
#' \dontrun{
#' probe_list(terms, "txt", copy2clip = TRUE)
#' }
probe_list <- function(terms, data.name, copy2clip = getOption("termco.copy2clip")){

    terms <- stringi::stri_escape_unicode(terms)

    m <- paste(paste0('search_term(',  data.name, ', "', terms, "\")"), collapse = "\n")
    if (isTRUE(copy2clip)) clipr::write_clip(m)
    class(m) <- c("probe_list", class(m))
    m
}

#' Prints a probe_list Object
#'
#' Prints a probe_list object
#'
#' @param x A probe_list object
#' @param \ldots ignored.
#' @method print probe_list
#' @export
print.probe_list <- function(x, ...){
    cat(x, "\n")
}


