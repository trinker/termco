#' Uncovered/Untagged Group Variable
#'
#' \code{uncovered} - Get logical vector of uncovered data from the original
#' text used to build the model.
#'
#' @note This is most useful when \code{grouping.var = TRUE} and an \code{id} variable
#' was created that corresponds to the text variable.  This allows the user to
#' quickly grab the untagged text.
#' @param x A \code{\link[termco]{term_count}} object.
#' @param \ldots ignored.
#' @return \code{uncovered} - Returns logical indeices of untagged/uncovered group variables.
#' @export
#' @rdname uncovered
#' @examples
#' library(dplyr)
#'
#' untagged <- presidential_debates_2012 %>%
#'      with(., term_count(dialogue, TRUE, list(manners = c("please|excuse|sorry")))) %>%
#'      uncovered() %>%
#'      {presidential_debates_2012[., "dialogue"]} %>%
#'      unlist(use.names = FALSE)
#'
#' ## Shorthand equivalent to code chunk above
#' untagged <- presidential_debates_2012 %>%
#'      with(., term_count(dialogue, TRUE, list(manners = c("please|excuse|sorry")))) %>%
#'      get_uncovered()
#'
#' frequent_terms(untagged)
#' search_term(untagged, colo("romney", "governor"))
#'
#' search_term(untagged, colo("people")) %>%
#'     frequent_terms()
uncovered <- function(x, ...){
    val <- validate_term_count(x)
    if (isTRUE(val)) {
        return(coverage(x)[["not"]])
    } else {
        warning("Expecting a termco object; attempting to provided uncovered indices.",immediate. = TRUE)
    }
    rowSums(x) == 0
}

#' Uncovered/Untagged Group Variable
#'
#' \code{get_uncovered} - Get text vector from the uncovered data of the original
#' text used to build the model.
#'
#' @return \code{get_uncovered} - Returns a vector of uncovered text from the
#' original data set used to train the model.
#' @export
#' @rdname uncovered
get_uncovered <- function(x, ...){
    y <- attributes(x)[["text.var"]][["text.var"]]
    if (length(y) > nrow(x)) {
        stop("`get_uncovered` should be used on a `temco` object where `grouping.var = TRUE`")
    }
    unlist(y, use.names=FALSE)[uncovered(x)]
}



