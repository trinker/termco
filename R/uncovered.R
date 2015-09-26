#' Indices of Uncovered/Untagged Group Variable
#'
#' This is most useful when \code{grouping.var = TRUE} and an \code{id} variable
#' was created that corresponds to the text variable.  This allows the user to
#' quickly grab the untagged text.
#'
#' @param x A \code{\link[termco]{term_count}} object.
#' @param \ldots ignored.
#' @return Returns logical indeices of untagged/uncovered group variables.
#' @export
#' @examples
#' library(dplyr)
#'
#' untagged <- presidential_debates_2012 %>%
#'      with(., term_count(dialogue, TRUE, list(manners = c("please|excuse|sorry")))) %>%
#'      uncovered(FALSE) %>%
#'      {presidential_debates_2012[., "dialogue"]} %>%
#'      unlist()
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








