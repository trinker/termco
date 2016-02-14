#' Search For Colocations
#'
#' \code{search_colo} - Find colocations in text items that contain a term(s).
#'
#' @param text A text vect to search through.
#' @param colo A regular expression to search for (uses \code{grep}).
#' @param \ldots Other options related to search_terms() function
#' @return \code{search_colo} - Returns a data.frame containing top terms
#' colocating with the \code{colo} regex.
#' @keywords search
#' @rdname search_colo
#' @export
#' @examples
#' search_colo(sam_i_am, "\\bsam")


search_colo <- function(text, colo, n=10, ...) {
    frequent_terms(
        search_term(text, paste0(colo), ...)
                   , n
                    , stopwords=paste(colo) )
}

