#' Search For Terms
#'
#' \code{search_term} - Find text items that contain a term(s).
#'
#' @param x A vector of character strings.
#' @param term A regular expression to search for (uses \code{grep}).
#' @param exclude A regular expression to exclude cases for (uses \code{grep}).
#' @param and A regular expression that must also be contained in addition to
#' \code{term} (uses \code{\link[base]{grep}}).
#' @param ignore.case logical. Should \code{\link[base]{grep}} be done
#' idependant of case? Can also be length 3 corresponding to the arguments
#' \code{term}, \code{exclude}, & \code{and}.
#' @param \ldots ignored.
#' @return \code{search_term} - Returns a text vector meeting \code{term}
#' regex but not \code{exclude} regex.
#' @keywords search
#' @rdname search_term
#' @export
#' @examples
#' search_term_which(sam_i_am, "\\bsam")
#' search_term(sam_i_am, "\\bsam")
#' search_term(sam_i_am, c('green', "\\bsam"))
search_term <- function(x, term, exclude = NULL, and = NULL, ignore.case=TRUE,
    ...){

    out <- search_term_which(term=term, x=x, exclude=exclude, and=and, ignore.case)

    out2 <- x[out]
    class(out2) <- c("search_term", class(out2))
    attributes(out2)[["coverage"]] <- coverage(out)
    out2
}


#' Prints a search_term Object
#'
#' Prints a search_term object.
#'
#' @param x The search_term object.
#' @param \ldots ignored
#' @method print search_term
#' @export
print.search_term <-
    function(x,  ...) {

    class(x) <- class(x)[!class(x) %in% "search_term"]

    print(x)

}

#'
#'
#'  \code{search_term_which} - Find index of text items that contain a term(s).
#'
#' @rdname search_term
#' @export
search_term_which <- function(x, term, exclude = NULL, and = NULL, ignore.case=TRUE){

    if (!length(ignore.case) %in% c(1, 3)) {
        stop("`ignore.case` must be of length 1 (recycled) or 3 corresponding to the arguments `term`, `exclude`, & `and`")
    }
    if (length(ignore.case) == 1) ignore.case <- rep(ignore.case, 3)
	term <- paste(paste0("(", term, ")"), collapse = "|")
    out <- grepl(term, x, ignore.case=ignore.case[1], perl=TRUE)
    if (!is.null(exclude)){
          out <- out & !grepl(exclude, x, ignore.case=ignore.case[2], perl=TRUE)
    }
    if (!is.null(and)){
          out <- out & grepl(and, x, ignore.case=ignore.case[3], perl=TRUE)
    }
    out
}

