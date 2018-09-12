#' Search For Terms
#'
#' \code{search_term} - Find text items that contain a term(s).
#'
#' @param text.var A vector of character strings.
#' @param term A regular expression to search for (uses \code{grep}).
#' @param exclude A regular expression to exclude cases for (uses \code{grep}).
#' @param and A regular expression that must also be contained in addition to
#' \code{term} (uses \code{\link[base]{grep}}).
#' @param ignore.case logical. Should \code{\link[base]{grep}} be done
#' independent of case? Can also be length 3 corresponding to the arguments
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
search_term <- function(text.var, term, exclude = NULL, and = NULL, ignore.case=TRUE,
    ...){

    out <- search_term_which(term=term, text.var=text.var, exclude=exclude, and=and, ignore.case)

    out2 <- text.var[out]
    class(out2) <- c("search_term", class(out2))
    attributes(out2)[["coverage"]] <- coverage(out)
    out2
}


#' Prints a search_term Object
#'
#' Prints a search_term object.
#'
#' @param x The search_term object.
#' @param n The number of elements to print.
#' @param width The width of the printing.
#' @param sep A string that separates the elements.
#' @param digits The number of coverage digits to print.
#' @param \ldots ignored
#' @method print search_term
#' @export
print.search_term <- function(x, n = Inf, width = 80,
    sep = '\n\n\n===================================\n', digits = 5, ...){


    out <- unlist(lapply(x, function(x){strwrap(x, width)}%>% paste(collapse = '\n')))

    out <- paste(paste0('[', seq_along(out), ' of ', length(out), ']'), out, sep ='\n\n')
    
    if (n > length(out)) {
        n <- length(out)
    } 
        
    cat(out[seq_len(n)], sep = sep)

    cv1 <- sprintf('\n\n%s\ncoverage = %s', paste(rep('-', 35), collapse = ''), numform::f_num(attributes(x)[['coverage']], digits))
    cv2 <- sprintf('%s of %s', numform::f_comma(length(out)), numform::f_comma(round(length(st)/attributes(st)[['coverage']], 0)))

    cat(cv1, cv2, sep = '  >>>  ')

}

# print.search_term <- #retired on 2018-09-12
#     function(x,  ...) {
# 
#     class(x) <- class(x)[!class(x) %in% "search_term"]
# 
#     print(x)
# 
#     }

#'
#'
#'  \code{search_term_which} - Find index of text items that contain a term(s).
#'
#' @rdname search_term
#' @export
search_term_which <- function(text.var, term, exclude = NULL, and = NULL, ignore.case=TRUE){

    if (!length(ignore.case) %in% c(1, 3)) {
        stop("`ignore.case` must be of length 1 (recycled) or 3 corresponding to the arguments `term`, `exclude`, & `and`")
    }
    if (length(ignore.case) == 1) ignore.case <- rep(ignore.case, 3)
	term <- paste(paste0("(", term, ")"), collapse = "|")
    out <- grepl(term, text.var, ignore.case=ignore.case[1], perl=TRUE)
    if (!is.null(exclude)){
          out <- out & !grepl(exclude, text.var, ignore.case=ignore.case[2], perl=TRUE)
    }
    if (!is.null(and)){
          out <- out & grepl(and, text.var, ignore.case=ignore.case[3], perl=TRUE)
    }
    out
}

