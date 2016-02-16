#' Search For Collocations
#'
#' A wrapper for \code{\link[termco]{search_term}} +
#' \code{\link[termco]{frequent_terms}}.  Find words that frequently collocate
#' with a term(s).  Note that the `\code{term} regexes are eliminated from the
#' output of top occurring terms.
#'
#' @param text.var A vector of character strings.
#' @param term A regular expression(s) to search for (uses \code{grep}).
#' @param n The number of rows to print.  If integer selects the frequency at
#' the nth row and prints all rows >= that value.  If proportional (less than 0)
#' the frequency value for the nth\% row is selected and prints all rows >= that
#' value.
#' @param ignore.case logical. Should \code{grep} be done independent of case?
#' @param \ldots Other arguments passed to \code{\link[termco]{search_term}}
#' and \code{\link[termco]{frequent_terms}}.
#' @return Returns a \code{\link[base]{data.frame}} of collocating terms and
#' frequencies.
#' @keywords collocation
#' @export
#' @author Steve T. Simpson and Tyler Rinker <tyler.rinker@@gmail.com>.
#' @examples
#' ## Example 1
#' search_term_collocations(sam_i_am, "\\bsam")
#' search_term_collocations(sam_i_am, c('green', "\\bsam"))
#' search_term_collocations(sam_i_am, c('green', "\\bsam"), min.char=2)
#'
#' ## Example 2
#' top_colo <- search_term_collocations(
#'     presidential_debates_2012[["dialogue"]],
#'     "president",
#'     n =50
#' )
#'
#' top_colo
#' plot(top_colo)
#' plot(top_colo, as.cloud=TRUE)
#'
#' ## Example 3
#' top_colo_exclude <- search_term_collocations(
#'     presidential_debates_2012[["dialogue"]],
#'     "president",
#'     exclude = "obama",
#'     n =50
#' )
#'
#' top_colo_exclude
#' plot(top_colo_exclude)
search_term_collocations <- function(text.var, term, n=10, ignore.case = TRUE, ...) {

    ## frequent terms for a term (collocations)
    out <- frequent_terms(search_term(text.var, term, ignore.case=ignore.case, ...),
        n = n, ...)

    ## reset n.words and n from output
    n.words <- attributes(out)[["n.words"]]

    if (n < 1) {
        n <- round(n * nrow(out), 0)
    }
    if (n > nrow(out)) {
        n <- nrow(out)
    }

    ## remove the search term itself and get `n` rows
    term <- paste(paste0("(", term, ")"), collapse = "|")
    out <- attributes(out)[["full"]][
        !stringi::stri_detect_regex(attributes(out)[["full"]][["term"]], term,
            opts_fixed = stringi::stri_opts_fixed(case_insensitive = ignore.case )),
    ]
    out2 <- out[out[["frequency"]] >= out[n, "frequency"], ]

    ## Class setup
    class(out2) <- c("frequent_terms", class(out))
    attributes(out2)[["n"]] <- min(n, nrow(out))
    attributes(out2)[["full"]] <- out
    attributes(out2)[["n.words"]] <- n.words
    out2

}
