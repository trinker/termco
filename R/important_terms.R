#' Top Min-Max Scaled TF-IDF terms
#'
#' View the top n min-max scaled tf-idf weighted terms in a text.
#'
#' @param text.var A vector of character strings.
#' @param n The number of rows to print.  If integer selects the frequency at
#' the nth row and prints all rows >= that value.  If proportional (less than 0)
#' the frequency value for the nth\% row is selected and prints all rows >= that
#' value.
#' @param stopwords A vector of stopwords to exclude.
#' @param stem logical.  If \code{TRUE} the \code{\link[SnowballC]{wordStem}}
#' is used with \code{language = "porter"} as the default.  Note that stopwords
#' will be stemmed as well.
#' @param language The stem language to use (see  \code{\link[SnowballC]{wordStem}}).
#' @param strip logical.  If \code{TRUE} all values that are not alpha, apostrophe,
#' or spaces are stripped.  This regex can be changed via the \code{strip.regex}
#' argument.
#' @param strip.regex A regular expression used for stripping undesired characters.
#' @param \ldots \code{\link[gofastr]{remove_stopwords}}
#' @return Returns a \code{\link[base]{data.frame}} of terms and min-max scaled tf-idf weights.
#' @keywords important
#' @export
#' @examples
#' x <- presidential_debates_2012[["dialogue"]]
#'
#' frequent_terms(x)
#' important_terms(x)
#' important_terms(x, n=899)
#' important_terms(x, n=.1)
#' important_terms(x, min.char = 7)
#' important_terms(x, min.char = 6, stem=TRUE)
important_terms <- function (text.var, n = 20, stopwords = tm::stopwords("en"),
    stem = FALSE, language = "porter", strip = TRUE, strip.regex = "[^A-Za-z' ]",
    ...) {

    if (isTRUE(strip)) text.var <- gsub(strip.regex, " ", text.var)

    if (isTRUE(stem)){
        dtm <- gofastr::q_dtm_stem(text.var)
        stopwords <- gofastr:::stem(stopwords)
    } else {
        dtm <- gofastr::q_dtm(text.var)
    }

    if (!is.null(stopwords)) dtm <- gofastr::remove_stopwords(dtm, stopwords = stopwords, stem = stem, ...)

    dtm <- suppressWarnings(tm::weightTfIdf(dtm))

    sorted <- sort(minmax_scale(slam::col_sums(dtm)/nrow(dtm)), TRUE)
    out <- data.frame(term = names(sorted), tf_idf = unlist(sorted, use.names=FALSE),
                      stringsAsFactors = FALSE, row.names=NULL)

    class(out) <- c("important_terms", class(out))
    attributes(out)[["n"]] <- n
    out

}

#' Prints an important_terms Object
#'
#' Prints an important_terms object
#'
#' @param x An important_terms object.
#' @param n The number of rows to print.  If integer selects the frequency at
#' the nth row and prints all rows >= that value.  If proportional (less than 0)
#' the frequency value for the nth\% row is selected and prints all rows >= that
#' value.
#' @param \ldots Ignored.
#' @method print important_terms
#' @export
print.important_terms <- function(x, n = NULL, ...){
    if (is.null(n)) n <- attributes(x)[['n']]
    if (n < 1) n <- ceiling(n * nrow(x))
    if (n > nrow(x)) n <- nrow(x)
    x <- rm_class(x, 'important_terms')
    print(x[x[['tf_idf']] >= x[n, 'tf_idf'], ] )
}

