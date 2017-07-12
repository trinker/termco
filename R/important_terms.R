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
#' \dontrun{
#' x <- presidential_debates_2012[["dialogue"]]
#'
#' frequent_terms(x)
#' important_terms(x)
#' important_terms(x, n=899)
#' important_terms(x, n=.1)
#' important_terms(x, min.char = 7)
#' important_terms(x, min.char = 6, stem=TRUE)
#'
#' plot(important_terms(x))
#' plot(important_terms(x, n = .02))
#' plot(important_terms(x, n = 40))
#' plot(important_terms(x, n = 100), as.cloud = TRUE)
#' }
important_terms <- function (text.var, n = 20, stopwords = tm::stopwords("en"),
    stem = FALSE, language = "porter", strip = TRUE, strip.regex = "[^A-Za-z' ]",
    ...) {

    if (is.data.frame(text.var)) stop("`text.var` is a `data.frame`; please pass a vector")

    if (isTRUE(strip)) text.var <- gsub(strip.regex, " ", text.var)

    if (isTRUE(stem)){
        dtm <- gofastr::q_dtm_stem(text.var)
        stopwords <- SnowballC::wordStem(stopwords, language = language)
    } else {
        dtm <- gofastr::q_dtm(text.var)
    }

    dots <- any(names(list(...)) %in% names(formals(gofastr::remove_stopwords))[-1])

    if (!is.null(stopwords) | dots) {
        dtm <- gofastr::remove_stopwords(dtm, stopwords = stopwords, stem = stem, ...)
    }

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





#' Plots a important_terms Object
#'
#' Plots a important_terms object.
#'
#' @param x The \code{important_terms} object.
#' @param n The number of rows to plot.  If integer selects the frequency at
#' the nth row and plots all rows >= that value.  If proportional (less than 0)
#' the frequency value for the nth\% row is selected and plots all rows >= that
#' value.
#' @param as.cloud logical.  If \code{TRUE} a wordcloud will be plotted rather
#' than a bar plot.
#' @param random.order logical.  Should the words be place randomly around the
#' cloud or if \code{FALSE} the more frequent words are in the center of the cloud.
#' @param rot.per The precentage of rotated words.
#' @param \ldots Other arguments passed to \code{\link[wordcloud]{wordcloud}}.
#' @method plot important_terms
#' @export
plot.important_terms <- function(x, n, as.cloud = FALSE, random.order = FALSE,
    rot.per = 0, ...){

    if (missing(n)) n <- attributes(x)[["n"]]

    if (n < 1) {
        n <- round(n * nrow(x), 0)
    }

    x <- x[x[["tf_idf"]] >= x[["tf_idf"]][n], ]

    if (isTRUE(as.cloud)) {
        wordcloud::wordcloud(x[[1]], x[[2]], random.order = random.order,
            rot.per = rot.per, ...)
    } else {

        x[["term"]] <- factor(x[["term"]], levels=rev(x[["term"]]))

        ggplot2::ggplot(x, ggplot2::aes_string(x='term', weight='tf_idf')) +
            ggplot2::geom_bar() +
            ggplot2::coord_flip() +
            ggplot2::ylab("TF-IDF") +
            ggplot2::xlab("Terms") +
    	      ggplot2::scale_y_continuous(expand = c(0, 0),
    	          labels = function(x) format(x, big.mark = ",", scientific = FALSE, trim = TRUE),
    	          limits = c(0, 1.02 * x[1, "tf_idf"])) +
            ggplot2::theme_bw() +
            ggplot2::theme(
            panel.grid.major.y = ggplot2::element_blank(),
            #legend.position="bottom",
            legend.title = ggplot2::element_blank(),
            panel.border = ggplot2::element_blank(),
            axis.line = ggplot2::element_line(color="grey70")
        )
    }

}






