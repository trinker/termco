#' N Most Frequent Terms
#'
#' \code{frequent_terms} - Find a list of the n most frequent terms.
#'
#' @param x A vector of character strings.
#' @param n The number of rows to print.  If integer selects the frequency at
#' the nth row and prints all rows >= that value.  If proportional (less than 0)
#' the frequency value for the nth\% row is selected and prints all rows >= that
#' value.
#' @param stopwords A vector of stopwords to exclude.
#' @param min.freq The minimum frequency to print.  Note that this argument
#' overides the \code{n} argument.
#' @param min.char The minimum number of characters a word must be (including
#' apostrophes) for inclusion.
#' @param max.char The maximum number of characters a word must be (including
#' apostrophes) for inclusion.
#' @param stem logical.  If \code{TRUE} the \code{\link[SnowballC]{wordStem}}
#' is used with \code{language = "porter"} as the default.  Note that stopwords
#' will be stemmed as well.
#' @param language The stem language to use (see  \code{\link[SnowballC]{wordStem}}).
#' @param strip logical.  If \code{TRUE} all values that are not alpha, apostrophe,
#' or spaces are stripped.  This regex can be changed via the \code{strip.regex}
#' argument.
#' @param strip.regex A regular expression used for stripping undesired characters.
#' @param alphabetical logical.  Should rows be arranged alphabetically by term
#' or frequency.
#' @param \ldots ignored.
#' @return Returns a data.frame of terms and frequencies.
#' @importFrom tm stopwords
#' @keywords term word frequency
#' @rdname frequent_terms
#' @export
#' @examples
#' x <- presidential_debates_2012[["dialogue"]]
#'
#' frequent_terms(x)
#' frequent_terms(x, min.char = 1)
#' frequent_terms(x, n = 50)
#' frequent_terms(x, n = .02)
#' frequent_terms(x, stem = TRUE)
#' frequent_terms(x, n = 50, stopwords = c(tm::stopwords("en"), "said", "well"))
#'
#' plot(frequent_terms(x))
#' plot(frequent_terms(x, n = .02))
#' plot(frequent_terms(x, n = 40))
#' plot(frequent_terms(x, n = 40), as.cloud = TRUE)
#'
#' ## Note `n` can be used in print to change how many rows are returned.
#' ## This output can be reassigned when wrapped in print.  This is useful
#' ## reduce computational time on larger data sets.
#' y <- frequent_terms(x, n=10)
#' nrow(y)
#' z <- print(frequent_terms(x, n=100))
#' nrow(z)
#'
#' ## Cumulative Percent Plot
#' plot_cum_percent(frequent_terms(presidential_debates_2012[["dialogue"]]))
frequent_terms <- function(x, n = 20, stopwords = tm::stopwords("en"), min.freq = NULL,
    min.char = 4, max.char = Inf, stem = FALSE, language = "porter", strip = TRUE,
    strip.regex = "[^a-z' ]", alphabetical = FALSE, ...) {

    if (is.data.frame(x)) stop("`x` is a `data.frame`; please pass a vector")

    x <- stringi::stri_trans_tolower(x)

    ## remove nonascii characters
    x <- iconv(x, "latin1", "ASCII", sub = "")

    ## regex strip of non-word/space characters
    if (isTRUE(strip)) x <- gsub(strip.regex, " ", x)

    y <- unlist(stringi::stri_extract_all_words(x))

    ## stemming
    if (isTRUE(stem)) {
        y <- SnowballC::wordStem(y, language = language)
        if (! is.null(stopwords)) stopwords <- SnowballC::wordStem(stopwords, language = language)
    }

    ## exclude less than the min character cut-off
    y <- y[nchar(y) > min.char - 1]

    ## exclude more than the max character cut-off
    y <- y[nchar(y) < max.char + 1]

    ## data frame of counts
    y <- sort(table(y), TRUE)

    ## stopword removal
    if (!is.null(stopwords)){
        y <- y[!names(y) %in% stopwords]
    }

    out <- data.frame(term = names(y), frequency = c(unlist(y, use.names=FALSE)),
        stringsAsFactors = FALSE, row.names=NULL)

    if (isTRUE(alphabetical)){
        out <- out[order(out[["term"]]), ]
    }

    if (n < 1) {
        n <- round(n * nrow(out), 0)
    }

    if (n > nrow(out)) {
        n <- nrow(out)
    }

    if (is.null(min.freq)) {
        out2 <- out[out[["frequency"]] >= out[["frequency"]][n], ]
    } else {
        out2 <- out[out[["frequency"]] >= min.freq, ]
        n <- nrow(out2)
    }

    class(out2) <- c('frequent_terms', class(out))
    attributes(out2)[["n"]] <- n
    attributes(out2)[["full"]] <- out
    out2

}

#' N Most Frequent Terms
#'
#' \code{all_words} - Find a list of all terms used.
#' @rdname frequent_terms
#' @export
all_words <- function(x, stopwords = NULL, min.char = 0, ...) {
    frequent_terms(x, stopwords = stopwords, min.char = min.char, min.freq = 1)
}

#' Prints a frequent_terms Object
#'
#' Prints a frequent_terms object.
#'
#' @param x The \code{frequent_terms} object.
#' @param n The number of rows to print.  If integer selects the frequency at
#' the nth row and prints all rows >= that value.  If proportional (less than 0)
#' the frequency value for the nth\% row is selected and prints all rows >= that
#' value.
#' @param \ldots ignored.
#' @method print frequent_terms
#' @export
print.frequent_terms <- function(x, n, ...){

    if (missing(n)) n <- attributes(x)[["n"]]

    full <- x <- attributes(x)[["full"]]

    if (n < 1) {
        n <- round(n * nrow(x), 0)
    }

    out2 <- x <- x[x[["frequency"]] >= x[["frequency"]][n], ]


    rownames(x) <- 1:nrow(x)
    print(as.matrix(x), justify = "left", quote=FALSE)

    class(out2) <- c('frequent_terms', class(out2))
    attributes(out2)[["n"]] <- n
    attributes(out2)[["full"]] <- full
    out2
    return(invisible(out2))
}



#' Plots a frequent_terms Object
#'
#' Plots a frequent_terms object.
#'
#' @param x The \code{frequent_terms} object.
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
#' @method plot frequent_terms
#' @export
plot.frequent_terms <- function(x, n, as.cloud = FALSE, random.order = FALSE,
    rot.per = 0, ...){

    if (missing(n)) n <- attributes(x)[["n"]]

    if (n < 1) {
        n <- round(n * nrow(x), 0)
    }

    x <- attributes(x)[["full"]]
    x <- x[x[["frequency"]] >= x[["frequency"]][n], ]

    if (isTRUE(as.cloud)) {
        wordcloud::wordcloud(x[[1]], x[[2]], random.order = random.order,
            rot.per = rot.per, ...)
    } else {

        x[["term"]] <- factor(x[["term"]], levels=rev(x[["term"]]))

        ggplot2::ggplot(x, ggplot2::aes_string(x='term', weight='frequency')) +
            ggplot2::geom_bar() +
            ggplot2::coord_flip() +
            ggplot2::ylab("Count") +
            ggplot2::xlab("Terms") +
    	      ggplot2::scale_y_continuous(expand = c(0, 0),
    	          labels = function(x) format(x, big.mark = ",", scientific = FALSE, trim = TRUE),
    	          limits = c(0, 1.01 * x[1, "frequency"])) +
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

