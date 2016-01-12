#' Classify Rows of a Count Matrix
#'
#' Use a count matrix to classify the rows, based on the frequencies in the cells.
#'
#' @param x A \code{term_count} or count \code{\link[base]{matrix}}/\code{\link[base]{data.frame}}.
#' @param n The number of classifications per row to return.
#' @param ties.method Either \code{c("random", "first", "last")} for specifying
#' how ties are handled; "random" by default.  See \code{\link[base]{max.col}}.
#' @param \ldots ignored.
#' @return Returns a single \code{\link[base]{vector}} or \code{\link[base]{list}}
#' of ordered vectors of predicted classifications; order by term frequency.
#' Ties default to random order.
#' @keywords classify predict
#' @export
#' @seealso \code{\link[base]{max.col}}
#' @examples
#' set.seed(10)
#' (x <- data.frame(matrix(sample(c(0, 0, 0, 1, 2, 3), 24, TRUE), ncol=3)))
#' classify(x)
#' classify(x, n=3)
#'
#' ## Example
#' library(dplyr)
#' data(presidential_debates_2012)
#'
#' discoure_markers <- list(
#'     response_cries = c("\\boh", "\\bah", "\\baha", "\\bouch", "yuk"),
#'     back_channels = c("uh[- ]huh", "uhuh", "yeah"),
#'     summons = "hey",
#'     justification = "because"
#' )
#'
#' presidential_debates_2012 %>%
#'     with(., term_count(dialogue, TRUE, discoure_markers)) %>%
#'     classify()
#'
#' presidential_debates_2012 %>%
#'     with(., term_count(dialogue, TRUE, discoure_markers)) %>%
#'     classify(n = 2)
#'
#' presidential_debates_2012 %>%
#'     with(., term_count(dialogue, TRUE, discoure_markers)) %>%
#'     {.[!uncovered(.), -c(1:2)]} %>%
#'     classify()
classify <- function(x, n = 1, ties.method = "random", ...) {

    val <- validate_term_count(x)
    if (isTRUE(val)) {
        x <- x[, attributes(x)[["term.vars"]]]
    }
    if (n < 2){

        out <- colnames(x)[max.col(x, ties.method = ties.method)]
        out[rowSums(x) == 0] <- NA
        out
        class(out) <- unique(c("classify", class(out)))
        out
    } else {

        out <- apply(x, 1, function(y){
            if (all(y == 0)) return(NA)
            out <- names(sort(rank(y[y > 0], ties.method = ties.method), TRUE))
            out[seq_len(min(length(out), n))]
        })
        class(out) <- unique(c("classify", class(out)))
        out
    }
}

#' Plots a plot.classify Object
#'
#' Plots a plot.classify object
#'
#' @param x A classify object.
#' @param \ldots Other arguments passed to \code{\link[termco]{plot_counts}}.
#' @method plot classify
#' @export
plot.classify <- function(x, ...){
    plot_counts(unlist(x), ...) + ggplot2::xlab("Number of Tags")
}
