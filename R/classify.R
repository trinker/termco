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
#' data(markers)
#' classify(markers)
#' classify(markers, n = 2)
classify <- function(x, n = 1, ties.method = "random", ...) {

    val <- validate_term_count(x)
    if (isTRUE(val)) {
        x <- x[, attributes(x)[["term.vars"]]]
    }
    if (n < 2){

        out <- colnames(x)[max.col(x, ties.method = ties.method)]
        out[rowSums(x) == 0] <- NA
        out

    } else {

        apply(x, 1, function(y){
            if (all(y == 0)) return(NA)
            out <- names(sort(rank(y[y > 0], ties.method = ties.method), TRUE))
            out[seq_len(min(length(out), n))]
        })

    }
}
