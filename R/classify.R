#' Classify Rows of a Count Matrix
#'
#' Use a count matrix to classify the rows, based on the frequencies in the cells.
#'
#' @param x A \code{term_count} or count \code{\link[base]{matrix}}/\code{\link[base]{data.frame}}.
#' @param n The number of classifications per row to return.
#' @param ties.method Either \code{c("probability", "random", "first", "last")}
#' for specifying how ties are handled; "probability" by default.  This utilizes
#' the probability distributions from all tags (regardless of strength/counts of
#' tags) to randomly \code{\link[base]{sample}} with probabilties to break ties.
#' Note that this can lead to different results each time \code{classify} is run.
#' Use \code{seed} to make results reproducible.  The other methods
#' use \code{\link[base]{max.col}} for tie breaking.  See
#' \code{\link[base]{max.col}} for a description of those arguments.
#' @param seed A seed to use in the sample to make the results reproducible.
#' @param \ldots ignored.
#' @return Returns a single \code{\link[base]{vector}} or \code{\link[base]{list}}
#' of ordered vectors of predicted classifications; order by term frequency.
#' Ties default to random order.
#' @keywords classify predict
#' @export
#' @seealso \code{\link[base]{max.col}}
#' @examples
#' \dontrun{
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
#'     classify() %>%
#'     plot()
#'
#' presidential_debates_2012 %>%
#'     with(., term_count(dialogue, TRUE, discoure_markers)) %>%
#'     classify() %>%
#'     plot(rm.na=FALSE)
#'
#' presidential_debates_2012 %>%
#'     with(., term_count(dialogue, TRUE, discoure_markers)) %>%
#'     classify(n = 2)
#'
#' presidential_debates_2012 %>%
#'     with(., term_count(dialogue, TRUE, discoure_markers)) %>%
#'     {.[!uncovered(.), -c(1:2)]} %>%
#'     classify()
#' }
classify <- function(x, n = 1, ties.method = "probability", seed = NULL, ...) {

    val <- validate_term_count(x)
    if (isTRUE(val)) {
        x <- tag_cols(x)
    } else {
        warning("Object is not `term_count`...\n",
            "Object may have been altered.  Check to make sure tags are correct.")
    }

    if (n < 2){
        if (ties.method == "probability"){

            probs <- (probs <- sort(colSums(x > 0), TRUE))/sum(probs)

            out <- apply(x, 1, function(y){
                if (sum(y) == 0) return(NA)
                pot_tag <- names(y[y == max(y)])
                if (length(pot_tag) == 1) {
                    out <- pot_tag
                } else {
                    props <- probs[match(pot_tag, names(probs))]
                    if (!is.null(seed)) set.seed(seed)
                    out <- sample(names(props), 1, prob = props)
                }

            })

        } else {

            if (!is.null(seed)) set.seed(seed)
            out <- colnames(x)[max.col(x, ties.method = ties.method)]

        }
        out[rowSums(x) == 0] <- NA
        out
        class(out) <- unique(c("classify", class(out)))
        out
    } else {
        if (ties.method == "probability"){

            probs <- (probs <- sort(colSums(x > 0), TRUE))/sum(probs)

            out <- apply(x, 1, function(y){
                if (sum(y) == 0) return(NA)

                pot_tag <- sort(y[y > 0], TRUE)

                if (length(pot_tag) <= n) {
                    out <- names(pot_tag)
                } else {
                    min_val <- pot_tag[n]
                    pot_tag <- pot_tag[pot_tag >= min_val]

                    front <- names(pot_tag[pot_tag > min_val])
                    back <- names(pot_tag[pot_tag == min_val])

                    props <- probs[match(back, names(probs))]

                    if (!is.null(seed)) set.seed(seed)
                    out <- c(front, sample(names(props), min(n - length(front), length(props)), prob = props))
                }
                out
            })

        } else {
            out <- apply(x, 1, function(y){
                if (all(y == 0)) return(NA)
                pot_tag <- sort(y[y > 0], TRUE)

                if (length(pot_tag) <= n) {
                    out <- names(pot_tag)
                } else {

                    if (!is.null(seed)) set.seed(seed)
                    out <- names(sort(rank(y[y > 0], ties.method = ties.method), TRUE))
                    out <- out[seq_len(min(length(out), n))]
                }
                out
            })
        }

        if (nrow(x) == 1) out <- list(c(out))

        class(out) <- unique(c("classify", class(out)))
        out
    }
}

#' Plots a plot.classify Object
#'
#' Plots a plot.classify object
#'
#' @param x A classify object.
#' @param rm.na logical.  If \code{TRUE} unclassified \code{NA} values are not displayed.
#' @param \ldots Other arguments passed to \code{\link[termco]{plot_counts}}.
#' @method plot classify
#' @export
plot.classify <- function(x, rm.na = TRUE, ...){

    x <- unlist(x)
    if (!isTRUE(rm.na)) x[is.na(x)] <- "No Tag"
    plot_counts(x, ...) + ggplot2::xlab("Number of Tags")
}

