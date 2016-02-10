#' Plot Cumulative Percent of Terms
#'
#' Plot a cumulative percentage of terms for frequent terms.
#'
#' @param x A \code{frequent_terms} object.
#' @param rotate.term logical.  If \code{TRUE} the term labels will be rotated 45 degrees.
#' @param \ldots ignored.
#' @export
#' @examples
#' plot_cum_percent(frequent_terms(presidential_debates_2012[["dialogue"]]))
plot_cum_percent <- function(x, rotate.term = TRUE,  ...){

    term <- NULL

    out <- x %>%
        dplyr::mutate(
            prop = frequency/attributes(x)[["n.words"]],
            cumprop = cumsum(prop),
            term = factor(term, levels = term)
        ) %>%
        ggplot2::ggplot(ggplot2::aes_string('term', 'cumprop', group = 1)) +
            ggplot2::geom_line(size=1, color="blue") +
            ggplot2::geom_point(size=3, shape=16, color="blue") +
            ggplot2::geom_point(size=1.2, shape=16, color = "white") +
            ggplot2::scale_y_continuous(label = function(x) paste0(round(100 * x, 0), "%")) +
            ggplot2::ylab("Percent") +
            ggplot2::xlab("Term") +
            ggplot2::theme_bw()
    if (isTRUE(rotate.term)){
        out <- out +
            ggplot2::theme(axis.text.x = ggplot2::element_text(angle=45, , hjust = 1, vjust = 1))
    }

    out   +
        ggplot2::ggtitle("Cumulative Percent Coverage of Each Term")
}

