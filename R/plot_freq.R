#' Vertical Bar Plot of Frequencies of Counts
#'
#' Plot the counts of groups within a vector or list as a horizontal bar plot.
#'
#' @param x A vector or list of elements.
#' @param direct.label logical.  If \code{TRUE} count + percent labels are
#' placed above bars.
#' @param label.diff The amount to place the labels above the bars.  If
#' \code{missing} a reasonable guess is attempted.
#' @param digits The number of percent digits to print.
#' @param top.diff.weight A weight to apply to the space between the top bar and
#' the top of the plot area.
#' @param \ldots Other arguments passed to \code{\link[ggplot2]{geom_text}}.
#' @return \pkg{ggplot2} object
#' @export
#' @importFrom dplyr %>%
#' @examples
#' x <- sample(LETTERS, 100, TRUE)
#' y <- lapply(1:100, function(i) sample(LETTERS[1:10], sample(0:5, 1), TRUE))
#' y <- sapply(y, function(x) {
#'     if(identical(x, character(0))) return(NULL)
#'     x
#' })
#'
#' plot_freq(x)
#' plot_freq(y)
#'
#' ## Example
#' library(dplyr)
#' data(presidential_debates_2012)
#'
#' discoure_markers <- list(
#'     response_cries = c("oh", "ah", "aha", "ouch", "yuk"),
#'     back_channels = c("uh[- ]huh", "uhuh", "yeah"),
#'     summons = "hey",
#'     justification = "because"
#' )
#'
#' presidential_debates_2012 %>%
#'     with(., term_count(dialogue, TRUE, discoure_markers)) %>%
#'     as_terms() %>%
#'     plot_freq(size=3) +
#'         ggplot2::xlab("Number of Tags")
#'
#' presidential_debates_2012 %>%
#'     with(., term_count(dialogue, TRUE, discoure_markers)) %>%
#'     as_terms() %>%
#'     plot_counts() +
#'         ggplot2::xlab("Tags")
plot_freq <- function(x, direct.label = TRUE,
    label.diff, digits = 1, top.diff.weight = .06, ...){

    Terms <- Prop <- Frequency <- Counts <- NULL

    if (is.list(x)){
        y <- table(sapply(x, length))
    } else {
        y <- table(table(x))
    }

    mbar <- max(y/sum(y))
    if (missing(label.diff))  label.diff <- mbar*.03

    dat <- data.frame(names(y), c(unlist(y)), stringsAsFactors = FALSE, row.names=NULL) %>%
        stats::setNames(c("Frequency", "Counts")) %>%
        dplyr::tbl_df() %>%
        dplyr::mutate(
            Frequency = factor(Frequency, levels = Frequency),
            Prop = Counts/sum(y),
            y = Prop + label.diff,
            Prop_Labs = paste0(
                prettyNum(Counts),
                "(",
                paste0(digit_format(100*Prop, digits = digits), "%"), ")")
         )



    out <- dat %>%
        ggplot2::ggplot(ggplot2::aes_string("Frequency", weight = "Prop")) +
            ggplot2::geom_bar() +
            ggplot2::scale_y_continuous(labels=function(x) paste0(100*x, "%"), expand = c(0,0), limits = c(0, mbar + top.diff.weight*mbar)) +
            ggplot2::theme_bw() +
            ggplot2::ylab("Percent") +
            ggplot2::xlab("Frequency Bin")  +
            ggplot2::theme(panel.grid = ggplot2::element_blank())

    if (isTRUE(direct.label)) {
        out <- out + ggplot2::geom_text(ggplot2::aes_string(y = "y", label="Prop_Labs"), ...)
    }
    out
}

