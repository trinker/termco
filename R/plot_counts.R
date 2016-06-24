#' Horizontal Bar Plot of Group Counts
#'
#' Plot the counts of groups within a vector or list as a horizontal bar plot.
#'
#' @param x A vector or list of elements.
#' @param n Minimum frequency to be shown in the plot.  If \code{NULL} all are
#' shown.
#' @param percent logical.  If \code{TRUE} the x axis is scaled as percentages.
#' Otherwise, the x axis is counts.
#' @param item.name The name of the variable that contains the groups (different
#' element in the vector/list).
#' @param rev logical.  If \code{TRUE} the bars go from least to greatest.
#' @param drop logical.  If \code{FALSE} and \code{x} is an \code{as_terms} object
#' created from a \code{term_count} object, then unfound terms will not be dropped.
#' @param \ldots ignored.
#' @return \pkg{ggplot2} object.
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
#' plot_counts(x)
#' plot_counts(y)
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
#'     as_terms() %>%
#'     plot_counts() +
#'         ggplot2::xlab("Tags")
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
#'     plot_counts(percent=FALSE, item.name = "Tags")
plot_counts <- function(x, n = NULL, percent = TRUE, item.name = "Terms",
    rev= FALSE, drop = TRUE, ...){
    if (isTRUE(percent)){
       plot_counts_percent(x = x, n = n, item.name = item.name, rev = rev, , drop = drop, ...)
    } else {
       plot_counts_count(x = x, n = n, item.name = item.name, rev = rev, , drop = drop, ...)
    }
}


plot_counts_percent <- function(x, n = NULL, item.name = "Terms", rev= FALSE, drop = TRUE, ...){

    Terms <- Prop <- Frequency <- Counts <- NULL

    if(!isTRUE(drop) && is.null(attributes(x)[['term.vars']])) {
        warning("Not derived from `term_count` object; `drop = FALSE` ignored")
    }

    if (!isTRUE(drop) && !is.null(attributes(x)[['term.vars']])){
        term.vars <- attributes(x)[['term.vars']]
    } else {
        term.vars <- NULL
    }

    if (is.list(x)){
        x <- unlist(x)
    }
    y <- sort(table(x), rev)


    dat <- data.frame(names(y), c(unlist(y)), stringsAsFactors = FALSE, row.names=NULL) %>%
        stats::setNames(c("Terms", "Frequency")) %>%
        dplyr::tbl_df() %>%
        dplyr::mutate(
            Terms = factor(Terms, levels = Terms),
            Prop = Frequency/sum(y)
         )

    if (!is.null(term.vars)){

        if(any(!term.vars %in% dat[["Terms"]])){
            Terms <- rev(term.vars[!term.vars %in% dat[["Terms"]]])
            dat <- dplyr::bind_rows(
                dplyr::data_frame(Terms = Terms, Frequency = rep(0, length(Terms)), Prop = Frequency),
                dat
            ) %>%
                dplyr::mutate(Terms = factor(Terms, levels = Terms))
        }
    }


    if (!is.null(n)){
        dat <- dplyr::top_n(dat, n)
    }

    mbar <- max(dat[["Prop"]])

    dat %>%
        ggplot2::ggplot(ggplot2::aes_string("Terms", weight = "Prop")) +
            ggplot2::geom_bar() +
            ggplot2::scale_y_continuous(labels=function(x) paste0(100*x, "%"),
                expand = c(0,0), limits = c(0, mbar + .05*mbar)) +
            ggplot2::theme_bw() +
            ggplot2::ylab("Percent") +
            ggplot2::xlab(item.name)  +
            ggplot2::coord_flip() +
            ggplot2::theme(panel.grid.major.y = ggplot2::element_blank())

}



plot_counts_count <- function(x, n = NULL, item.name = "Terms", rev= FALSE, drop = TRUE, ...){

    Terms <- Prop <- Frequency <- Counts <- NULL

    if(!isTRUE(drop) && is.null(attributes(x)[['term.vars']])) {
        warning("Not derived from `term_count` object; `drop = FALSE` ignored")
    }

    if (!isTRUE(drop) && !is.null(attributes(x)[['term.vars']])){
        term.vars <- attributes(x)[['term.vars']]
    } else {
        term.vars <- NULL
    }

    if (is.list(x)){
        x <- unlist(x)
    }
    y <- sort(table(x), rev)


    dat <- data.frame(names(y), c(unlist(y)), stringsAsFactors = FALSE, row.names=NULL) %>%
        stats::setNames(c("Terms", "Frequency")) %>%
        dplyr::tbl_df() %>%
        dplyr::mutate(
            Terms = factor(Terms, levels = Terms)
         )

    if (!is.null(term.vars)){

        if(any(!term.vars %in% dat[["Terms"]])){
            Terms <- rev(term.vars[!term.vars %in% dat[["Terms"]]])
            dat <- dplyr::bind_rows(
                dplyr::data_frame(Terms = Terms, Frequency = rep(0, length(Terms))),
                dat
            ) %>%
                dplyr::mutate(Terms = factor(Terms, levels = Terms))
        }
    }


    if (!is.null(n)){
        dat <- dplyr::top_n(dat, n)
    }

    mbar <- max(dat[["Frequency"]])

    dat %>%
        ggplot2::ggplot(ggplot2::aes_string("Terms", weight = "Frequency")) +
            ggplot2::geom_bar() +
            ggplot2::scale_y_continuous(expand = c(0,0), limits = c(0, mbar + .05*mbar)) +
            ggplot2::theme_bw() +
            ggplot2::ylab("Frequency") +
            ggplot2::xlab(item.name)  +
            ggplot2::coord_flip() +
            ggplot2::theme(panel.grid.major.y = ggplot2::element_blank())

}

