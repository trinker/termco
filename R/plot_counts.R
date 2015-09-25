#' Horizontal Bar Plot of Group Counts
#'
#' Plot the counts of groups within a vector or list as a horizontal bar plot.
#'
#' @param x A vector or list of elements.
#' @param n Minimum frequency to be shown in the plot.  If \code{NULL} all are
#' shown.
#' @param item.name The name of the variable that contains the groups (different
#' element in the vector/list).
#' @param rev logical.  If \code{TRUE} the bars go from least to greatest.
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
plot_counts <- function(x, n = NULL, item.name = "Terms", rev= FALSE, ...){

    Terms <- Prop <- Frequency <- Counts <- NULL

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

    if (!is.null(n)){
        dat <- dplyr::top_n(dat, n)
    }

    mbar <- max(dat[["Prop"]])

    dat %>%
        ggplot2::ggplot(ggplot2::aes_string("Terms", weight = "Prop")) +
            ggplot2::geom_bar() +
            ggplot2::scale_y_continuous(labels=function(x) paste0(100*x, "%"), expand = c(0,0), limits = c(0, mbar + .05*mbar)) +
            ggplot2::theme_bw() +
            ggplot2::ylab("Percent") +
            ggplot2::xlab(item.name)  +
            ggplot2::coord_flip() +
            ggplot2::theme(panel.grid.major.y = ggplot2::element_blank())

}

