#' Hierarchical Coverage of Terms
#'
#' The unique coverage of a text vector by a term after partitioning out the
#' elements matched by previous terms.
#'
#' @param text.var A text vector (vector of strings).
#' @param terms A vector of regular expressions to match against \code{x}.
#' @param bound logical.  If \code{TRUE} the terms are bound with boundary
#' markers to ensure \code{"read"} matches \code{"read"} but not \code{"ready"}).
#' @param ignore.case logical.  Should case be ignored in matching the
#' \code{terms} against \code{x}?
#' @param sort logical.  If \code{TRUE} the output is sorted by highest unique
#' gain.  If \code{FALSE} order of term input is retained.
#' @param \ldots ignored.
#' @return Returns a \code{\link[base]{data.frame}} with 3 columns:
#' \describe{
#'   \item{terms}{the search term}
#'   \item{unique}{the unique coverage of the term}
#'   \item{cumulative}{the cumulative coverage of the term}
#' }
#' @keywords coverage
#' @family hierarchical_coverage functions
#' @export
#' @author Steve T. Simpson and Tyler Rinker <tyler.rinker@@gmail.com>.
#' @examples
#' x <- presidential_debates_2012[["dialogue"]]
#' terms <- frequent_terms(x)[[1]]
#' (out <- hierarchical_coverage_term(x, terms))
#' plot(out)
#'
#' (out2 <- hierarchical_coverage_term(x, frequent_terms(x, 30)[[1]]))
#' plot(out2, use.terms = TRUE)
#' plot(out2, use.terms = TRUE, mark.one = TRUE)
hierarchical_coverage_term <- function(text.var, terms, bound = TRUE, ignore.case = TRUE,
    sort = FALSE, ...){

    unique <- cumulative <- NULL
    original <- terms

    ic <- ifelse(ignore.case, "(?i)", "")
    if (bound) terms <- sprintf("%s(?<=^|[^a-z'])(%s)(?=$|[^a-z'])", ic, terms)

    stopifnot(is.atomic(text.var))

    nx <- length(text.var)
    nt <- length(terms)
    i <- 1

    coverage <- vector(mode="numeric", length=nt)

    while (sum(coverage) < 1 & i <= nt){

        covterm <- grepl(terms[i], text.var, perl = TRUE, ignore.case = ignore.case)
        text.var <- text.var[!covterm]
        coverage[i] <- sum(covterm)/nx
        i <- i + 1

    }

    out <- data.frame(term = original, unique = coverage, stringsAsFactors = FALSE)
    if (isTRUE(sort)) out <- dplyr::arrange(out, dplyr::desc(unique))
    out <- dplyr::mutate(out, cumulative = cumsum(unique))
    class(out) <- c("hierarchical_coverage_term", "data.frame")
    attributes(out)[["remaining"]] <- text.var
    out
}







#' Plots a hierarchical_coverage_term Object
#'
#' Plots a hierarchical_coverage_term object
#'
#' @param x A hierarchical_coverage_term object.
#' @param use.terms logical.  If \code{TRUE} terms are plotted on the x axis.
#' If \code{FALSE} word numbers are.  Te default is to plot terms if they are
#' equal to or less than 30 in length.
#' @param mark.one logical.  If \code{TRUE} a purple horizontal line is added at
#' 100\% and the y axis is extended as well.
#' @param sort logical.  If \code{TRUE} the terms are sorted by highest unique
#' gain.
#' @param \ldots ignored.
#' @method plot hierarchical_coverage_term
#' @export
plot.hierarchical_coverage_term <- function(x, use.terms = nrow(x) <= 30,
    mark.one = FALSE, sort = FALSE, ...){

    x[["Word_Number"]] <- seq_len(nrow(x))

    unique_prop <- unique_n <- NULL
    if (isTRUE(sort)) {
        x <- dplyr::arrange(x, dplyr::desc(unique))
        x <- dplyr::mutate(x, cumulative = cumsum(unique))
    }

    x[["term"]] <- factor(x[["term"]], levels = x[["term"]])

    out <- ggplot2::ggplot(x, ggplot2::aes_string(ifelse(use.terms, 'term', "Word_Number"), 'cumulative', group = 1)) +
        ggplot2::geom_line(size=1, color="blue") +
        ggplot2::geom_point(size=3, shape=16, color="blue") +
        ggplot2::geom_point(size=1.2, shape=16, color = "white") +
        ggplot2::scale_y_continuous(label = function(x) paste0(round(100 * x, 0), "%")) +
        ggplot2::ylab("Percent") +
        ggplot2::xlab(ifelse(use.terms, "Term", "Word Number")) +
        ggplot2::theme_bw() +
        ggplot2::theme(axis.text.x = ggplot2::element_text(angle=45, , hjust = 1, vjust = 1))

    if (mark.one) out <- out +  ggplot2::geom_hline(yintercept = 1, color = "purple")
    out  +
        ggplot2::ggtitle("Cumulative Unique Percent Coverage of Each Term")
}











