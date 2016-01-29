#' Hierarchical Coverage
#'
#' The unique coverage of a text vector by a term after partitioning out the
#' elements matched by previous terms.
#'
#' @param x A text vector (vector of strings).
#' @param terms A vector of regular expressions to match against \code{x}.
#' @param bind logical.  If \code{TRUE} The terms are bound with boundary
#' markers to ensure \code{"read"} matches \code{"read"} but not \code{"ready"}).
#' @param ignore.case logical.  Should case be ignored in matching the
#' \code{terms} against \code{x}?
#' @param \ldots ignored.
#' @return Returns a \code{\link[base]{data.frame}} with 3 columns:
#' \describe{
#'   \item{terms}{the search term}
#'   \item{unique}{the unique coverage of the term}
#'   \item{cumulative}{the cumulative coverage of the term}
#' }
#' @keywords coverage
#' @export
#' @author Steve T. Simpson and Tyler Rinker <tyler.rinker@@gmail.com>.
#' @examples
#' x <- presidential_debates_2012[["dialogue"]]
#' terms <- frequent_terms(x)[[1]]
#' (out <- hierarchical_coverage(x, terms))
#' plot(out)
#'
#' (out2 <- hierarchical_coverage(x, frequent_terms(x, 30)[[1]]))
#' plot(out2, use.terms = TRUE)
#' plot(out2, use.terms = TRUE, mark.one = FALSE)
hierarchical_coverage <- function(x, terms, bind = TRUE, ignore.case = TRUE, ...){

    unique <- cumulative <- NULL
    original <- terms

    ic <- ifelse(ignore.case, "(?i)", "")
    if (bind) terms <- sprintf("%s(?<=^|[^a-z'])(%s)(?=$|[^a-z'])", ic, terms)

    stopifnot(is.atomic(x))

    nx <- length(x)
    nt <- length(terms)
    i <- 1

    coverage <- vector(mode="numeric", length=nt)

    while (sum(coverage) < 1 & i <= nt){

        covterm <- grepl(terms[i], x, perl = TRUE, ignore.case = ignore.case)
        x <- x[!covterm]
        coverage[i] <- sum(covterm)/nx
        i <- i + 1

    }

    out <- data.frame(term = original, unique = coverage, stringsAsFactors = FALSE)
    out <- dplyr::arrange(out, dplyr::desc(unique))
    out <- dplyr::mutate(out, cumulative = cumsum(unique))
    class(out) <- c("hierarchical_coverage", "data.frame")
    out
}







#' Plots a hierarchical_coverage Object
#'
#' Plots a hierarchical_coverage object
#'
#' @param x A hierarchical_coverage object.
#' @param use.terms logical.  If \code{TRUE} terms are plotted on the x axis.
#' If \code{FALSE} word numbers are.  Te default is to plot terms if they are
#' equal to or less than 30 in length.
#' @param mark.one logical.  If \code{TRUE} a purple horizontal line is added at
#' 100\% and the y axis is extended as well.
#' @param \ldots ignored.
#' @method plot hierarchical_coverage
#' @export
plot.hierarchical_coverage <- function(x, use.terms = nrow(x) <= 30, mark.one = TRUE, ...){

    x[["Word_Number"]] <- seq_len(nrow(x))
    x[["term"]] <- factor(x[["term"]], levels = x[["term"]])

    out <- ggplot2::ggplot(x, ggplot2::aes_string(ifelse(use.terms, 'term', "Word_Number"), 'cumulative', group = 1)) +
        ggplot2::geom_line(size=1, color="blue") +
        ggplot2::scale_y_continuous(label = function(x) paste0(round(100 * x, 0), "%")) +
        ggplot2::ylab("Percent") +
        ggplot2::xlab(ifelse(use.terms, "Term", "Word Number")) +
        ggplot2::theme_bw() +
        ggplot2::theme(axis.text.x = ggplot2::element_text(angle=45, , hjust = 1, vjust = 1))

    if (mark.one) out <- out +  ggplot2::geom_hline(yintercept = 1, color = "purple")
    out  +
        ggplot2::ggtitle("Cumulative Unique Percent Coverage of Each Term")
}











