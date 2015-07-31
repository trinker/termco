#' Extract Coverage Information
#'
#' Extract coverage information from a termco object including the percentage of
#' rows that sum to zero as well as the location of non-covered rows for easy
#' extraction.
#'
#' @param x A \code{\link[termco]{term_count}} object.
#' @param \ldots ignored
#' @return Returns a list:
#' \item{not}{A logical vector of all rows not covered (row sums equal zero)}
#' \item{covered}{A logical vector of all rows covered (row sums greater than zero)}
#' \item{coverage}{The percentage rate of \deqn{\frac{covered}{not + covered}}{covered/(not + covered)}}
#' \item{n_covered}{The row sums of the unique terms}
#' \item{total_terms}{The row sums of the terms}
#' @keywords coverage
#' @export
#' @examples
#' data(pres_debates2012)
#'
#' discoure_markers <- list(
#'     like = c("love", "like"),
#'     water = c("lake", "ocean", "water"),
#'     justify = c("because"),
#'     he = c("\\bhe", "him"),
#'     we = c("\\bwe", "\\bus", "\\bour")
#' )
#'
#' library(dplyr)
#' (markers2 <- with(dplyr::mutate(pres_debates2012, turn = dplyr::id(dialogue)),
#'     term_count(dialogue, turn, discoure_markers)
#' ))
#'
#' coverage(markers2)
#'
#' pres_debates2012[coverage(markers2)$not, "dialogue"] %>%
#'    c()
coverage <- function(x, ...){

    val <- validate_term_count(x)
    if (!isTRUE(val)) {

        termcols <- attributes(x)[["term.vars"]]
        wrdscol <- any(colnames(x) %in% 'n.words')

        if (wrdscol & !is.null(termcols) && any(colnames(x) %in% termcols)) {

            termcols <- colnames(x)[colnames(x) %in% termcols]

        } else {

            warning("Does not appear to be a `term_count` object.\n",
                "  Has the object or column names been altered?",
                immediate. = TRUE
            )

            return(NULL)

        }
    } else {

        termcols <- attributes(x)[["term.vars"]]
    }

    total_terms <- rowSums(x[, termcols])
    n_covered <- rowSums(x[, termcols] > 0)
    not_covered <- n_covered < 1
    covered <- !not_covered
    coverage <- mean(covered, na.rm = TRUE) * 100
    out <- list(not = not_covered, covered = covered,
        coverage = coverage, n_covered = n_covered, total_terms = total_terms)
    class(out) <- "coverage"
    out

}



#' Prints a coverage Object
#'
#' Prints a coverage object
#'
#' @param x The coverage object.
#' @param \ldots ignored
#' @method print coverage
#' @export
print.coverage <- function(x, ...){

    nots <- sum(x[["not"]], na.rm = TRUE)
    covs <- sum(x[["covered"]], na.rm = TRUE)
    perc <- round(x[["coverage"]], 2)

    cat(sprintf("Coverage:    %s%%\n", perc))
    cat(sprintf("Coverered:   %s\n", covs))
    cat(sprintf("Not Covered: %s\n", nots))
}


