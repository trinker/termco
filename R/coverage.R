#' Coverage for Various Objects
#'
#' @description \code{coverage} - Get coverage of a logical vector, \code{term_count}, or \code{search_term} object.
#'
#' @param x A logical vector, \code{termc_count}, or \code{search_term} object.
#' @param \ldots Ignored.
#' @description \code{coverage.term_count} - Extract coverage information from a \code{term_count}
#' object including the percentage of rows that sum to zero as well as the
#' location of non-covered rows for easy extraction.
#'
#' @export
#' @return \code{termc_count} - Returns a proportion of elements covered by the search.
#' @return \code{coverage.term_count} - Returns a list:
#' \item{not}{A logical vector of all rows not covered (row sums equal zero)}
#' \item{covered}{A logical vector of all rows covered (row sums greater than zero)}
#' \item{coverage}{The percentage rate of \deqn{\frac{covered}{not + covered}}{covered/(not + covered)}}
#' \item{n_covered}{The row sums of the unique terms}
#' \item{total_terms}{The row sums of the terms}
#' @export
#' @keywords coverage
#' @examples
#' coverage(sample(c(TRUE, FALSE), 1000, TRUE))
#'
#' data(presidential_debates_2012)
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
#' (markers2 <- with(presidential_debates_2012,
#'     term_count(dialogue, TRUE, discoure_markers)
#' ))
#'
#' coverage(markers2)
#'
#' presidential_debates_2012[coverage(markers2)$not, "dialogue"] %>%
#'    c()
coverage <- function(x, ...) {
    UseMethod("coverage")
}


#' @export
#' @method coverage default
coverage.default <- function(x, ...) {
     sum(x)/length(x)
}


#' @export
#' @method coverage search_term
coverage.search_term <- function(x, ...) {
    attributes(x)[["coverage"]]
}







#' @export
#' @method coverage term_count
coverage.term_count <- function(x, ...){

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

    nots <- pn(sum(x[["not"]], na.rm = TRUE))
    covs <- pn(sum(x[["covered"]], na.rm = TRUE))
    perc <- pp(x[["coverage"]]/100, 1)

    mn <- max(nchar(c(nots, covs, perc)))
    params <- paste(
        sapply(nchar(c(nots, covs, perc)), function(x) paste(rep(" ", mn - x), collapse="")),
        c(nots, covs, perc),
        sep=""
    )

    cat(sprintf("Coverage    : %s\n", params[3]))
    cat(sprintf("Coverered   : %s\n", params[2]))
    cat(sprintf("Not Covered : %s\n", params[1]))
}

