#' Unnest a Nested Term List
#'
#' Term lists can be stored as lists within a list for use in \code{termc_count}
#' in a hierarchical fashion.  This structure is not always useful and can be
#' taken to a single nest via \code{unnest_term_list}.  The function detects if
#' the \code{term.list} is nested or not and then unnests only if needed, thus
#' allowing it to be safely used on both nested and unnested \code{term.list}s.
#'
#' @param term.list A list of named character vectors.  `code{term_count} can
#' be used in a hierarchical fashion as well; that is a list of regexes can be
#' passed and counted and then a second (or more) pass can be taken wit a new
#' set of regexes on only those rows/text elements that were left untagged
#' (count \code{\link[base]{rowSums}} is zero).  This is accomplished by passing
#' a \code{\link[base]{list}} of \code{\link[base]{list}}s of regexes.
#' See \bold{Examples} for the \strong{hierarchical terms} section for a
#' demonstration.
#' @param \ldots ignored.
#' @return Returns a \code{\link[base]{list}} of one level.
#' @keywords term.list unnest
#' @export
#' @examples
#' x <- list(
#'     a = setNames(as.list(LETTERS[1:5]),LETTERS[1:5]),
#'     b = setNames(as.list(LETTERS[6:11]), LETTERS[6:11])
#' )
#' y <- list(a=LETTERS[1:11])
#' unnest_term_list(x)
#' unnest_term_list(y)
unnest_term_list <-
function (term.list, ...) {
    if (is.list(term.list[[1]]) && length(term.list) > 1 && all(sapply(term.list,
        is.list))) {
        nms <- unlist(lapply(term.list, names))
        term.list <- unlist(term.list, recursive = FALSE)
        term.list <- stats::setNames(term.list, nms)
    }
    term.list
}

