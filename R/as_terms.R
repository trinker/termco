#' Convert a Count Matrix to List of Term Vectors
#'
#' Convert a count matrix to a named list of term vectors.
#'
#' @param x A \code{\link[base]{data.frame}}/\code{\link[base]{matrix}} of counts.
#' @param names A character vector of names to assign to the list.
#' @param \ldots ignored.
#' @return Returns a list of term vectors.
#' @export
#' @examples
#' data(markers)
#' as_terms(markers)
as_terms <- function(x, names = NULL, ...) {
    val <- validate_term_count(x)
    if (isTRUE(val)) {
        if (is.null(names)) names <- paste2(x[, attributes(x)[["group.vars"]], drop=FALSE], ...)
        x <- x[, attributes(x)[["term.vars"]], drop=FALSE]
    } else {
         if (is.null(names)) names <- rownames(x)
    }
    nms <- colnames(x)
	lst <- apply(x, 1, function(y) rep(nms, y))
    if(!is.list(lst)) lst <- lapply(1:ncol(lst), function(i) lst[, i])	
    stats::setNames(lst, nm = names)
}
