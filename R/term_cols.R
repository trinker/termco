#' Get Term/Group Columns
#'
#' Convenience functions to grab just the term or grouping variable columns from
#' a \code{term_count} object.
#'
#' @param x A \code{term_count} object.
#' @param \ldots ignored.
#' @return Returns a \code{tibble} frame of just terms or grouping variables.
#' @export
#' @rdname term_cols
#' @examples
#' term_cols(markers)
#' group_cols(markers)
term_cols <- function(x, ...){

    y <- validate_term_count(x, FALSE)
    if (!isTRUE(y)) stop('`x` does not appear to be a valid `term_count` object.  Was the object altered after creation?')
    x[unlist(attributes(x)[['term.vars']])]

}

#' @export
#' @rdname term_cols
group_cols <- function(x, ...){

    y <- validate_term_count(x, FALSE)
    if (!isTRUE(y)) stop('`x` does not appear to be a valid `term_count` object.  Was the object altered after creation?')
    x[unlist(attributes(x)[['group.vars']])]

}





