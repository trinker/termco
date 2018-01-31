#' Get Tag/Group Columns
#'
#' Convenience functions to grab just the tag or grouping variable columns from
#' a \code{term_count} object.
#'
#' @param x A \code{term_count} object.
#' @param \ldots ignored.
#' @return Returns a \code{tibble} frame of just terms or grouping variables.
#' @export
#' @rdname tag_cols
#' @examples
#' tag_cols(markers)
#' group_cols(markers)
tag_cols <- function(x, ...){

    terms <- ifelse(inherits(x, 'token_count'), "token.vars", "term.vars")
    type <- ifelse(inherits(x, 'token_count'), "token", "term")

    y <- validate_term_count(x, FALSE)
    if (!isTRUE(y)) stop(paste0('`x` does not appear to be a valid `', type, '_count` object.  Was the object altered after creation?'))
    x[unlist(attributes(x)[[terms]])]

}

#' @export
#' @rdname tag_cols
group_cols <- function(x, ...){

    type <- ifelse(inherits(x, 'token_count'), "token", "term")

    y <- validate_term_count(x, FALSE)
    if (!isTRUE(y)) stop(paste0('`x` does not appear to be a valid `', type, '_count` object.  Was the object altered after creation?'))
    x[unlist(attributes(x)[['group.vars']])]

}


