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

     out <- x[tag_names(x)]
     class(out) <- class(x)[!grepl('(term|token)_count', class(out))]
     out

}

# tag_cols <- function(x, ...){
# 
#      out <- x[tag_names(x)]
#      classs(out) <- 
#      out
# 
# }

#' @export
#' @rdname tag_cols
tag_names <- function(x, ...){

    terms <- ifelse(inherits(x, "token_count"), "token.vars",
        "term.vars")
    type <- ifelse(inherits(x, "token_count"), "token", "term")
    y <- validate_term_count(x, FALSE)
    if (!isTRUE(y)) {
        stop(paste0("`x` does not appear to be a valid `", type,
            "_count` object.  Was the object altered after creation?"))
    }
    unlist(attributes(x)[[terms]])

}

#' @export
#' @rdname tag_cols
group_cols <- function(x, ...){

    out <- x[group_names(x)]
    class(out) <- class(x)[!grepl('(term|token)_count', class(out))]
    out
    
}


# group_cols <- function(x, ...){
# 
#     x[group_names(x)]
# 
# }

#' @export
#' @rdname tag_cols
group_names <- function(x, ...){

    type <- ifelse(inherits(x, 'token_count'), "token", "term")

    y <- validate_term_count(x, FALSE)
    if (!isTRUE(y)) {
        stop(paste0('`x` does not appear to be a valid `', type, 
            '_count` object.  Was the object altered after creation?'))
    }
    
    unlist(attributes(x)[['group.vars']])

}

