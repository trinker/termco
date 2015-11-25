#' Set Pretty/Count Printing of a term_count Object
#'
#' \code{as_count} - Set the pretty printing of a \code{term_count} object.
#' Either print pretty as a combination of count and percent/proportion or as
#' just counts.
#'
#' @param x A \code{term_count} object.
#' @param value logical.  If \code{TRUE} the object will attempt to be printed
#' pretty.
#' @return Returns a \code{term_count} with the \code{pretty}
#' \code{\link[base]{attributes}} set.
#' @keywords printing pretty count
#' @rdname as_count
#' @export
#' @details Note that pretty printing can be turned off globally by setting
#' \code{options(termco_pretty = FALSE)}.
#' @seealso \code{\link[termco]{as_count}}
#' @examples
#' out <- as_count(markers, FALSE)
#' out
#' as_count(out) <- TRUE
#' out
as_count <- function (x, value = FALSE){
    validate_term_count(x)
    attributes(x)[["pretty"]] <- value
    x
}

#' Set Pretty/Count Printing of a term_count Object
#'
#' \code{pretty<-} - Set the pretty/count printing of a \code{term_count} object.
#'
#' @rdname as_count
#' @export
`as_count<-` <- function(x, value){
    validate_term_count(x)
    attributes(x)[["pretty"]] <- value
    x
}


