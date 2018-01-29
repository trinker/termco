#' Drop Terms from a Term List
#'
#' Usage allows the user to explore/iterate on a term list and drop terms prior
#' prior to \code{term_count} use without manually editing an external term list
#' file.
#'
#' @param x A term list.
#' @param drop.terms A vector of terms to drop.
#' @param \ldots ignored.
#' @return Returns a term list
#' @export
#' @examples
#' ## Single level term list
#' discoure_markers <- list(
#'     response_cries = c("\\boh", "\\bah", "\\baha", "\\bouch", "yuk"),
#'     back_channels = c("uh[- ]huh", "uhuh", "yeah"),
#'     summons = "hey",
#'     justification = "because"
#' )
#' drop_terms(discoure_markers, 'response_cries')
#' drop_terms(discoure_markers, c('summons', 'response_cries'))
#'
#' ## Hierarchical term list
#' trpl_list <- list(
#'     list(
#'         response_cries = c("\\boh", "\\bah", "\\baha", "\\bouch", "yuk"),
#'         back_channels = c("uh[- ]huh", "uhuh", "yeah"),
#'         summons = "hey",
#'         justification = "because"
#'     ),
#'     list(summons ='the'),
#'     list(summons = 'it', justification = 'ed\\s')
#' )
#'
#' drop_terms(trpl_list, 'response_cries')
#' drop_terms(trpl_list, c('summons', 'response_cries'))
#' drop_terms(trpl_list, c('summons', 'response_cries', 'justification'))
drop_terms <- function(x, drop.terms, ...){

    ## determine if hierarchical
    type <- ifelse(
        is.list(x[[1]]) && length(x) > 1 && all(sapply(x, is.list)),
        'termco_nested',
        'termco_unnested'
    )

    term_list <- switch(type,

        termco_unnested = {
            cls <- class(x)
            x <- x[!names(x) %in% drop.terms]
            class(x) <- unique(c(cls, class(x)))
            x
        },

        termco_nested = {
            cls <- class(x)
            x <- lapply(x, function(z) {
                y <- z[!names(z) %in% drop.terms]
                if (length(y) == 0) return(NULL)
                y
            })
            x <- x[!sapply(x, is.null)]
            if (length(x) == 1) {
                x <- x[[1]]
            } else {
                class(x) <- unique(c(cls, class(x)))
                x
            }
        },

        stop('Doesn\'t appear to be a term list')

    )

    term_list

}
