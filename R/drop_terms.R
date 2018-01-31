#' Drop Terms from a Term List
#'
#' \code{drop_terms} Usage allows the user to explore/iterate on a term list and
#' drop terms prior prior to \code{term_count} use without manually editing an
#' external term list file.
#'
#' @param x A term list.
#' @param drop.terms A vector of terms to drop or a regex.
#' @param fixed logical.  If \code{FALSE} then \code{drop.terms} may be a regex.
#' @param negate logical.  If \code{TRUE} then the \code{drop.terms} will be kept.
#' @param \ldots If \code{fixed = FALSE} then other terms passed to
#' \code{search_term_which}, otherwise, ignored.
#' @return Returns a term list
#' @rdname drop_terms
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
#' drop_terms_regex(discoure_markers, 'on')
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
#' drop_terms_regex(trpl_list, '[ln]s')
#' keep_terms_regex(trpl_list, '[ln]s')
drop_terms <- function(x, drop.terms, fixed = TRUE, negate = FALSE, ...){

    if (!isTRUE(fixed)){
        drop_terms_regex(x, drop.terms, ...)
    } else {
        drop_terms_fixed(x, drop.terms, ...)
    }
}

#' Drop Terms from a Term List
#'
#' \code{drop_terms_regex} Contol \code{fixed} parameter with function name, in
#'  this case, \code{drop.terms} is matched via regex.
#' @rdname drop_terms
#' @export
drop_terms_regex <- function(x, drop.terms, negate = FALSE, ...){

    ## determine if hierarchical
    type <- ifelse(
        is.list(x[[1]]) && length(x) > 1 && all(sapply(x, is.list)),
        'termco_nested',
        'termco_unnested'
    )

    term_list <- switch(type,

        termco_unnested = {
            cls <- class(x)
            drops <- search_term_which(names(x), term = drop.terms, ...)
            if (!isTRUE(negate)){
                drops <- !drops
            }
            x <- x[drops]
            class(x) <- unique(c(cls, class(x)))
            x
        },

        termco_nested = {
            cls <- class(x)
            x <- lapply(x, function(z) {
                drops <- search_term_which(names(z), term = drop.terms, ...)
                if (!isTRUE(negate)){
                    drops <- !drops
                }
                y <- z[drops]
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

#' Drop Terms from a Term List
#'
#' \code{drop_terms_fixed} Contol \code{fixed} parameter with function name, in
#'  this case, \code{drop.terms} is matched via exactly.
#' @rdname drop_terms
#' @export
drop_terms_fixed <- function(x, drop.terms, negate = FALSE, ...){

    ## determine if hierarchical
    type <- ifelse(
        is.list(x[[1]]) && length(x) > 1 && all(sapply(x, is.list)),
        'termco_nested',
        'termco_unnested'
    )

    term_list <- switch(type,

        termco_unnested = {
            cls <- class(x)
            drops <- names(x) %in% drop.terms
            if (!isTRUE(negate)){
                drops <- !drops
            }
            x <- x[drops]
            class(x) <- unique(c(cls, class(x)))
            x
        },

        termco_nested = {
            cls <- class(x)
            x <- lapply(x, function(z) {
                drops <- names(z) %in% drop.terms
                if (!isTRUE(negate)){
                    drops <- !drops
                }

                y <- z[drops]
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


#' Drop Terms from a Term List
#'
#' \code{keep_terms_regex} Negated version of \code{drop_terms_regex}.
#' @rdname drop_terms
#' @export
keep_terms_regex <- function(x, drop.terms, negate = TRUE, ...){
    drop_terms_regex(x = x, drop.terms = drop.terms, negate = negate, ...)
}


#' Drop Terms from a Term List
#'
#' \code{keep_terms_fixed} Negated version of \code{drop_terms_fixed}.
#' @rdname drop_terms
#' @export
keep_terms_fixed <- function(x, drop.terms, negate = TRUE, ...){
    drop_terms_regex(x = x, drop.terms = drop.terms, negate = negate, ...)
}


#' Drop Terms from a Term List
#'
#' \code{keep_terms} Negated version of \code{drop_terms}.
#' @rdname drop_terms
#' @export
keep_terms <- function(x, drop.terms, fixed = TRUE, negate = TRUE, ...){
    drop_terms(x = x, drop.terms = drop.terms, fixed = fixed, negate = negate, ...)
}



