#' Test Regex Validity
#'
#' Test an atomic vector, list, or term list of regexes for validity.
#'
#' @param regex An atomic vector, list, or term list of regexes to test for
#' validity.
#' @param stringi logical.  If \code{TRUE} \pkg{stringi}'s implementation of reguar
#' expressions is utilized for testing.  This allows for more flexible use of
#' regular expressions, otherwise base R is used for validation.
#' @param \ldots ignored.
#' @return Returns x back with warnings.  Note that if a list is passed it is
#' coerced to a \code{term_list}.
#' @export
#' @examples
#' \dontrun{
#' ## atomic vector
#' test_regex(c('.', 'a'))  ## works
#' test_regex(c('.', '(a')) ## warning
#'
#' ## list
#' test_regex(list(c('.', 'a'), 'd'))   ## works
#' test_regex(list(c('.', 'a(('), 'd')) ## warning
#'
#' discoure_markers <- list(
#'     response_cries = c("\\boh", "\\bah", "\\baha", "\\bouch", "yuk"),
#'     back_channels = c("uh[- ]huh", "uhuh", "yeah"),
#'     summons = "hey",
#'     justification = "because"
#' )
#'
#' trmlst_unnested <- read_term_list(term.list = discoure_markers, collapse = FALSE)
#' test_regex(trmlst_unnested) ## works
#' trmlst_unnested[[1]][1] <- 'bad1('
#' trmlst_unnested[[1]][3] <- 'bad2('
#' trmlst_unnested[[3]][1] <- 'bad3('
#' test_regex(trmlst_unnested) ## warning
#'
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
#' trmlst_nested <- read_term_list(term.list = trpl_list, collapse = FALSE)
#' test_regex(trmlst_nested) ## works
#' trmlst_nested[[1]][[1]][1] <- 'bad1('
#' trmlst_nested[[1]][[1]][3] <- 'bad2('
#' trmlst_nested[[1]][[3]][1] <- 'bad3('
#' trmlst_nested[[3]][[1]][1] <- 'bad4('
#' test_regex(trmlst_nested) ## warning
#'
#' }
test_regex <- function(regex, stringi = TRUE, ...){
    UseMethod('test_regex')
}

#' @export
#' @method test_regex character
test_regex.character <- function(regex, stringi = TRUE, ...){

    out <- unlist(lapply(regex, is_valid_regex, stringi = stringi))

    if (sum(!out) > 0) {

        warning(
            sprintf(
                'The following elements were invalid regex(es):\n\n%s',
                paste(sprintf('    %s. "%s:"', which(!out), regex[!out]), collapse = '\n')
            ),
            call. = FALSE
        )
    }

    regex

}

#' @export
#' @method test_regex term_list
test_regex.term_list <- function(regex, stringi = TRUE, ...){
    test_term_list_regex_h(regex, stringi = stringi, ...)
}

#' @export
#' @method test_regex termco_unnested
test_regex.termco_unnested <- function(regex, stringi = TRUE, ...){
    test_term_list_regex_h(regex, stringi = stringi, ...)
}

#' @export
#' @method test_regex termco_nested
test_regex.termco_nested <- function(regex, stringi = TRUE, ...){
    test_term_list_regex_h(regex, stringi = stringi, ...)
}

#' @export
#' @method test_regex list
test_regex.list <- function(regex, stringi = TRUE, ...){
    regex <- as_term_list(regex, collapse = FALSE, test.regex = FALSE)
    test_term_list_regex_h(regex, stringi = stringi, ...)
}


is_valid_regex <- function (regex, stringi = TRUE, ...) {

    if (stringi){
        out <- suppressWarnings(try(stringi::stri_replace_all_regex("hello", regex, ""),
        silent = TRUE))
    } else {
        out <- suppressWarnings(try(gsub(regex, "", "hello", perl = TRUE),
        silent = TRUE))
    }
    !inherits(out, "try-error")
}


test_term_list_regex_h <- function(cats, stringi = TRUE, ...){

    if ("termco_nested" %in% class(cats)) {

        outs <- lapply(cats, function(y) lapply(y, is_valid_regex, stringi = stringi))
        tiers <- sapply(outs, function(x) sum(!unlist(x)) > 0)
        if (sum(tiers) == 0) return(cats)

        tags <- lapply(outs[tiers], function(x){

            sprintf('    %s. %s', which(!unlist(x)), names(x)[!unlist(x)])

        })

        regs <- lapply(cats[tiers], function(y) lapply(y, function(z) {
            locs <- !unlist(lapply(z, is_valid_regex, stringi = stringi))
            m <- z[locs]
            if (length(m) == 0) return(NULL)
            paste(sprintf('        (%s) "%s"', which(locs), m), collapse = '\n')
        }))

        tagregs <- unlist(Map(function(x, y){
            paste(paste(x, unname(unlist(y)), sep = '\n'), collapse = '\n\n')
        }, tags, regs))

        mess <- paste(
            sprintf(
                'Tier %s has invalid regex(es) within the following categories: \n\n%s',
                which(tiers),
                tagregs
            ),
            collapse = '\n\n'
        )
        warning(mess, call. = FALSE)

        return(cats)

    } else {

        outs <- lapply(cats, function(y) lapply(y, is_valid_regex, stringi = stringi))
        vects <- !sapply(outs, function(x) sum(!unlist(x)) > 0)

        if (sum(!vects) == 0) return(cats)

        nms <- names(outs)
        if (is.null(nms)) {
            warning('Unnamed vectors found.', call. = FALSE)
            nms <- paste('Unamed Vector', seq_along(outs))
        }

        tags <- sprintf('    %s. %s\n', which(!unlist(vects)), nms[!unlist(vects)])


        regs <- lapply(cats[!vects], function(z) {
            locs <- !unlist(lapply(z, is_valid_regex, stringi = stringi))
            m <- z[locs]
            if (length(m) == 0) return(NULL)
            paste(sprintf('        (%s) "%s"', which(locs), m), collapse = '\n')
        })

        tagregs <- unname(unlist(Map(function(x, y){

            paste(paste(x, unname(unlist(y)), sep = '\n'), collapse = '\n\n')
        }, tags, regs)))


        mess <- sprintf('Invalid regex(es) within the following categories: \n\n%s', paste(tagregs, collapse = '\n\n'))

        warning(mess, call. = FALSE)

        return(cats)

    }
}
