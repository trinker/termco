#' Read-In Term List
#'
#' Read-in a term list from an external file.
#'
#' @param file Path to external term list.
#' @param indices Indices of the elements to retain (used to take part of a
#' term list).
#' @param term.list A term list object that can be passed rather than an
#' external file.
#' @param \ldots ignored.
#' @return Returns a formatted term list.
#' @export
#' @examples
#' good_cats <- system.file("termco_docs/categories.R", package = "termco")
#' bad_cats <- system.file("termco_docs/mal_categories.R", package = "termco")
#'
#' read_term_list(good_cats)
#'
#' \dontrun{
#' ## Throws warnings
#' read_term_list(bad_cats)
#'
#' ## Using term list object
#' my_term_list <- list(
#' ## Tier 1
#'     list(
#'         Response_Cries = c("\\boh", "\\bah", "\\baha", "\\bouch", "yuk"),
#'         Back_Channels = c("uh[- ]huh", "uhuh", "yeah"),
#'         Summons = "hey",
#'         Justification = "because"
#'     ),
#'
#'  ## Tier 2
#'     list(
#'         Summons = 'the',
#'         Back_Channels = c('\\ber+\\b', 'hm+'),
#'         Empty = c('^\\s*$')
#'     ),
#'
#'  ## Tier 3
#'     list(
#'         Summons = c(),
#'         Back_Channels = c()
#'     ),
#'
#' ## Tier 4
#'     list(
#'         Summons = 'hey you',
#'         Justification = 'ed\\s',
#'         Exclamation = c('\\!$', '^[^w]{0,5}wow+.{0,4}'),
#'         Empty = c()
#'     )
#' )
#'
#' read_term_list(term.list = my_term_list)
#' }
read_term_list <- function(file, indices = NULL, term.list, ...){

    obj <- 'unspecified_termco_obj1234'

    if (missing(term.list)) {
        ## ensure file exists
        stopifnot(file.exists(file))

        ## read in categories file
        cats <- source(file)[[1]]
    } else {
        cats <- term.list
    }

    ## determine if hierarchical
    type <- ifelse(
        is.list(cats[[1]]) && length(cats) > 1 && all(sapply(cats, is.list)),
        'termco_nested',
        'termco_unnested'
    )

    switch(type,
        termco_nested = {

            ## check for empty tiers
            cats <- term_lister_empty_hierarchy_check(cats)

            ## grab the warnings for later printing
            first_pass <- lapply(cats, function(x) {
                tryCatch(term_lister_check(x, G = obj), warning=function(w) w)
            })

            locs <- unlist(lapply(first_pass, inherits, what = 'simpleWarning'))
            main <- 'The categories within the following hierarchies (tiers) contained no regular expressions and were dropped:\n\n'

            if (any(locs)) {

                tiers <- which(locs)

                messages <- paste(paste0(
                    'Tier ', tiers, ": ",
                     paste0('\n', gsub('^.+?\\n\\n|\\n\\n$', '', unlist(lapply(first_pass[locs], `[[`, 'message')))),
                     '\n'
                ), collapse = '\n')

                warning(paste0(main, messages))

            }


            ## actually run the term list through `term_lister_check`
            cats <- lapply(cats, function(x) {
                suppressWarnings(term_lister_check(x, G = obj))
            })

        },
        termco_unnested = {
            cats <- term_lister_check(cats, obj)
        }
    )

    if (!is.null(indices)) {
        warning('`indices` set: dropping `term_list` elements...\n\nUse `indices = NULL` to keep all elements')
        cats <- cats[indices]
    }

    class(cats) <- c('term_list', type)
    return(cats)

}

#' Prints a term_list Object
#'
#' Prints a term_list object
#'
#' @param x A \code{term_list} object
#' @param \ldots ignored
#' @method print term_list
#' @export
print.term_list <- function(x, ...){
    x <- rm_class(x, 'term_list')
    print(x)
}

#' Prints a termco_unnested Object
#'
#' Prints a termco_unnested object
#'
#' @param x A \code{termco_unnested} object
#' @param \ldots ignored
#' @method print termco_unnested
#' @export
print.termco_unnested <- function(x, ...){
    x <- rm_class(x, 'termco_unnested')
    print(x)
}

#' Prints a termco_nested Object
#'
#' Prints a termco_nested object
#'
#' @param x A \code{termco_nested} object
#' @param \ldots ignored
#' @method print termco_nested
#' @export
print.termco_nested <- function(x, ...){
    x <- rm_class(x, 'termco_nested')
    print(x)
}

## handle if not hierarchical
## can turn off this checking in term_count if passed through
## make sure token_count is not broken and has same features implemented
term_list_summary_stats <- function(x){

    UseMethod('term_list_summary_stats')

}

term_list_summary_stats.termco_nested <- function(x, ...){


    ## per category/tier (print as table)
    list_n_char <- lapply(x, nchar)
    nms <- lapply(list_n_char, names)

    list_n <- lengths(x)
    list_n_lookarounds <- Map(function(x, y) {stats::setNames(x, y)},
        lapply(x, stringi::stri_count_regex, pattern = '\\(\\?<?(=|!)'), nms)

    list_n_quantifiers_0_1_greedy <- Map(function(x, y) {stats::setNames(x, y)},
        lapply(x, stringi::stri_count_regex, pattern = '(?<!\\\\)(\\?)[^?]'), nms)
    list_n_quantifiers_0_more_greedy <- Map(function(x, y) {stats::setNames(x, y)},
        lapply(x, stringi::stri_count_regex, pattern = '(?<!\\\\)(\\*)[^?]'), nms)
    list_n_quantifiers_1_more_greedy <- Map(function(x, y) {stats::setNames(x, y)},
        lapply(x, stringi::stri_count_regex, pattern = '(?<!\\\\)(\\+)[^?]'), nms)

    list_n_quantifiers_0_1_lazy <- Map(function(x, y) {stats::setNames(x, y)},
        lapply(x, stringi::stri_count_regex, pattern = '(?<!\\\\)(\\?)[?]'), nms)
    list_n_quantifiers_0_more_lazy <- Map(function(x, y) {stats::setNames(x, y)},
        lapply(x, stringi::stri_count_regex, pattern = '(?<!\\\\)(\\*)[?]'), nms)
    list_n_quantifiers_1_more_lazy <- Map(function(x, y) {stats::setNames(x, y)},
        lapply(x, stringi::stri_count_regex, pattern = '(?<!\\\\)(\\+)[?]'), nms)

    list_n_quantifiers_bounded <- Map(function(x, y) {stats::setNames(x, y)},
        lapply(x, stringi::stri_count_regex, pattern = '(?<!\\\\)(\\{)'), nms)
    list_n_pipes <- Map(function(x, y) {stats::setNames(x, y)},
        lapply(x, stringi::stri_count_regex, pattern = '(?<!\\))\\|'), nms)
    list_n_quantified_dots <- Map(function(x, y) {stats::setNames(x, y)},
        lapply(x, stringi::stri_count_regex, pattern = '(?<!\\\\)\\.(\\{|\\*|\\+|\\?)'), nms)
    list_n_unbounded_dots <- Map(function(x, y) {stats::setNames(x, y)},
        lapply(x, stringi::stri_count_regex, pattern = '(?<!\\\\)\\.(\\*|\\+)'), nms)

    list_n_char_classes <- Map(function(x, y) {stats::setNames(x, y)},
        lapply(x, stringi::stri_count_regex, pattern = '(?<!\\\\)(\\[)[^^]'), nms)
    list_n_negated_char_classes <- Map(function(x, y) {stats::setNames(x, y)},
        lapply(x, stringi::stri_count_regex, pattern = '(?<!\\\\)(\\[)\\^'), nms)

    list_n_boundaries <- Map(function(x, y) {stats::setNames(x, y)},
        lapply(x, stringi::stri_count_regex, pattern = '(?<=\\\\)b'), nms)
    list_n_anchors <- Map(function(x, y) {stats::setNames(x, y)},
        lapply(x, stringi::stri_count_regex, pattern = '(([^\\[\\\\]|^)(\\^|\\$))'), nms)


    ## meta
    n_tiers <- length(list_n)
    n_iterations <- sum(list_n)
    n_categories <- length(unique(unlist(lapply(x, names))))
    n_char <- sum(unlist(list_n_char))
    n_greedy_quantifiers <- sum(unlist(list_n_quantifiers_0_1_greedy, list_n_quantifiers_0_more_greedy, list_n_quantifiers_1_more_greedy))
    n_unbounded_dot <- sum(unlist(list_n_unbounded_dots))

    ## tiers
    n_categories_tiered <- lengths(lapply(x, names))
    n_char_tiered <- unlist(lapply(list_n_char, sum))
    n_greedy_quantifiers_tiered <- unlist(Map(function(x, y, z){sum(unlist(x, y, z))},
        list_n_quantifiers_0_1_greedy, list_n_quantifiers_0_more_greedy, list_n_quantifiers_1_more_greedy))
    n_unbounded_dot_tiered <- unlist(lapply(list_n_unbounded_dots, sum))


}


## https://www.loggly.com/blog/five-invaluable-techniques-to-improve-regex-performance/
## https://docs.bmc.com/docs/display/DISCO90/Writing+efficient+regular+expressions

# x <- pp
# term_list_summary_stats(pp)
#
# class(pp)
#
# file <- 'comment_categories.R'
# pp <- read_term_list(file)
# term_lister_empty_hierarchy_check <- termco:::term_lister_empty_hierarchy_check
# term_lister_check <- termco:::term_lister_check
# rm_class <- termco:::rm_class

