#' Read-In/Write-Out Term List
#'
#' \code{read_term_list} - Read-in and format a term list from an external file.
#' \code{read_term_list} collapses the terms within categories by default.  To
#' negate this behavior use \code{source_term_list}.
#'
#' @param path Path to external term list.
#' @param indices Indices of the elements to retain (used to take part of a
#' term list).
#' @param term.list A term list object that can be passed rather than an
#' external file.
#' @param \ldots ignored.
#' @return Returns a formatted term list.
#' @rdname read_term_list
#' @export
#' @examples
#' good_cats <- system.file("termco_docs/categories.R", package = "termco")
#' bad_cats <- system.file("termco_docs/mal_categories.R", package = "termco")
#'
#' read_term_list(good_cats)
#' source_term_list(good_cats)
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
#'
#' discoure_markers <- list(
#'     response_cries = c("\\boh", "\\bah", "\\baha", "\\bouch", "yuk"),
#'     back_channels = c("uh[- ]huh", "uhuh", "yeah"),
#'     summons = "hey",
#'     justification = "because"
#' )
#'
#' ## writing to the console (not that useful)
#' write_term_list(discoure_markers)
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
#' ## writing to the console (not that useful)
#' write_term_list(trpl_list)
#'
#' ## Writing to an external file
#' temp <- tempdir()
#'
#' write_term_list(discoure_markers, path = file.path(temp, 'categories.R'))
#' read_term_list(path = file.path(temp, 'categories.R'))
#' source_term_list(path = file.path(temp, 'categories.R'))
#'
#' write_term_list(trpl_list, path = file.path(temp, 'categories2.R'))
#' read_term_list(path = file.path(temp, 'categories2.R'))
#' source_term_list(path = file.path(temp, 'categories2.R'))
#'
#' ## Writing term list for non-R .json others to use:
#' \dontrun{
#' my_term_list %>%
#'     jsonlite::toJSON(pretty=TRUE) %>%
#'     stringi::stri_unescape_unicode() %>%
#'     cat(file = 'testing.json')
#' }
read_term_list <- function(path, indices = NULL, term.list, ...){

    obj <- 'unspecified_termco_obj1234'

    dots <- list(...)
    if (!is.null(dots[['G']])) obj <- dots[['G']]
    collapse <- is.null(dots[['collapse']]) | isTRUE(dots[['collapse']])

    if (missing(term.list)) {
        ## ensure path exists
        stopifnot(file.exists(path))

        ## read in categories file
        cats <- source(path)[[1]]
    } else {

        if (methods::is(term.list, 'term_list')) return(term.list)

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
                tryCatch(term_lister_check(x, G = obj, collapse = collapse), warning=function(w) w)
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
                suppressWarnings(term_lister_check(x, G = obj, collapse = collapse))
            })

            open_or_list <- unlist(lapply(cats, function(y) {
                open_or <- search_open_or(y)
                if (all(!open_or)) return(NA)
                paste(names(y)[open_or], collapse = ', ')
            }))

            if (any(!is.na(open_or_list))) {
                offending_open_or_vect <- paste(paste0("Tier ", seq_along(open_or_list), '. ',open_or_list)[!is.na(open_or_list)], collapse = '\n')
                warning(paste0('Open or statement [(i.e., `|)` unescaped pipe followed by a closing\n  group character] found in the following tiers & categories:\n\n', offending_open_or_vect))
            }

        },
        termco_unnested = {
            cats <- term_lister_check(cats, obj, collapse = collapse)
            open_or <- unlist(lapply(cats, search_open_or))
            if (any(open_or)) {
                offending_open_or <- paste(names(open_or)[open_or], collapse = ', ')
                warning(paste0('Open or statement [(i.e., `|)` unescaped pipe followed by a closing\n  group character] found in the following categories:\n\n', offending_open_or))
            }

        }
    )

    if (!is.null(indices)) {
        warning('`indices` set: dropping `term_list` elements...\n\nUse `indices = NULL` to keep all elements')
        cats <- cats[indices]
    }

    if (isTRUE(collapse)) class(cats) <- c('term_list', type)
    return(cats)

}

#' @rdname read_term_list
#' @export
source_term_list <- function(path, indices = NULL, ...){

    read_term_list(path = path, collapse = FALSE, indices = indices, ...)

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
# path <- 'comment_categories.R'
# pp <- read_term_list(path)
# term_lister_empty_hierarchy_check <- termco:::term_lister_empty_hierarchy_check
# term_lister_check <- termco:::term_lister_check
# rm_class <- termco:::rm_class




## x <- "(((it was|what a).*(class|course|labs?\\b|))|((class|course|labs?\\b|).*(it was|what a)))"
search_open_or <- function(x, ...){

    grepl('(?<!\\\\)\\|\\)', x, perl = TRUE)

}

## json write double backslashes
write_model <- function(term.list, path, ...) {

    df <- textshape::tidy_list(lapply(term.list, textshape::tidy_list, 'tag', 'regex'), 'iteration')

    file.type <- tolower(gsub('(^.+\\.)([A-Za-z]+$)', '\\2', path))

    switch(file.type,
        csv = {utils::write.csv(df, file = path, row.names=FALSE)},
        #txt = {},
        json = {cat(
                stringi::stri_unescape_unicode(
                    jsonlite::toJSON(
                        term.list,
                        pretty=TRUE
                    )
                ),
                file = path
            )
        },
        stop('`file.type` not supported')
    )
}

#' Title
#'
#' \code{write_term_list} - Write-out a term list out to a file.
#'
#' @rdname read_term_list
#' @export
write_term_list <- function(term.list, path = "", ...){

    stopifnot(is.list(term.list))
    stopifnot(!is.list(term.list[[1]][[1]]))

    if (!is.list(term.list[[1]])) {

        cat(sprintf(list1, paste(unlist(unname(Map(function(x, y){
            x <- stringi::stri_escape_unicode(x)
            sprintf(vects1, y, paste(shQuote(x), collapse = ', '))
        }, term.list, names(term.list)))), collapse = ',\n')), file = path)

    } else {

        cat(sprintf(list1, paste(unlist(lapply(term.list, function(z){
            a <- unlist(unname(Map(function(x, y){

                x <- stringi::stri_escape_unicode(x)
                sprintf(vects2, y, paste(shQuote(x), collapse = ', '))
            }, z, names(z))))

            sprintf(list2, paste(a, collapse = ',\n'))
        })), collapse = ',\n')), file = path)

    }

}



list1 <- 'list(\n%s\n)\n'
vects1 <- '    `%s` = c(%s)'

list2 <- '    list(\n%s\n    )'
vects2 <- '        `%s` = c(%s)'




