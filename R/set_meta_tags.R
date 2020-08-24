#' Set Meta Tags Attribute on a \code{term_count}/\code{token_count} Object
#'
#' Set the \code{metatags} attribute on a \code{term_count}/\code{token_count}
#' object.
#'
#' @param x A \code{term_count}/\code{token_count} object.
#' @param tags A data.frame with a \code{'tag'} column corresponding to the tag
#' columns in the \code{term_count}/\code{token_count} object.  Can also take a
#' named list but only allows for tags and meta tags where as the number of tag
#' levels is unrestricted if passing a data.frame.
#' @param \ldots ignored.
#' @return Returns a \code{term_count}/\code{token_count} object with the
#' \code{metatags} attribute set.
#' @export
#' @examples
#' token_list2 <- list(
#'     list(
#'         person = c('sam', 'i')
#'     ),
#'     list(
#'         place = c('here', 'house'),
#'         thing = c('boat', 'fox', 'rain', 'mouse', 'box', 'eggs', 'ham')
#'     ),
#'     list(
#'         no_like = c('not like'),
#'         thing = c('train', 'goat'),
#'         other = c('in')
#'     ),
#'     list(
#'         other = 'i'
#'     )
#' )
#'
#'
#' x <- token_count(sam_i_am, grouping.var = TRUE, token.list = token_list2)
#' attributes(x)[['metatags']]
#'
#' tag_list <- list(
#'     noun = c('person', 'place', 'thing'),
#'     odd_ones = c('other', 'no_like')
#' )
#'
#' library(textshape)
#'
#' tag_df <- tidy_list(list(
#'     noun = c('person', 'place', 'thing'),
#'     odd_ones = c('other', 'no_like')
#' ), 'meta', 'tag')[, 2:1]
#'
#' x2 <- set_meta_tags(x, tag_list)
#' attributes(x2)[['metatags']]
#'
#' x3 <- set_meta_tags(x, tag_df)
#' attributes(x3)[['metatags']]
set_meta_tags <- function(x, tags, ...){

    UseMethod('set_meta_tags', tags)
}


#' @export
#' @rdname set_meta_tags
#' @method set_meta_tags data.frame
set_meta_tags.data.frame <- function(x, tags, ...){

    return(check_set_tags(x, tags, ...) )

}



#' @export
#' @rdname set_meta_tags
#' @method set_meta_tags list
set_meta_tags.list <- function(x, tags, ...){

    tags <- termlist2termdf(tags)

    set_meta_tags(x, tags, ...)

}

## Helpers

check_set_tags <- function(x, tags, ...){

    validate_term_count(x)
    stopifnot(ncol(tags) > 1)
    cls <- colnames(tags)
    tags <- tibble::tibble(tags)[c('tag', cls[!cls %in% 'tag'])]
    #tags[] <- lapply(tags, as.character)

    type <- ifelse('token_count' %in% class(x), 'token', 'term')

    if (!'tag' %in% colnames(tags)) {
        stop(sprintf(
            '`tags` must contain a column named `tag` corresponding to the tag columns in the %s_count object.', type
        ), call. = FALSE)
    }

    if (type == 'token') {
        trms <- attributes(x)[['token.vars']]
    } else {
        trms <- attributes(x)[['term.vars']]
    }

    mtchs <- trms %in% tags[['tag']]

    if (!all(mtchs)) {
        stop(sprintf(
            'The following tags were not found in the %s_count object: \n\n%s\n',
            type,
            paste(paste0('    -', trms[!mtchs]), collapse = '\n')
        ), call. = FALSE)
    }

    tags <- tags[stats::na.omit(match(tags[['tag']], trms)), ]

    attributes(x)[['metatags']] <- tags

    x
}


termlist2termdf <- function(x, ...){
    textshape::tidy_list(x, 'meta', 'tag')[, 2:1]
}


