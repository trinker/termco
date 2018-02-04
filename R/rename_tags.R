#' Rename a term_count Object's Term Columns
#'
#' Safely rename a \code{term_count} object's tag columns and attributes.
#'
#' @param x A \code{term_count} object.
#' @param old A vector of the current names.
#' @param new A vector of new names corresponding to the order of \code{old} names.
#' @return Returns a renamed \code{term_count} object.
#' @export
#' @examples
#' data(presidential_debates_2012)
#'
#' discoure_markers <- list(
#'     response_cries = c("\\boh", "\\bah", "\\baha", "\\bouch", "yuk"),
#'     back_channels = c("uh[- ]huh", "uhuh", "yeah"),
#'     summons = "hey",
#'     justification = "because"
#' )
#'
#' (markers <- with(presidential_debates_2012,
#'     term_count(dialogue, list(person, time), discoure_markers)
#' ))
#'
#'
#' rename_tags(markers, old = c('summons', 'back_channels'), new = c('s', 'bcs'))
#' rename_tags(markers, old = c('person'), new = c('people'))
#' rename_tags(markers, old = c('person', 'back_channels', 'summons'), new = c('people', 'bcs', 's'))
#' attributes(rename_tags(markers, old = c('back_channels', 'summons'), new = c('bcs', 's')))
#' 
#' #' ## Token Counts
#' token_list2 <- list(
#'     list(
#'         noun__person = c('sam', 'i')
#'     ),
#'     list(
#'         noun__place = c('here', 'house'),
#'         noun__thing = c('boat', 'fox', 'rain', 'mouse', 'box', 'eggs', 'ham')
#'     ),
#'     list(
#'         feeling__no_like = c('not like'),
#'         noun__thing = c('train', 'goat'),
#'         other__other = c('in')
#'     ),
#'     list(
#'         other__other = 'i'
#'     )
#' )
#'
#'
#' x <- token_count(sam_i_am, grouping.var = TRUE, token.list = token_list2)
#' 
#' \dontrun{
#' ## Strips meta tags
#' x %>%
#'     rename_tags(
#'         c('noun__person', 'noun__place', 'feeling__no_like'), 
#'         c('human', 'ten_20', 'dislike')
#'     )
#' }
rename_tags <- function(x, old, new){

    type <- term_token_validate(x)

    attr_x <- attributes(x)
    
    ## ensure `old` names are actually in the counts object
    vars <- unlist(attr_x[c("group.vars", type)])
    if(any(!old %in% vars)){
        stop(
            "The following `old` names were not found:\n", 
            paste(old[!old %in% vars], collapse=", "),
            call. = FALSE
        )
    }

    y <- data.table::copy(x)
    data.table::setnames(y, old, new)

    if (any(old %in% attr_x[[type]])){
        matches <- match(attributes(y)[[type]], old)
        attributes(y)[[type]][!is.na(matches)] <- new[matches[!is.na(matches)]]
    }

    if(any(old %in% attr_x[["group.vars"]])){
        matches <- match(attributes(y)[["group.vars"]], old)
        attributes(y)[["group.vars"]][!is.na(matches)] <- new[matches[!is.na(matches)]]
    }

    if (!is.null(attr_x[["metatags"]])){    
        warning(paste0(
            '\'metatags\' attribute has been stripped because mapping is unknown ',
            'after collapsing tags.\nManually, set updated tags via `set_meta_tags` function.'
        ), call. = FALSE)    
    }
    
    attributes(y)[["metatags"]] <- NULL    
    
    
    y
}







