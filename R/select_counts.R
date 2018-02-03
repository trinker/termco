#' Apply Normalization to Term/Token Columns
#'
#' Apply normalization to a term count columns of \code{termco} object without
#' stripping the class & attributes of the object.
#'
#' @param x A \code{term_count} object.
#' @param \ldots One or more unquoted expressions separated by commas. You can 
#' treat variable names like they are positions.  See \code{?dplyr::select} for 
#' more information on \ldots.
#' @return Returns a \code{term_count} object.
#' @export
#' @examples
#' \dontrun{
#' library(dplyr)
#'
#' term_list <- list(
#'     `if` = c('if'),
#'     ands = c('an'),
#'     or = c('or'),
#'     buts = c('but')
#' )
#'
#' out <- presidential_debates_2012 %>%
#'      with(term_count(dialogue, TRUE, term_list))
#'
#' out
#'
#' out %>%
#'     select_counts(-or)
#' 
#' out %>%
#'     select_counts(-c(ands:buts))
#'     
#' out %>%
#'     select_counts(n.words, ands:buts)
#'     
#' ## Can't print as a term_count object (n n.words)
#' out %>%
#'     select_counts(ands:buts)
#'     
#' ## Token Counts
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
#' 
#' x %>%
#'     select_counts(-thing)
#'     
#' x %>%
#'     select_counts(-c(person, other))
#'     
#' ## Handles metatags
#' tag_list <- list(
#'     noun = c('person', 'place', 'thing'),
#'     odd_ones = c('other', 'no_like')
#' )
#'
#' x2 <- set_meta_tags(x, tag_list)
#' 
#' x2 %>%
#'     tidy_counts()
#'     
#' x2 %>%
#'     select_counts(-c(person, other))%>%
#'     tidy_counts()
#' }
select_counts <- function(x, ...){

    type <- term_token_validate(x)

    attr_x <- attributes(x)

    out <- dplyr::select(x, ...)
    new_nms <- colnames(out)

    attributes(out)[["group.vars"]] <- attr_x[[type]][attr_x[[type]] %in% new_nms]
    attributes(out)[[type]] <- attr_x[[type]][attr_x[[type]] %in% new_nms]
    ## Strip out premade coverage from original run as it may not be applicable
    attributes(out)[['pre_collapse_coverage']] <- NULL
    rm_class(out, "collapsed_hierarchical_term_count")

}
