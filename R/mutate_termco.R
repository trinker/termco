#' Apply Normalization to Term/Token Columns
#'
#' Apply normalization to a term count columns of \code{termco} object without
#' stripping the class & attributes of the object.
#'
#' @param x A \code{term_count} object.
#' @param fun A function to apply column-wise.
#' @param \ldots ignored.
#' @return Returns a \code{term_count} object.
#' @export
#' @examples
#' \dontrun{
#' library(dplyr)
#'
#' term_list <- list(
#'     `if` = c('if'),
#'     ans = c('an'),
#'     or = c('or'),
#'     buts = c('but')
#' )
#'
#' out <- presidential_debates_2012 %>%
#'      with(term_count(dialogue, TRUE, term_list))
#'
#' out %>%
#'     mutate_termco()
#'
#' ## default one-hot encoding
#' out %>%
#'     mutate_termco()
#'
#' ## min-max scaling
#' out %>%
#'     mutate_termco(function(x) (x - min(x)) / ((max(x) - min(x))))
#'
#' ## token counts
#' token_list <- list(
#'     person = c('sam', 'i'),
#'     place = c('here', 'house'),
#'     thing = c('boat', 'fox', 'rain', 'mouse', 'box', 'eggs', 'ham'),
#'     no_like = c('not like')
#' )
#'
#' out2 <- token_count(sam_i_am, grouping.var = TRUE, token.list = token_list)
#'
#' ## default one-hot encoding
#' out2 %>%
#'     mutate_termco()
#'
#' ## min-max scaling
#' out2 %>%
#'     mutate_termco(function(x) (x - min(x)) / ((max(x) - min(x))))
#'
#' }
mutate_termco <- function(x, fun = function(x) as.integer(x > 0)){


    terms <- ifelse(inherits(x, 'token_count'), "token.vars", "term.vars")
    type <- ifelse(inherits(x, 'token_count'), "token", "term")

    if (!isTRUE(validate_term_count(x, FALSE))) {
        stop(paste0(
            '`x` does not appear to be a valid `',
            type,
            '_count` object.  Was the object altered after creation?'
        ))
    }

    attr_x <- attributes(x)
    cls_x <- class(x)

    trm_cols <- dplyr::quo(unlist(attributes(x)[[terms]]))

    x <- dplyr::mutate_at(x, dplyr::vars(!!trm_cols), fun)

    attributes(x) <- attr_x
    class(x) <- cls_x

    x

}
