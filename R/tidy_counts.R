#' Convert `term_count` & `token_count` to Tidy Form
#'
#' Converts a wide matrix of counts to tidy form (tags are stretched long-wise
#' with corresponding counts of tags).
#'
#' @param x A `term_count` object.
#' @param \ldots ignored.
#' @return Returns a tibble with tags and counts in long form (retains all other
#' variables in the `term_count` object.
#' @keywords tidy
#' @note \code{n.words} or \code{n.tokens} will be repeated for each row element
#' id (\code{element_id}) and thus are nested.
#' @export
#' @examples
#' ## On term counts
#' discoure_markers <- list(
#'     AA__response_cries = c("\\boh", "\\bah", "\\baha", "\\bouch", "yuk"),
#'     AA__back_channels = c("uh[- ]huh", "uhuh", "yeah"),
#'     BB__summons = "\\bhey",
#'     CC__justification = "because"
#' )
#'
#' terms1 <- with(presidential_debates_2012,
#'     term_count(dialogue, TRUE, discoure_markers)
#' )
#'
#' tidy_counts(terms1)
#'
#'
#' terms2 <- with(presidential_debates_2012,
#'     term_count(dialogue, list(person, time), discoure_markers)
#' )
#'
#' tidy_counts(terms2)
#'
#'
#' ## On token count
#' library(dplyr)
#' token_list <- lexicon::nrc_emotions %>%
#'     textshape::column_to_rownames() %>%
#'     t() %>%
#'     textshape::as_list()
#'
#' token1 <- presidential_debates_2012 %>%
#'      with(token_count(dialogue, TRUE, token_list))
#'
#' tidy_counts(token1)
#'
#'
#' token2 <- presidential_debates_2012 %>%
#'      with(token_count(dialogue, list(person, time), token_list))
#'
#' tidy_counts(token2)
tidy_counts <- function(x, ...){

    i <- n <- element_id <- term <- NULL

    is_token <- methods::is(x, 'token_count')

    validate_term_count(x)

    if (is_token) {

        x_grp <- dplyr::bind_cols(group_cols(x), x[,'n.tokens', drop = FALSE])

    } else {

        x_grp <- dplyr::bind_cols(group_cols(x), x[,'n.words', drop = FALSE])

    }

    if (!isTRUE(attributes(x)[['amodel']])) {
        if ('element_id' %in% colnames(x_grp)) colnames(x_grp)[colnames(x_grp) %in% 'element_id'] <- 'element_id.2'
        x_grp[['element_id']] <- seq_len(nrow(x_grp))
    }

    # z <- tag_cols(x)
    #
    # dplyr::bind_cols(
    #     tibble::tibble(element_id = seq_len(nrow(x))),
    #     tag_cols(x)
    # )


    y <- dplyr::arrange(stats::setNames(
        dplyr::select(
            textshape::tidy_dtm(gofastr::as_dtm(tag_cols(x))),
            i, term, n),
        c('element_id', 'tag', 'n.tag')
    ), element_id)



    out <- dplyr::left_join(
        y,
        x_grp,
        by = 'element_id'
    )

    # if ('id_temp_termco' %in% colnames(out)) {
    #     out[['id']] <- NULL
    #     colnames(out)[colnames(out) %in% 'id_temp_termco'] <- 'id'
    # }

    out <- tibble::tibble(out)

    out <- dplyr::bind_cols(out[, !colnames(out) %in% c('tag', 'n.tag'), drop =FALSE],
        out[, colnames(out) %in% c('tag', 'n.tag'), drop =FALSE])

    ## Add metatags data
    if (isTRUE(check_meta_tags(x))) {
        ## merge meta tags onto tidy tags

        mnms <- colnames(attributes(x)[['metatags']])
        mnms <- mnms[!mnms %in% c('tag')]

        out <- dplyr::left_join(out, attributes(x)[['metatags']], by = 'tag')

        ## reorder to put meta tags before tags
        out <- dplyr::bind_cols(
            out[, !colnames(out) %in% c(mnms, 'tag', 'n.tag'), drop =FALSE],
            out[, colnames(out) %in% mnms, drop =FALSE],
            out[, colnames(out) %in% c('tag', 'n.tag'), drop =FALSE]
        )
    }

    out[['n.tag']] <- as.integer(out[['n.tag']])

    if (!is.null(out[['element_id']]) && !is.null(out[['id']]) &&
            isTRUE(all.equal(out[['element_id']], out[['id']]))) {
        out[['id']] <- NULL
    }
    
    if (is_token) {

        out[['n.tokens']] <- as.integer(out[['n.tokens']])

    } else {

        out[['n.words']] <- as.integer(out[['n.words']])

    }

    ## add class
    if (!isTRUE(attributes(x)[['amodel']])) {

        class(out) <- c('tidy_model', 'tidy_counts', class(out))

    } else {

        class(out) <- c('tidy_counts', class(out))
    }

    attributes(out)[['is_token']] <- is_token
    attributes(out)[["text.var"]] <- attributes(x)[["text.var"]]
    attributes(out)[["group.vars"]] <- attributes(x)[["group.vars"]]
    out

}

