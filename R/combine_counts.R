#' Combine \code{term_count} and \code{token_count} Objects
#'
#' Combine \code{term_count} and \code{token_count} objects.
#'
#' @param x A \code{term_count} or \code{token_count} object.
#' @param y A \code{term_count} or \code{token_count} object.
#' @param mapping A list of named vectors where the vector names are the collapsed
#' column names and the vectors are the names of the columns to collapse.  The
#' default, \code{NULL}, combines columns with the same name in both \code{x} and
#' \code{y}.
#' @param \ldots ignored.
#' @return Returns a \code{combine_counts} object.
#' @export
#' @examples
#' \dontrun{
#' token_list <- list(
#'     list(
#'         person = c('sam', 'i')
#'     ),
#'     list(
#'         place = c('here', 'house'),
#'         thing = c('boat', 'fox', 'rain', 'mouse', 'box', 'eggs', 'ham')
#'     ),
#'     list(
#'         no_like = c('not like'),
#'         thing = c('train', 'goat')
#'     )
#' )
#'
#' (y <- token_count(sam_i_am, grouping.var = TRUE, token.list = token_list))
#'
#'
#' term_list <- list(
#'     list(Is = c("I")),
#'     list(
#'         oulds = c("ould"),
#'         thing = c('egg', 'ham')
#'     )
#' )
#'
#'
#' (x <-term_count(sam_i_am, grouping.var = TRUE, term_list, ignore.case = FALSE))
#'
#' combine_counts(x, y)
#' combine_counts(y, x)
#'
#' library(lexicon)
#' library(textshape)
#' library(dplyr)
#'
#' token_list <- lexicon::nrc_emotions %>%
#'     textshape::column_to_rownames() %>%
#'     t() %>%
#'     textshape::as_list()
#'
#'
#' a <- presidential_debates_2012 %>%
#'      with(token_count(dialogue, list(person, time), token_list))
#'
#' term_list <- list(
#'     response_cries = c("\\boh", "\\bah", "\\baha", "\\bouch", "yuk"),
#'     back_channels = c("uh[- ]huh", "uhuh", "yeah"),
#'     summons = "hey",
#'     justification = "because"
#' )
#'
#' b <- presidential_debates_2012 %>%
#'      with(term_count(dialogue, list(person, time), term_list))
#'
#' combine_counts(a, b)
#' combine_counts(b, a)
#'
#'
#' d <- sam_i_am %>%
#'      term_count(TRUE, token_list[1:2])
#'
#' e <- sam_i_am %>%
#'      term_count(TRUE, token_list[[3]])
#'
#'
#' combine_counts(e, d)
#' }
combine_counts <- function(x, y, mapping = NULL, ...){

    ## check that nrows of x and y are same
    stopifnot(nrow(x) == nrow(y))

    ## Get x/y classes
    x_type <- ifelse(validate_token_count(x), 'token',
        ifelse(validate_term_count(x), 'term',
            stop("'x' must be a `term_count` or `token_count` object")))

    y_type <- ifelse(validate_token_count(y), 'token',
        ifelse(validate_term_count(y), 'term',
            stop("'y' must be a `term_count` or `token_count` object")))

    ## attributes x/y
    att_x <- attributes(x)
    att_y <- attributes(y)

    ## term columns
    x_terms <- colnames(tag_cols(x))
    y_terms <- colnames(tag_cols(y))

    ## group columns
    x_groups <- colnames(group_cols(x))
    y_groups <- colnames(group_cols(y))
    if (!all(x_groups %in% y_groups)) stop("'x' must have the same group columns as 'y'")


    ## ensure x/y are ordered the same
    if (x_type == 'term') {
        y <- y[match(paste2(group_cols(x)), paste2(group_cols(y))),]
    } else {
        x <- x[match(paste2(group_cols(y)), paste2(group_cols(x))),]
    }


    ## term columns
    xx <- x[x_terms]
    yy <- y[y_terms]
    if(!isTRUE(all.equal(lapply(x[x_groups], as.character), lapply(y[y_groups], as.character)))) {
        stop("'x' must have the same group columns as 'y'")
    }

    ## generate combine column mapping for when mapping is not provided
    if (is.null(mapping)) {

        nms_xx <- colnames(xx)
        nms_yy <- colnames(yy)
        nms_inter <- intersect(nms_xx, nms_yy)

        if (length(nms_inter) > 0){

            mapping <- lapply(nms_inter, function(x){
                c(paste0(x, "__1"), paste0(x, "__2"))
            })
            names(mapping) <- nms_inter

            nms_xx[match(nms_inter, nms_xx)] <- paste0(nms_xx[match(nms_inter, nms_xx)], "__1")
            colnames(xx) <- nms_xx
            nms_yy[match(nms_inter, nms_yy)] <- paste0(nms_yy[match(nms_inter, nms_yy)], "__2")
            colnames(yy) <- nms_yy
        }

    }

    ## actually combine the combine the like columns
    if (length(mapping) > 0){

        collapses <- paste(Map(function(x, y){paste0(x, " = sum(", paste(y, collapse=", "),
            ")")}, names(mapping), mapping), collapse=", ")
        cnts <- eval(parse(text=paste0("dplyr::mutate(dplyr::rowwise(dplyr::bind_cols(xx, yy)), ", collapses, ")")))

        ## change term.vars attribute and remove columns
        removes <- unlist(mapping, use.names=FALSE)
        cnts <- cnts[excluder(colnames(cnts), excluder(removes, names(mapping)))]

    } else {
        cnts <- dplyr::bind_cols(xx, yy)
    }

    ## recombine the counts, the group cols, and the token/term cols
    if (y_type != x_type) {

        tknsx <- dplyr::select(x, paste0('n.', ifelse(x_type == 'token', 'tokens', 'words')))
        tknsy <- dplyr::select(y, paste0('n.', ifelse(y_type == 'token', 'tokens', 'words')))
        out <- dplyr::bind_cols(group_cols(switch(x_type, term = {x}, token = {y})), tknsx, tknsy, cnts)

    } else {
        tkns <- dplyr::select(x, paste0('n.', ifelse(x_type == 'token', 'tokens', 'words')))
        out <- dplyr::bind_cols(group_cols(x), tkns, cnts)
    }


    class(out) <- c("combine_counts", "term_count", "tbl_df", "tbl", "data.frame")

    attributes(out)[["group.vars"]] <- att_x[["group.vars"]]
    attributes(out)[["text.var"]] <- att_x[["text.var"]]
    attributes(out)[["model"]] <- att_x[["model"]]
    attributes(out)[["pretty"]] <- FALSE
    attributes(out)[["weight"]] <- att_x[["weight"]]
    attributes(out)[["counts"]] <- out
    attributes(out)[["term.vars"]] <- colnames(cnts)

    out
}

#' Prints a combine_counts Object
#'
#' Prints a combine_counts object.
#'
#' @param x The combine_counts object.
#' @param \ldots ignored
#' @method print combine_counts
#' @export
print.combine_counts <- function(x, ...) {

    class(x) <- class(x)[!class(x) %in% c("term_count", "combine_counts")]
    coverage <- sum(cov <- rowSums(tag_cols(x)) != 0)/length(cov)
    cat(sprintf("Coverage: %s%%", 100 * round(coverage, 4)), "\n")
    print(x)

}

