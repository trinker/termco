#' Count Fixed Tokens
#'
#' Count the occurrence of tokens within a vector of strings.  This function
#' differs from \code{\link[termco]{term_count}} in that \code{term_count} is
#' regex based, allowing for fuzzy matching.  This function only searches for
#' lower cased tokens (words, number sequences, or punctuation).  This counting
#' function is faster but less flexible.
#'
#' @param text.var The text string variable.
#' @param grouping.var The grouping variable(s).  Default \code{NULL} generates
#' one word list for all text.  Also takes a single grouping variable or a list
#' of 1 or more grouping variables.  If \code{TRUE} an \code{id} variable is
#' used with a \code{seq_along} the \code{text.var}.
#' @param token.list A list of named character vectors of tokens.  Search will
#' combine the counts for tokens supplied that are in the same vector.  Tokens
#' are defined as  \code{"^([a-z' ]+|[0-9.]+|[[:punct:]]+)$"} and should
#' conform to this standard.
#' @param stem logical.  If \code{TRUE} the search is done after the terms have
#' been stemmed.
#' @param keep.punctuation logical.  If \code{TRUE} the punctuation marks are
#' considered as tokens.
#' @param group.names A vector of names that corresponds to group.  Generally
#' for internal use.
#' @param \ldots Other arguments passed to \code{\link[gofastr]{q_dtm}}.
#' @return Returns a \code{\link[dplyr]{tbl_df}} object of term counts by
#' grouping variable.
#' @export
#' @examples
#' token_list <- list(
#'     person = c('sam', 'i')   ,
#'     place = c('here', 'house'),
#'     thing = c('boat', 'fox', 'rain', 'mouse', 'box', 'eggs', 'ham'),
#'     no_like = c('not like')
#' )
#'
#' token_count(sam_i_am, grouping.var = TRUE, token.list = token_list)
#' token_count(sam_i_am, grouping.var = NULL, token.list = token_list)
#'
#' \dontrun{
#' if (!require("pacman")) install.packages("pacman")
#' pacman::p_load(tidyverse, lexicon, textshape)
#'
#' token_list <- lexicon::nrc_emotions %>%
#'     textshape::column_to_rownames() %>%
#'     t() %>%
#'     textshape::as_list()
#'
#' presidential_debates_2012 %>%
#'      with(token_count(dialogue, TRUE, token_list))
#'
#' presidential_debates_2012 %>%
#'      with(token_count(dialogue, list(person, time), token_list))
#' }
token_count <- function(text.var, grouping.var = NULL, token.list, stem = FALSE,
    keep.punctuation = TRUE, group.names, ...) {

    ## check tht token.list is a named list &
    ## tokens are words, numbers or punctuation
    token.list <- token_lister_check(token.list)

    ## swap out spaces as necessary
    subwrds <- grep(".\\s+.", unlist(token.list), value=TRUE)
    if (length(subwrds > 0)){
        tph <-'termcoplaceholdertermco'
        text.var <- .mgsub(subwrds, gsub("\\s+", tph, subwrds), tolower(text.var), perl=FALSE)
        token.list <- lapply(token.list, function(x) gsub("\\s+", tph, x))
    }

    if (is.null(grouping.var)) {
        G <- "all"
    } else {
        if (is.list(grouping.var)) {
            if (is.data.frame(grouping.var)){
                G <- colnames(grouping.var)
            } else {
                m <- gsub("(\'|\")\\]+$", "", unlist(as.character(substitute(grouping.var))[-1]))
                G <- sapply(strsplit(m, "\\$|(\\[\\s*,?\\s*(\'|\"))", fixed = FALSE), function(x) {
                    x[length(x)]
                })
            }
        } else {
            if (isTRUE(grouping.var)) {
                G <- "id"
            } else {
                G <- as.character(substitute(grouping.var))
                G <- G[length(G)]
            }
        }
    }
    if (is.null(grouping.var)) {
        grouping <- rep("all", length(text.var))
    } else {
        if (isTRUE(grouping.var)) {
            grouping <- seq_along(text.var)
        } else {
            if (is.list(grouping.var) & length(grouping.var) > 1) {
                grouping <- grouping.var
            } else {
                grouping <- unlist(grouping.var)
            }
        }
    }
    if (!missing(group.names)) {
        G <- group.names
    }

    ## choose stem or not version of doc-term-mat function
    if (stem) {
        make_dtm <- gofastr::q_dtm_stem
    } else {
        make_dtm <- gofastr::q_dtm
    }

    ## Build data.frames with text and vars
    DF <- data.frame(text.var, check.names = FALSE, stringsAsFactors = FALSE)
    DF[G] <- grouping

    ## create DTM
    dtm <- make_dtm(text.var, paste2(DF[G], sep = "___"), removePunct = !keep.punctuation, ...)

    nms <- colnames(dtm)

    out <- data.table::as.data.table(data.frame(lapply(token.list, function(x){
        x <- x[x %in% nms]
        unname(slam::row_sums(dtm[, x]))
    }), stringsAsFactors = FALSE, check.names = FALSE))
    grpv <- stats::setNames(do.call(rbind.data.frame, strsplit(rownames(dtm), "___")), G)

    out <- data.table::data.table(grpv, out)
    class(out) <- c("token_count", "tbl_df", "tbl", "data.frame")
    out
}


token_lister_check <- function(token.list){

    token_check <- grepl("^([a-z' ]+|[0-9.]+|[[:punct:]]+)$", unlist(token.list))
    if (!all(token_check)) {
        warning(
            paste0(
                "The following tokens did not conform to expected standards:\n",
                paste(paste0("  -", unlist(token.list)[which(!token_check)]), collapse = "\n")
            )
        )
    }

    nms <- names(token.list)
    names(token.list)[sapply(nms, identical, "")] <- make.names(seq_len(length(nms[sapply(nms, identical, "")])))

    if (!is.list(token.list)) {
        warning("Expecting a named list for `token.list`; coercing to list.")
        token.list <- as.list(token.list)
        if (is.null(names(token.list))) token.list <- stats::setNames(token.list, token.list)
    }

    token.list
}
