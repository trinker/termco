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
#' conform to this standard.  `code{token_count} can be used in a hierarchical
#' fashion as well; that is a list of tokens that can be passed and counted and
#' then a second (or more) pass can be taken with a new set of tokens on only
#' those rows/text elements that were left untagged (count
#' \code{\link[base]{rowSums}} is zero).  This is accomplished by passing
#' a \code{\link[base]{list}} of \code{\link[base]{list}}s of search tokens.
#' See \bold{Examples} for the \strong{hierarchical tokens} section for a
#' demonstration.
#' @param stem logical.  If \code{TRUE} the search is done after the terms have
#' been stemmed.
#' @param keep.punctuation logical.  If \code{TRUE} the punctuation marks are
#' considered as tokens.
#' @param pretty logical.  If \code{TRUE} pretty printing is used.  Pretty
#' printing can be turned off globally by setting
#' \code{options(termco_pretty = FALSE)}.
#' @param group.names A vector of names that corresponds to group.  Generally
#' for internal use.
#' @param \ldots Other arguments passed to \code{\link[gofastr]{q_dtm}}.
#' @return Returns a \code{\link[dplyr]{tbl_df}} object of term counts by
#' grouping variable.  Has all of the same features as a \code{term_count}
#' object, meaning functions that work on a \code{term_count} object will
#' operate on a a \code{token_count} object as well.
#' @export
#' @examples
#' token_list <- list(
#'     person = c('sam', 'i'),
#'     place = c('here', 'house'),
#'     thing = c('boat', 'fox', 'rain', 'mouse', 'box', 'eggs', 'ham'),
#'     no_like = c('not like')
#' )
#'
#' token_count(sam_i_am, grouping.var = TRUE, token.list = token_list)
#' token_count(sam_i_am, grouping.var = NULL, token.list = token_list)
#'
#' x <- presidential_debates_2012[["dialogue"]]
#'
#' bigrams <- apply(ngram_collocations(x)[, 1:2], 1, paste, collapse = " ")
#' bigram_model <- token_count(x, TRUE, token.list = as_term_list(bigrams))
#' as_dtm(bigram_model)
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
#'
#' presidential_debates_2012 %>%
#'      with(token_count(dialogue, list(person, time), token_list)) %>%
#'      plot()
#' }
#'
#' ## hierarchical tokens
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
#' (x <- token_count(sam_i_am, grouping.var = TRUE, token.list = token_list))
#' attributes(x)[['pre_collapse_coverage']]
token_count <- function(text.var, grouping.var = NULL, token.list, stem = FALSE,
    keep.punctuation = TRUE, pretty = ifelse(isTRUE(grouping.var), FALSE, TRUE),
    group.names, ...) {

    amodel <- FALSE
    auto_map <- FALSE

    ## check for hierarchical terms
    list_list <- FALSE
    if (is.list(token.list[[1]]) && length(token.list) > 1 && all(sapply(token.list, is.list))) list_list <- TRUE

    token.listsaved <- token.list

    ## swap out spaces as necessary
    subwrds <- grep(".\\s+.", unlist(token.list), value=TRUE)
    if (length(subwrds > 0)){
        tph <-'termcoplaceholdertermco'
        text.var <- .mgsub(subwrds, gsub("\\s+", tph, subwrds), tolower(text.var), perl=FALSE)
        if (list_list) {
            token.list <- lapply(token.list, function(a) {lapply(a, function(x) gsub("\\s+", tph, x))})
        } else {
            token.list <- lapply(token.list, function(x) gsub("\\s+", tph, x))
        }
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
                amodel <- TRUE
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
    n.tokens <- slam::row_sums(dtm)

    ## respond based on hierarchical terms
    if (list_list) {

        ## make sure for hierarchical terms that each observation is also a group
        if(nrow(DF) != nrow(unique(DF[,G, drop=FALSE]))) {
            stop("In order to run nested `token.list` then `grouping.var` must place every observation in its own group.")
        }

        ## make sure the tokens conform to regex stadards
        token.list <- lapply(token.list, token_lister_check)

        ## Auto create a map for same named term lists and
        ## add ending number to distinguish
        token.nms <- lapply(token.list, names)
        token.lens <- sapply(token.nms, length)
        token.nms <- unlist(token.nms)

        if (any(duplicated(token.nms))){

            map <- as.list(unique(token.nms))
            names(map) <- unique(token.nms)

            for(i in names(map)){
                suffix <- seq_len(sum(token.nms == i))
                if (length(suffix) == 1) {
                    replacements <- i
                    map[i] <- NULL
                } else {
                    replacements <- paste(i, seq_len(sum(token.nms == i)), sep = "__")
                    map[[i]] <- paste(i, seq_len(sum(token.nms == i)), sep = "__")
                }
                token.nms[token.nms == i] <- replacements
            }

            token.list <- Map(function(x, y) {
                names(x) <- y
                x
            }, token.list, split(token.nms, rep(seq_along(token.lens), token.lens)))

            auto_map <- TRUE

        }

        inds <- seq_along(text.var)
        out_list <- vector(mode = "list", length = length(token.list))

        for (i in seq_along(token.list)){

            out_list[[i]] <- lapply(token.list[[i]], function(x){
                x <- x[x %in% nms]
                unname(slam::row_sums(dtm[, x]))
            })

            if (length(token.list[[i]]) > 1) {
                locs <- which(rowSums(do.call(cbind, out_list[[i]])) > 0)
            } else {
                locs <- which(out_list[[i]][[1]] > 0)
            }

            dtm_locs <- !dtm[['i']] %in% locs
            dtm[['i']] <- dtm[['i']][dtm_locs]
            dtm[['j']] <- dtm[['j']][dtm_locs]
            dtm[['v']] <- dtm[['v']][dtm_locs]

        }


        out <- data.table::as.data.table(data.frame(
            do.call(cbind.data.frame, out_list),
            stringsAsFactors = FALSE,
            check.names = FALSE
        ))

    } else {

        ## check tht token.list is a named list &
        ## tokens are words, numbers or punctuation
        token.list <- token_lister_check(token.list)

        out <- data.table::as.data.table(data.frame(lapply(token.list, function(x){
            x <- x[x %in% nms]
            unname(slam::row_sums(dtm[, x]))
        }), stringsAsFactors = FALSE, check.names = FALSE))

    }

    grpv <- stats::setNames(do.call(rbind.data.frame, strsplit(rownames(dtm), "___")), G)
    if (isTRUE(grouping.var)) grpv['id'] <- seq_along(grpv[['id']])
    out <- data.table::data.table(grpv, n.tokens, out)

    class(out) <- c("token_count", "term_count", "tbl_df", "tbl", "data.frame")
    if(isTRUE(list_list)) class(out) <- c("hierarchical_term_count", class(out))

    text <- new.env(hash=FALSE)
    text[["text.var"]] <- text.var

    regex <- new.env(hash=FALSE)
    regex[["term.list"]] <- token.list

    attributes(out)[["group.vars"]] <- G
    if (isTRUE(list_list)) {
        attributes(out)[["token.vars"]] <- unlist(lapply(token.list, names))
    } else {
        attributes(out)[["token.vars"]] <- names(token.list)
    }
    attributes(out)[["text.var"]] <- text
    attributes(out)[["model"]] <- amodel
    attributes(out)[["pretty"]] <- pretty

    count <- new.env(hash=FALSE)
    count[["count"]] <- out

    attributes(out)[["weight"]] <- "count"
    attributes(out)[["counts"]] <- count
    attributes(out)[["tokens"]] <- token.listsaved

    if(isTRUE(list_list)) attributes(out)[["hierarchical_terms"]] <- lapply(token.list, names)

    if (isTRUE(auto_map)){
        message("Collapsing duplicate `token.list` columns.")
        out <- collapse_tags(out, map, ...)
    }

    out
}


#' Prints a token_count Object
#'
#' Prints a token_count object.
#'
#' @param x The token_count object.
#' @param digits The number of digits displayed.
#' @param weight The weight type.  Currently the following are available:
#' \code{"proportion"}, \code{"percent"}.  See \code{\link[termco]{weight}} for
#' additional information.
#' @param zero.replace The value to replace zero count elements with; defaults
#' to \code{"0"}.
#' @param pretty logical.  If \code{TRUE} the counts print in a pretty fashion,
#' combining count and weighted information into a single display.
#' \code{pretty} printing can be permanently removed with
#' \code{\link[termco]{as_count}}.
#' @param \ldots ignored
#' @method print token_count
#' @export
print.token_count <- function(x, digits = 2, weight = "percent",
    zero.replace = "0", pretty = getOption("termco_pretty"), ...) {

    n.tokens <- count <- NULL
    if (is.null(pretty)) pretty <- TRUE
    if (weight == "count") pretty <- FALSE

    print_order <- c(attributes(x)[['group.vars']], 'n.tokens', attributes(x)[['token.vars']])

    val <- validate_token_count(x)
    if (!isTRUE(val)) {

        termcols <- attributes(x)[["token.vars"]]
        wrdscol <- any(colnames(x) %in% 'n.tokens')

        if (wrdscol & !is.null(termcols) && any(colnames(x) %in% termcols)) {

            termcols <- colnames(x)[colnames(x) %in% termcols]

        } else {

            return(print(rm_class(x, "token_count")))

        }
    } else {

        termcols <- attributes(x)[["token.vars"]]
    }

    coverage <- sum(cov <- rowSums(x[, termcols]) != 0)/length(cov)

    start <- Sys.time()
    if (is.count(x) & pretty & attributes(x)[["pretty"]]) {

        tall <- tidyr::gather_(x, "term", "count", termcols)
        tall_weighted <- dplyr::mutate(tall, count = comb(count, n.tokens, digits = digits,
            zero.replace = zero.replace, weight = weight))

        x <- tidyr::spread_(tall_weighted, "term", "count")
    }
    ptime <- difftime(Sys.time(), start)

    class(x) <- class(x)[!class(x) %in% "token_count"]
    cat(sprintf("Coverage: %s%%", 100 * round(coverage, 4)), "\n")

    print(x[, print_order])

    ask <- getOption("termco_pretty_ask")
    if(is.null(ask)){
        ask <- TRUE
    }

    if(ask && ptime > .61 && interactive()){
        message(paste0(paste(rep("=", 70), collapse = ""), "\n"),
                "\nYour `token_count` object is larger and is taking a while to print.\n",
                "You can reduce this time by using `as_count` or setting:\n\n`options(termco_pretty = FALSE)`\n\n",
                "Would you like to globally set `options(termco_pretty = FALSE)` now?\n")
        ans <- utils::menu(c("Yes", "No", "Not Now"))
        switch(ans,
               `1` = {options(termco_pretty = FALSE)
                   options(termco_pretty_ask = FALSE)},
               `2` = {options(termco_pretty_ask = FALSE)},
               `3` = {options(termco_pretty_ask = TRUE)}
        )
    }

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


validate_token_count <- function(x, warn = FALSE){

    terms <- ifelse(inherits(x, 'token_count'), "token.vars", "term.vars")
    nwords <- ifelse(inherits(x, 'token_count'), "n.tokens", "n.words")
    type <- ifelse(inherits(x, 'token_count'), "token", "term")


    nms2 <- unlist(list(attributes(x)[["token.vars"]], "n.tokens"))
    nms <- unlist(list(attributes(x)[["group.vars"]], nms2))
    check <- all(nms %in% colnames(x)) && all(sapply(x[, nms2], is.numeric))
    check2 <- all(sapply(c("group.vars", "token.vars", "weight", "pretty"), function(y){
        !is.null(attributes(x)[[y]])
    }))
    check3 <- !any(colnames(x) %in% c(nms2, nms, "n.tokens"))
    if (!check | !check2 | check3) {
        if (isTRUE(warn)){
            warning("Does not appear to be a `token_count` object.\n",
                "  Has the object or column names been altered/added?",
                immediate. = TRUE
            )
        }
        return(FALSE)
    }
    TRUE
}


#' Plots a token_count object
#'
#' Plots a token_count object.
#'
#' @param x The token_count object.
#' @param labels logical.  If \code{TRUE} the cell count values will be included
#' on the heatmap.
#' @param low The color to be used for lower values.
#' @param high The color to be used for higher values.
#' @param grid The color of the grid (Use \code{NA} to remove the grid).
#' @param label.color The color to make labels if \code{labels = TRUE}.
#' @param label.size The size to make labels if \code{labels = TRUE}.
#' @param label.digits The number of digits to print if labels are printed.
#' @param weight The weight to apply to the cell values for gradient fill.
#' Currently the following are available:
#' \code{"proportion"}, \code{"percent"}, and \code{"count"}.  See
#' \code{\link[termco]{weight}} for additional information.
#' @param \ldots ignored
#' @method plot token_count
#' @export
plot.token_count <- function(x, labels = FALSE, low ="white",
    high = "red", grid = NA, label.color = "grey70", label.size = 3,
    label.digits = if(weight=="count"){0} else {2}, weight = "percent", ...){

    group <- attributes(x)[["group.vars"]]
    if (weight == "count") {
        y <- x
    } else {
        y <- weight(x, weight = weight)
    }

    y[["group.vars"]] <- paste2(y[, group], sep = "_")
    y[["group.vars"]] <- factor(y[["group.vars"]], levels = rev(y[["group.vars"]]))
    y <- y[!colnames(y) %in% group]
    vars <- colnames(y)[!colnames(y) %in% c("group.vars", "n.tokens")]
    dat <- tidyr::gather_(y, "terms", "values", vars)

    if (isTRUE(labels)){
        values <- NULL
        fact <- ifelse(weight == "percent", 100, 1)
        dat <- dplyr::mutate(dat, labels = values/fact, label.digits)
    }

    out <- ggplot2::ggplot(dat, ggplot2::aes_string(y = "group.vars", x = "terms", fill = "values")) +
        ggplot2::theme_bw() +
        ggplot2::theme(
            axis.text.x = ggplot2::element_text(angle = 90, vjust = .5, hjust = 1),
            panel.grid.minor = ggplot2::element_blank(),
            panel.grid.major = ggplot2::element_blank(),
            panel.border = ggplot2::element_rect(colour = "grey80"),
            legend.key.width = grid::unit(.25, 'cm'),
            legend.key.height = grid::unit(1, 'cm')
        ) +
        ggplot2::xlab("Terms Categories") +
        ggplot2::ylab("Groups") +
        ggplot2::geom_tile(color = grid)

    if (weight == "percent"){
        out <- out +
            ggplot2::scale_fill_gradient(high = high, low = low, name = "Percent",
                labels = function(x) paste0(x, "%"))
    } else {
        out <- out +
            ggplot2::scale_fill_gradient(high = high, low = low,
                name = gsub("(\\w)(\\w*)","\\U\\1\\L\\2", weight, perl=TRUE))
    }
    if (isTRUE(labels)){
        out <- out +
            ggplot2::geom_text(ggplot2::aes_string(label = 'labels'),
                color = label.color, size = label.size)
    }

    out
}
