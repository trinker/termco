#' Search For and Count Terms
#'
#' \code{term_count} - Search a string by any number of grouping variables for
#' categories (themes) of grouped root terms/substrings.
#'
#' @param text.var The text string variable.
#' @param grouping.var The grouping variable(s).  Default \code{NULL} generates
#' one word list for all text.  Also takes a single grouping variable or a list
#' of 1 or more grouping variables.  If \code{TRUE} an \code{id} variable is
#' used with a \code{seq_along} the \code{text.var}.
#' @param term.list A list of named character vectors.  `code{term_count} can
#' be used in a hierarchical fashion as well; that is a list of regexes can be
#' passed and counted and then a second (or more) pass can be taken wit a new
#' set of regexes on only those rows/text elements that were left untagged
#' (count \code{\link[base]{rowSums}} is zero).  This is accomplished by passing
#' a \code{\link[base]{list}} of \code{\link[base]{list}}s of regexes.
#' See \bold{Examples} for the \strong{hierarchical terms} section for a
#' demonstration.
#' @param ignore.case logical.  If \code{FALSE}, the pattern matching is case
#' sensitive and if \code{TRUE}, case is ignored during matching.
#' @param pretty logical.  If \code{TRUE} pretty printing is used.  Pretty
#' printing can be turned off globally by setting
#' \code{options(termco_pretty = FALSE)}.
#' @param group.names A vector of names that corresponds to group.  Generally
#' for internal use.
#' @param \ldots ignored.
#' @return Returns a \code{\link[dplyr]{tbl_df}} object of term counts by
#' grouping variable.
#' @note Note that while a \code{\link[termco]{term_count}} object prints as a
#' combination of integer counts and weighted (default percent of terms) in
#' parenthesis the underlying object is actually a \code{\link[dplyr]{tbl_df}}
#' of integer term/substring counts.  The user can alter a
#' \code{\link[termco]{term_count}} object to print as integer permanently using
#' the \code{\link[termco]{as_count}} function.  A percent \emph{Coverage} also
#' prints.  This is the rate of grouping variables with no term found (i.e.,
#' \code{\link[base]{rowSums}} is zero for terms).  For more details on coverage
#' see \code{\link[termco]{coverage}}.
#' @keywords term substring
#' @rdname term_count
#' @importFrom data.table := .SD
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
#' print(markers, pretty = FALSE)
#' print(markers, zero.replace = "_")
#' plot(markers)
#' plot(markers, labels=TRUE)
#'
#' # permanently remove pretty printing
#' (markers2 <- as_count(markers))
#'
#' # manipulating the output in a dplyr chain
#' library(dplyr)
#'
#' presidential_debates_2012 %>%
#'     with(., term_count(dialogue, list(person, time), discoure_markers)) %>%
#'     as_count()  # removes pretty print method (not necessary to manipulate)
#'
#' presidential_debates_2012 %>%
#'     with(., term_count(dialogue, list(person, time), discoure_markers)) %>%
#'     mutate(totals = response_cries + back_channels + summons + justification) %>%
#'     arrange(-totals)
#'
#' ## hierarchical terms
#' trms <- frequent_terms(presidential_debates_2012[["dialogue"]])[[1]]
#'
#' discoure_markers <- list(
#'     response_cries = c("\\boh", "\\bah", "\\baha", "\\bouch", "yuk"),
#'     back_channels = c("uh[- ]huh", "uhuh", "yeah"),
#'     summons = "hey",
#'     justification = "because"
#' )
#'
#' dbl_list <- list(
#'     discoure_markers,
#'     setNames(as.list(trms[1:8]), trms[1:8]),
#'     setNames(as.list(trms[9:length(trms)]), trms[9:length(trms)])
#' )
#'
#' x <- with(presidential_debates_2012,
#'     term_count(dialogue, TRUE, dbl_list)
#' )
#'
#' coverage(x)
term_count <- function(text.var, grouping.var = NULL, term.list,
                       ignore.case = TRUE, pretty = ifelse(isTRUE(grouping.var), FALSE, TRUE),
                       group.names, ...){

    amodel <- FALSE
    if(is.null(grouping.var)) {
        G <- "all"
    } else {
        if (is.list(grouping.var)) {
            m <- unlist(as.character(substitute(grouping.var))[-1])
            G <- sapply(strsplit(m, "$", fixed=TRUE), function(x) {
                x[length(x)]
            }
            )
        } else {
            if (isTRUE(grouping.var)) {
                G <- "id"
                amodel <- TRUE
            } else {
                G <- as.character(substitute(grouping.var))
                G <- G[length(G)]
            }
        }
    }
    if(is.null(grouping.var)){
        grouping <- rep("all", length(text.var))
    } else {
        if (isTRUE(grouping.var)) {
            grouping <- seq_along(text.var)
        } else {
            if (is.list(grouping.var) & length(grouping.var)>1) {
                grouping <- grouping.var
            } else {
                grouping <- unlist(grouping.var)
            }
        }
    }

    if(!missing(group.names)) {
        G <- group.names
    }

    DF <- data.frame(text.var, check.names = FALSE, stringsAsFactors = FALSE)
    DF[G] <- grouping
    DF['n.words'] <- stringi::stri_count_words(text.var)

    DF <- data.table::setkeyv(data.table::data.table(DF), G)

    ## check for hierarchical terms
    list_list <- FALSE
    if (is.list(term.list[[1]]) && length(term.list) > 1 && all(sapply(term.list, is.list))) {

        ## make sure for hierarchical terms that each observation is also a group
        if(nrow(DF) != nrow(unique(DF[,G, with=FALSE]))) {
            stop("In order to run nested `term.list` then `grouping.var` must place every observation in its own group.")
        }

        list_list <- TRUE

        #out_list <- vector(mode = "list", length = length(term.list))
        #inds  <- vector(mode = "list", length = length(term.list))
        term.list <- lapply(term.list, term_lister_check, G)

        inds <- seq_along(text.var)

        for (i in seq_along(term.list)){

            if (i == 1){
                counts <- data.table::setkeyv(
                    data.table::copy(data.table::setDT(DF))[inds, ][,
                                                                    names(term.list[[i]]):= lapply(term.list[[i]], countfun,
                                                                                                   text.var, ignore.case = ignore.case), ][, 'text.var':=NULL],
                    G
                )
            } else {

                counts <- merge(
                    counts,
                    data.table::setkeyv(data.table::copy(data.table::setDT(DF))[inds, ][,
                                                                                        names(term.list[[i]]):= lapply(term.list[[i]], countfun,
                                                                                                                       text.var, ignore.case = ignore.case), ][, 'text.var':=NULL][,
                                                                                                                                                                                   'n.words' := NULL], G),
                    all=TRUE
                )

            }

            terminds <- (1 + which(colnames(counts) == "n.words")):ncol(counts)
            inds <- which(rowSums(counts[, terminds, with = FALSE]) == 0)
        }


        term.cols <- colnames(counts)[(1 + which(colnames(counts) == "n.words")):ncol(counts)]
        for (i in term.cols) eval(parse(text=paste("counts[,",i,":=na.replace(",i,")]")))
        out <- counts[,lapply(.SD, sum, na.rm = TRUE), keyby = G]


    } else {
        term.list <- term_lister_check(term.list, G)

        counts <- data.table::setDT(DF)[, names(term.list):= lapply(term.list, countfun,
                                                                    text.var, ignore.case = ignore.case), ][, text.var:=NULL]

        out <- counts[,lapply(.SD, sum, na.rm = TRUE), keyby = G]
    }

    text <- new.env(hash=FALSE)
    text[["text.var"]] <- text.var

    count <- new.env(hash=FALSE)
    count[["count"]] <- counts

    out <- dplyr::tbl_df(out)
    class(out) <- c("term_count", class(out))

    if(isTRUE(list_list)) class(out) <- c("hierarchical_term_count", class(out))

    attributes(out)[["group.vars"]] <- G
    if (isTRUE(list_list)) {
        attributes(out)[["term.vars"]] <- unlist(lapply(term.list, names))
    } else {
        attributes(out)[["term.vars"]] <- names(term.list)
    }
    attributes(out)[["weight"]] <- "count"
    attributes(out)[["pretty"]] <- pretty
    attributes(out)[["counts"]] <- count
    attributes(out)[["text.var"]] <- text
    attributes(out)[["model"]] <- amodel
    if(isTRUE(list_list)) attributes(out)[["hierarchical_terms"]] <- lapply(term.list, names)
    out
}


na.replace <- function(v, value=0) { v[is.na(v)] <- value; v }
mymerge <-  function(x, y) merge(x, y, all=TRUE)


term_lister_check <- function(term.list, G){
    if(any(G %in% names(term.list))) stop("`grouping` names cannot be used as `term.list` names")

    nms <- names(term.list)
    names(term.list)[sapply(nms, identical, "")] <- make.names(seq_len(length(nms[sapply(nms,
                                                                                         identical, "")])))

    if (!is.list(term.list)) {
        warning("Expecting a named list for `term.list`; coercing to list.")
        term.list <- as.list(term.list)
        if (is.null(names(term.list))) term.list <- stats::setNames(term.list, term.list)
    } else {
        term.list <- lapply(term.list, function(x) paste(paste0("(", x, ")"), collapse = "|"))
    }
    term.list
}

na.replace <- function(v, value=0) { v[is.na(v)] <- value; v }
mymerge <-  function(x, y) merge(x, y, all=TRUE)


term_lister_check <- function(term.list, G){
    if(any(G %in% names(term.list))) stop("`grouping` names cannot be used as `term.list` names")

    nms <- names(term.list)
    names(term.list)[sapply(nms, identical, "")] <- make.names(seq_len(length(nms[sapply(nms,
        identical, "")])))

    if (!is.list(term.list)) {
        warning("Expecting a named list for `term.list`; coercing to list.")
        term.list <- as.list(term.list)
        if (is.null(names(term.list))) term.list <- stats::setNames(term.list, term.list)
    } else {
        term.list <- lapply(term.list, function(x) paste(paste0("(", x, ")"), collapse = "|"))
    }
    term.list
}

#' Prints a term_count Object
#'
#' Prints a term_count object.
#'
#' @param x The term_count object.
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
#' @method print term_count
#' @export
print.term_count <- function(x, digits = 2, weight = "percent",
                             zero.replace = "0", pretty = getOption("termco_pretty"), ...) {

    n.words <- count <- NULL
    if (is.null(pretty)) pretty <- TRUE
    if (weight == "count") pretty <- FALSE

    print_order <- c(attributes(x)[['group.vars']], 'n.words', attributes(x)[['term.vars']])

    val <- validate_term_count(x)
    if (!isTRUE(val)) {

        termcols <- attributes(x)[["term.vars"]]
        wrdscol <- any(colnames(x) %in% 'n.words')

        if (wrdscol & !is.null(termcols) && any(colnames(x) %in% termcols)) {

            termcols <- colnames(x)[colnames(x) %in% termcols]

        } else {

            return(print(rm_class(x, "term_count")))

        }
    } else {

        termcols <- attributes(x)[["term.vars"]]
    }

    coverage <- sum(cov <- rowSums(x[, termcols]) != 0)/length(cov)

    start <- Sys.time()
    if (is.count(x) & pretty & attributes(x)[["pretty"]]) {

        tall <- tidyr::gather_(x, "term", "count", termcols)
        tall_weighted <- dplyr::mutate(tall, count = comb(count, n.words, digits = digits,
                                                          zero.replace = zero.replace, weight = weight))

        x <- tidyr::spread_(tall_weighted, "term", "count")
    }
    ptime <- difftime(Sys.time(), start)

    class(x) <- class(x)[!class(x) %in% "term_count"]
    cat(sprintf("Coverage: %s%%", 100 * round(coverage, 4)), "\n")

    print(x[, print_order])

    ask <- getOption("termco_pretty_ask")
    if(is.null(ask)){
        ask <- TRUE
    }

    if(ask && ptime > .61 && interactive()){
        message(paste0(paste(rep("=", 70), collapse = ""), "\n"),
                "\nYour `term_count` object is larger and is taking a while to print.\n",
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




#' Plots a term_count object
#'
#' Plots a term_count object.
#'
#' @param x The term_count object.
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
#' @method plot term_count
#' @export
plot.term_count <- function(x, labels = FALSE, low ="white",
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
    vars <- colnames(y)[!colnames(y) %in% c("group.vars", "n.words")]
    dat <- tidyr::gather_(y, "terms", "values", vars)

    if (isTRUE(labels)){
        values <- NULL
        fact <- ifelse(weight == "percent", 100, 1)
        dat <- dplyr::mutate(dat, labels = digit_format(values/fact, label.digits))
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
