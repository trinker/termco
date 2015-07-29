#' Search For and Count Terms
#'
#' \code{termco} - Search a string by any number of grouping variables for
#' categories (themes) of grouped root terms/substrings.
#'
#' @param text.var The text string variable.
#' @param grouping.var The grouping variable(s).  Default \code{NULL} generates
#' one word list for all text.  Also takes a single grouping variable or a list
#' of 1 or more grouping variables.
#' @param term.list A list of named character vectors.
#' @param ignore.case logical.  If \code{FALSE}, the pattern matching is case
#' sensitive and if \code{TRUE}, case is ignored during matching.
#' @return Returns a \code{\link[dplyr]{tbl_df}} object of term counts by
#' grouping variable.
#' @note Note that while a \code{\link[termco]{term_count}} object prints as a
#' combination of integer counts and weighted (default percent of terms) in
#' parenthesis the underlying object is actually a \code{\link[dplyr]{tbl_df}}
#' of integer term/substring counts.  The user can alter a
#' \code{\link[termco]{term_count}} object to print as integer permanently using
#' the \code{\link[termco]{as.count}} function.
#' @keywords term substring
#' @rdname term_count
#' @importFrom data.table := .SD
#' @export
#' @examples
#' data(pres_debates2012)
#'
#' discoure_markers <- list(
#'     response_cries = c("oh", "ah", "aha", "ouch", "yuk"),
#'     back_channels = c("uh[- ]huh", "uhuh", "yeah"),
#'     summons = "hey",
#'     justification = "because"
#' )
#'
#' (markers <- with(pres_debates2012,
#'     term_count(dialogue, list(person, time), discoure_markers)
#' ))
#'
#' print(markers, pretty = FALSE)
#' print(markers, zero.replace = "_")
#' plot(markers)
#' plot(markers, labels=TRUE)
#'
#' # permanently remove pretty printing
#' (markers2 <- as.count(markers))
#'
#' # manipulating the output in a dplyr chain
#' library(dplyr)
#'
#' pres_debates2012 %>%
#'     with(., term_count(dialogue, list(person, time), discoure_markers)) %>%
#'     as.count()  # removes pretty print method (not necessary to manipulate)
#'
#' pres_debates2012 %>%
#'     with(., term_count(dialogue, list(person, time), discoure_markers)) %>%
#'     mutate(totals = response_cries + back_channels + summons + justification) %>%
#'     arrange(-totals)
term_count <- function(text.var, grouping.var = NULL, term.list, ignore.case = TRUE){

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
            G <- as.character(substitute(grouping.var))
            G <- G[length(G)]
        }
    }
    if(is.null(grouping.var)){
        grouping <- rep("all", length(text.var))
    } else {
        if (is.list(grouping.var) & length(grouping.var)>1) {
            grouping <- grouping.var
        } else {
            grouping <- unlist(grouping.var)
        }
    }

    DF <- data.frame(text.var, check.names = FALSE, stringsAsFactors = FALSE)
    DF[G] <- grouping
    DF['n.words'] <- stringi::stri_count_words(text.var)

    nms <- names(term.list)
    names(term.list)[sapply(nms, identical, "")] <- make.names(seq_len(length(nms[sapply(nms,
        identical, "")])))
    term.list <- lapply(term.list, function(x) paste(paste0("(", x, ")"), collapse = "|"))

    out <- data.table::setDT(DF)[, names(term.list):= lapply(term.list, countfun,
        text.var, ignore.case = ignore.case), ][, text.var:=NULL][,
            lapply(.SD, sum, na.rm = TRUE), keyby = G]

    out <- dplyr::tbl_df(out)
    class(out) <- c("term_count", class(out))
    attributes(out)[["group.vars"]] <- G
    attributes(out)[["term.vars"]] <- nms
    attributes(out)[["weight"]] <- "count"
    attributes(out)[["pretty"]] <- TRUE
    out

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
#' \code{pretty} printing can be permanantly removed with
#' \code{\link[termco]{as.count}}.
#' @param \ldots ignored
#' @method print term_count
#' @export
print.term_count <- function(x, digits = 2, weight = "percent",
    zero.replace = "0", pretty = TRUE, ...) {

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

    if (is.count(x) & pretty & attributes(x)[["pretty"]]) {
        fun2 <- function(y) comb(y, x[["n.words"]], digits = digits,
            zero.replace = zero.replace)

        dat <- dplyr::select_(x, .dots = termcols)
        x[termcols] <- dplyr::mutate_each_(dat, dplyr::funs(fun2), termcols)
    }

    class(x) <- class(x)[!class(x) %in% "term_count"]
    print(x)
}

#' Remove Pretty Printing from \code{term_count} Object
#'
#' Forces a \code{term_count} object to print as count integers rather than a
#' pretty integer weighted combination
#'
#' @param x A \code{term_count} object.
#' @param \ldots ignored
#' @rdname term_count
#' @export
as.count <- function(x, ...){
    validate_term_count(x)
    attributes(x)[["pretty"]] <- FALSE
    x
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
#' @param weight The weight to apply to the cell values for gradient fill.
#' Currently the following are available:
#' \code{"proportion"}, \code{"percent"}.  See \code{\link[termco]{weight}} for
#' additional information.
#' @param \ldots ignored
#' @method plot term_count
#' @export
plot.term_count <- function(x, labels = FALSE, low ="white",
    high = "red", grid = NA, label.color = "grey70", label.size = 3,
    weight = "proportion", ...){

    group <- attributes(x)[["group.vars"]]
    y <- weight(x, weight = weight)
    y[["group.vars"]] <- paste2(y[, group], sep = "_")
    y <- y[!colnames(y) %in% group]
    vars <- colnames(y)[!colnames(y) %in% c("group.vars", "n.words")]
    dat <- tidyr::gather_(y, "terms", "values", vars)

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
        ggplot2::geom_tile(color = grid) +
        ggplot2::scale_fill_gradient(high = high, low = low, name = "Percent",
            labels = scales::percent)

    if (isTRUE(labels)){
        values <- n.words <- NULL
        out <- out +
            ggplot2::geom_text(ggplot2::aes(label = round(n.words * values, 0)),
                color = label.color, size = label.size)
    }

    out
}
