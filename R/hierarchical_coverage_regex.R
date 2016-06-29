#' Hierarchical Coverage of Regexes
#'
#' The unique coverage of a text vector by a regex after partitioning out the
#' elements matched by previous regexes.
#'
#' @param text.var A text vector (vector of strings).
#' @param term.list A list of named character vectors to match against \code{x}.
#' @param ignore.case logical.  Should case be ignored in matching the
#' \code{terms} against \code{x}?
#' @param sort logical.  If \code{TRUE} the output is sorted by highest unique
#' gain.  If \code{FALSE} order of term input is retained.
#' @param verbose If \code{TRUE} each iteration of the \code{for} loop prints
#' \code{i of n}.
#' @param \ldots ignored.
#' @family hierarchical_coverage functions
#' @return Returns a \code{\link[base]{data.frame}} with 7 columns:
#' \describe{
#'   \item{step}{the order in which the regex was searched for}
#'   \item{name}{the human readable name of the bound regex group}
#'   \item{unique_prop}{the unique prop coverage of the regex}
#'   \item{unique_n}{the unique n coverage of the regex}
#'   \item{cum_prop}{the cumulative prop coverage of the regex}
#'   \item{cum_n}{the cumulative n coverage of the regex}
#'   \item{regex}{the bound (|) regex that corresponds to \code{name}}
#' }
#' @keywords coverage
#' @export
#' @examples
#' regs <- setNames(
#'     list(c('(?i)sam', "(?i)\\bam"), '^I', '(?i)(do|will) not', '(?i)(do|will)'),
#'     c('am', 'I', "won't")
#' )
#' (out <- hierarchical_coverage_regex(sam_i_am, regs, ignore.case=FALSE))
#' summary(out)
#' plot(out)
#' plot(out, mark.one = TRUE)
#'
#' # Use unnamed vectors for `term.list` too
#' hierarchical_coverage_regex(sam_i_am, unlist(regs, use.names = FALSE), ignore.case=FALSE)
hierarchical_coverage_regex <- function(text.var, term.list, ignore.case = TRUE,
    sort = FALSE, verbose = TRUE, ...){

    stopifnot(is.atomic(text.var))

    unique_prop <- unique_n <- unique <- cumulative <- NULL
    #original <- term  #removed 6/29 appears to not do anything

    if (!is.list(term.list)) {
        term.list <- as.list(term.list)
    } else {
        term.list <- lapply(unnest_term_list(term.list), function(x) paste(paste0("(", x, ")"), collapse = "|"))
    }

    if (is.null(names(term.list))) term.list <- stats::setNames(term.list, seq_along(term.list))
    if (anyNA(names(term.list))) names(term.list)[is.na(names(term.list))] <- "-"


    orig_nx <- length(text.var)
    n <- coverage <- vector(mode="numeric", length=length(term.list))
    N <- NA

    for(i in seq_along(term.list)){
        N <- length(text.var)
        text.var <- grep_return_null(term.list[[i]], text.var, ignore.case = ignore.case)
        n[i] <- (N - length(text.var))
        coverage[i] <- n[i]/orig_nx
        if (isTRUE(verbose)) {
            cat(sprintf("%s of %s", i, length(term.list)), "\n"); utils::flush.console()
            if (i == length(term.list)) cat("\n\n\n")
        }
    }

    out <- data.frame(step = seq_along(term.list), name = names(term.list),
        regex = unlist(term.list, use.names=FALSE),
        unique_prop = coverage, unique_n = n, stringsAsFactors = FALSE)


    if (isTRUE(sort)) out <- dplyr::arrange(out, dplyr::desc(unique_prop))


    out <- dplyr::mutate(out, cum_prop = cumsum(unique_prop), cum_n = cumsum(unique_n))
    out <- out[, c("step", "name", "unique_prop", "unique_n", "cum_prop", "cum_n", "regex")]

    class(out) <- c("hierarchical_coverage_regex", "data.frame")

    attributes(out)[["remaining"]] <- text.var
    attributes(out)[["prop_covered"]] <- (orig_nx - length(text.var))/orig_nx
    attributes(out)[["prop_uncovered"]] <- (length(text.var))/orig_nx
    attributes(out)[["n_covered"]] <- orig_nx - length(text.var)
    attributes(out)[["n_uncovered"]] <- length(text.var)
    attributes(out)[["n_original"]] <- orig_nx

    out
}



#' Prints a hierarchical_coverage_regex Object
#'
#' Prints a hierarchical_coverage_regex object
#'
#' @param x A hierarchical_coverage_regex object..
#' @param \ldots ignored.
#' @method print hierarchical_coverage_regex
#' @export
print.hierarchical_coverage_regex <- function(x, ...){

    class(x) <- class(x)[!class(x) %in% "hierarchical_coverage_regex"]
    origwd <- options()[["width"]]
    on.exit(options(width=origwd))
    options(width=10000)
    print(x)
    options(width=origwd)
}




#' Plots a hierarchical_coverage_regex Object
#'
#' Plots a hierarchical_coverage_regex object
#'
#' @param x A hierarchical_coverage_regex object.
#' @param use.names logical.  If \code{TRUE} terms are plotted on the x axis.
#' If \code{FALSE} word numbers are.  Te default is to plot terms if they are
#' equal to or less than 30 in length.
#' @param mark.one logical.  If \code{TRUE} a purple horizontal line is added at
#' 100\% and the y axis is extended as well.
#' @param sort logical.  If \code{TRUE} the regex terms are sorted by highest unique
#' gain.
#' @param \ldots ignored.
#' @method plot hierarchical_coverage_regex
#' @export
plot.hierarchical_coverage_regex <- function(x, use.names = nrow(x) <= 30,
    mark.one = FALSE, sort = FALSE, ...){

    unique_prop <- unique_n <- NULL
    if (isTRUE(sort)) {
        x <- dplyr::arrange(x, dplyr::desc(unique_prop))
        x <- dplyr::mutate(x, cum_prop = cumsum(unique_prop), cum_n = cumsum(unique_n))
    }

    x[["name"]] <- factor(x[["name"]], levels = x[["name"]])

    out <- ggplot2::ggplot(x, ggplot2::aes_string(ifelse(use.names , 'name', 'step'), 'cum_prop', group = 1)) +
        ggplot2::geom_line(size=1, color="blue") +
        ggplot2::geom_point(size=3, shape=16, color="blue") +
        ggplot2::geom_point(size=1.2, shape=16, color = "white") +
        ggplot2::scale_y_continuous(label = function(x) paste0(round(100 * x, 0), "%")) +
        ggplot2::ylab("Percent") +
        ggplot2::xlab(ifelse(use.names, "Regex Group", "Regex Number")) +
        ggplot2::theme_bw() +
        ggplot2::theme(axis.text.x = ggplot2::element_text(angle=45, , hjust = 1, vjust = 1))

    if (mark.one) out <- out +  ggplot2::geom_hline(yintercept = 1, color = "purple")
    out  +
        ggplot2::ggtitle("Cumulative Unique Percent Coverage of Each Regex Group")
}



#' Summary of an hierarchical_coverage_regex Object
#'
#' Summary of an hierarchical_coverage_regex object
#'
#' @param object An hierarchical_coverage_regex object.
#' @param \ldots ignored.
#' @method summary hierarchical_coverage_regex
#' @export
summary.hierarchical_coverage_regex <- function(object, ...){

    out <- list(
        remaining = attributes(object)[["remaining"]],
        prop_c = attributes(object)[["prop_covered"]],
        prop_u = attributes(object)[["prop_uncovered"]],
        n_c = attributes(object)[["n_covered"]],
        n_u = attributes(object)[["n_uncovered"]],
        n_o = attributes(object)[["n_original"]]
    )

    class(out) <- "summary_hierarchical_coverage_regex"
    out
}



#' Prints a summary_hierarchical_coverage_regex Object
#'
#' Prints a summary_hierarchical_coverage_regex object
#'
#' @param x A summary_hierarchical_coverage_regex object.
#' @param digits The number of digits to use in rounding percents.
#' @param \ldots ignored.
#' @method print summary_hierarchical_coverage_regex
#' @export
print.summary_hierarchical_coverage_regex <- function(x, digits = 1, ...){

    len <- nchar(pn(x[["n_o"]])) + 4
    c_s <- paste(rep(" ", len - nchar(pn(x[["n_c"]]))), collapse = "")
    u_s <- paste(rep(" ", len - nchar(pn(x[["n_u"]]))), collapse = "")

    cat("<<summary: hierarchical_coverage_regex>>\n\n")
    cat(sprintf("Original  : n = %s\n", pn(x[["n_o"]])))
    cat(sprintf("Covered   : n = %s%s(%s)\n", pn(x[["n_c"]]), c_s, pp(x[["prop_c"]], digits = digits)))
    cat(sprintf("Remaining : n = %s%s(%s)\n", pn(x[["n_u"]]), u_s, pp(x[["prop_u"]], digits = digits)))
}







