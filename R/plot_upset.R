#' Plot \code{term_count} Object as Upset Plot
#'
#' The upset plot is designed to allow for exploration of overlapping sets where
#' Euler/Venn plots fail to scale.  This function wraps the
#' \code{\link[UpSetR]{upset}} to allow for exploration of the degree to which
#' categories from a \code{term_count} object overlap.  This may help to collapse
#' codes or to see how constructs are combined within the same text.  The method
#' is complex and requires careful study and interpretations but the time
#' invested can pay dividends in scalable insights.
#'
#' @param x A \code{term_count} object.
#' @param text_funs Additional list of named functions (names will be used for
#' naming the variables created) for creating additional columns in the data set
#' that can be used to add attributes plots to the \code{\link[UpSetR]{upset}}
#' output.  These columns must be added but then called via \ldots using
#' \code{\link[UpSetR]{upset}} syntax.  Note that \code{n.tags} & \code{n.words}
#' is computed automatically without the need to pass a function in directly here.
#' @param \ldots Other arguments passed to \code{\link[UpSetR]{upset}}.
#' @return Returns an uset plot.
#' @references Jake R Conway, J. R, Lex, A., & Gehlenborg, N. (2017), UpSetR: An
#'  R package for the visualization of intersecting sets and their properties
#'  doi:10.1093/bioinformatics/btx364 \cr\cr
#'  \url{http://caleydo.org/tools/upset}
#' @note Use \code{?UpSetR::upset} for a full list of the parameters that can be
#' passed to \code{termco::plot_upset}.  For example, \code{sets} enables more/less
#' terms to be viewed, \code{order.by} specifies how the intersections between
#' categories is arranged (default is number of tags), and \code{nintersects}
#' hones in on how many intersects (top bar plot) can be viewed at one time.
#' The default is 20.
#' @export
#' @seealso \code{\link[UpSetR]{upset}}
#' @examples
#' require(dplyr)
#' require(UpSetR)
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
#' plot_upset(out)
#'
#' plot_upset(out,
#'     queries = list(
#'         list(query = intersects, params = list("or"), color = "orange", active = TRUE),
#'         list(query = intersects, params = list("if", 'ans'), color = "#0099CC", active = TRUE)
#'     )
#' )
#'
#'
#' ## Attributes plotting with built in text var measures
#' plot_upset(out,
#'     queries = list(
#'         list(query = intersects, params = list("or"), color = "orange", active = TRUE),
#'         list(query = intersects, params = list("if", 'ans'), color = "#0099CC", active = TRUE)
#'     ),
#'     attribute.plots = list(
#'         gridrows = 45,
#'         plots = list(
#'             list(
#'                 plot = scatter_plot,
#'                 x = "n.words",
#'                 y = "n.tags",
#'                 queries = TRUE
#'             )
#'         ),
#'         ncols = 1
#'     ),
#'     query.legend = "bottom"
#' )
#'
#' ## Attributes plotting:
#' ## Compute your own text var measures
#' plot_upset(
#'     out,
#'     text_funs = list(n.chars = function(x) nchar(x)),
#'
#'     main.bar.color = "gray60",
#'     sets.bar.color = "gray60",
#'     matrix.color = 'grey60',
#'
#'     queries = list(
#'         list(query = intersects, params = list("or"), color = "orange", active = TRUE),
#'         list(query = intersects, params = list("if", 'ans'), color = "#0099CC", active = TRUE)
#'     ),
#'     attribute.plots = list(
#'         gridrows = 45,
#'         plots = list(
#'             list(
#'                 plot = scatter_plot,
#'                 x = "n.words",
#'                 y = "n.tags",
#'                 queries = TRUE
#'             ),
#'             list(
#'                 plot = scatter_plot,
#'                 x = "n.words",
#'                 y = "n.chars",
#'                 queries = TRUE
#'             ),
#'             list(
#'                 plot = histogram,
#'                 x = "n.words",
#'                 queries = TRUE
#'             )
#'         ),
#'         ncols = 3
#'     ),
#'     query.legend = "bottom"
#' )
#'
#'
#' \dontrun{
#' ## More examples of computing your own text var measures
#' plot_upset(
#'     out,
#'     text_funs = list(
#'         sentiment = function(z){sentimentr::sentiment_by(z)$ave_sentiment}
#'     ),
#'     queries = list(
#'         list(query = intersects, params = list("or"), color = "orange", active = TRUE),
#'         list(query = intersects, params = list("if", 'ans'), color = "#0099CC", active = TRUE),
#'         list(query = intersects, params = list("buts", 'ans'), color = "#32CD32", active = TRUE)
#'     ),
#'     attribute.plots = list(
#'         gridrows = 45,
#'         plots = list(
#'             list(
#'                 plot = scatter_plot,
#'                 y = "sentiment",
#'                 x = "n.tags.unique",
#'                 queries = TRUE
#'             ),
#'             list(
#'                 plot = scatter_plot,
#'                 y = "sentiment",
#'                 x = "n.tags",
#'                 queries = TRUE
#'             ),
#'             list(
#'                 plot = histogram,
#'                 x = "sentiment",
#'                 queries = TRUE
#'             )
#'         ),
#'         ncols = 3
#'     ),
#'     query.legend = "bottom"
#' )
#'
#' plot_upset(
#'     out,
#'     text_funs = list(
#'         sentiment = function(z){sentimentr::sentiment_by(z)$ave_sentiment}
#'     ),
#'     queries = list(
#'         list(query = intersects, params = list("or"), color = "orange", active = TRUE),
#'         list(query = intersects, params = list("if", 'ans'), color = "#0099CC", active = TRUE),
#'         list(query = intersects, params = list("buts", 'ans'), color = "#32CD32", active = TRUE)
#'     ),
#'     boxplot.summary = c("sentiment")
#' )
#'
#' }
plot_upset <- function(x, text_funs = NULL, ...){

    type <- ifelse(inherits(x, 'token_count'), "token", "term")

    if (!isTRUE(validate_term_count(x, FALSE))) {
        stop(paste0(
            '`x` does not appear to be a valid `',
            type,
            '_count` object.  Was the object altered after creation?'
        ))
    }


    ## Coerce to one hot encoding
    z <- y <- mutate_termco(x)

    nord <- names(sort(colSums(term_cols(y)), TRUE))
    y <- as.data.frame(y, stringsAsFactors = FALSE, check.names = FALSE)

    if (!is.null(text_funs)) {

        txt <- attributes(x)[["text.var"]][["text.var"]]
        if (!is.list(text_funs)) text_funs <- list(text_funs)

        if(is.null(names(text_funs))) names(text_funs) <- make.names(seq_along(text_funs))
        if(any(names(text_funs) == '')) {
            nms <- names(text_funs)
            names(text_funs)[nms == ''] <- make.names(which(nms == ''))
        }

        not_funs <- !unlist(lapply(text_funs, is.function))
        if (any(not_funs)) {
            stop(sprintf(
                'The following `text_funs` were not functions:\n\n  %s',
                paste(names(which(not_funs)), collapse = ', ')
            ))
        }
        txt.dat <- data.frame(lapply(text_funs, function(fun) fun(txt)))
        y <- data.frame(y, txt.dat, stringsAsFactors = FALSE, check.names = FALSE)
    }

    ## search for ... and if not found set some defaults
    dots <- list(...)
    if (is.null(dots$sets)) dots$sets <- rev(nord)
    if (is.null(dots$keep.order)) dots$keep.order <- TRUE
    if (is.null(dots$order.by)) dots$order.by <- "freq"
    if (is.null(dots$nintersects)) dots$nintersects <- 20


    y[['n.tags']] <- rowSums(term_cols(x))
    y[['n.tags.unique']] <- rowSums(term_cols(z))

    epsp <- UpSetR::upset
    do.call("epsp", c(list(y), dots))

}
