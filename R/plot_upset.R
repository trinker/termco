#' Plot \code{term_count} Object as Upset Plot
#'
#' Enables exploration of overlapping term categories which is useful for tasks
#' such as improving discrimination (see also \code{\link[termco]{tag_co_occurrence}}).
#' The upset plot is designed to allow for exploration of overlapping sets where
#' Euler/Venn plots fail to scale.  This function wraps the
#' \code{\link[UpSetR]{upset}} to allow for exploration of the degree to which
#' categories from a \code{term_count} object overlap.  This may help to collapse
#' codes or to see how constructs are combined within the same text.  The upset
#' plot method is complex and requires careful study in order to lead to meaningful
#' interpretations but the time invested can pay dividends in scalable insights.
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
#' @note Because \code{\link[UpSetR]{upset}} has many arguments
#' \pkg{termco} has opted to use \ldots to pass the arguments to \code{plot_upset}
#' as it makes \code{plot_upset} easier to maintain as \code{\link[UpSetR]{upset}}
#' makes changes to its API.  This means the  \code{plot_upset} isn't that useful
#' for understanding how the function operates. Use \code{?UpSetR::upset} for a
#' full list of the parameters that can be passed to \code{termco::plot_upset}.
#' For example, \code{sets} enables more/less terms to be viewed, \code{order.by}
#' specifies how the intersections between categories is arranged (default is
#' number of tags), and \code{nintersects} hones in on how many intersects (top
#' bar plot) can be viewed at one time. The default is 25.  \code{mb.ratio}
#' controls the spacing given to the top and lower pane (2 element numeric vector).
#' By default \code{plot_upset} attempts to auto scale this based on the number
#' of tags being displayed.
#' @export
#' @seealso \code{\link[UpSetR]{upset}}
#' \code{\link[termco]{tag_co_occurrence}}
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
#' plot_upset(out, order.by = c("freq", "degree"))
#' plot_upset(out, order.by = "degree")
#' plot_upset(out, order.by = "degree", decreasing = FALSE)
#'
#' ## Adjust top pane/lower pane spacing
#' plot_upset(out, mb.ratio = c(0.45, 0.55))
#' \dontrun{
#' plot_upset(out, mb.ratio = c(0.85, 0.15))
#' }
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
#'
#' ## Demonstration of the auto scaling of the plot region
#' regs2 <- as_term_list(frequent_terms(presidential_debates_2012[["dialogue"]])[[1]])
#'
#' model2 <- with(presidential_debates_2012,
#'     term_count(dialogue, TRUE, regs2)
#' )
#'
#' plot_upset(model2)
#'
#' regs3 <- as_term_list(frequent_terms(presidential_debates_2012[["dialogue"]], 60)[[1]])
#'
#' model3 <- with(presidential_debates_2012,
#'     term_count(dialogue, TRUE, regs3)
#' )
#'
#' \dontrun{
#' plot_upset(model3)
#' plot_upset(model3, order.by = c("freq", "degree"), nintersects = 80)
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
    y <- mutate_term_count(x)

    nuni <- rowSums(term_cols(y))
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
    if (is.null(dots$nintersects)) dots$nintersects <- 25

    if (is.null(dots$mb.ratio)) {
        slen <- length(dots$sets)
        sloc <- which(ratios_key[['n.tags']] %in% slen)
        if (length(sloc) == 1){
            dots$mb.ratio <- unname(unlist(ratios_key[sloc, 2:3]))
        }
    }

    y[['n.tags']] <- rowSums(term_cols(x))
    y[['n.tags.unique']] <- nuni

    epsp <- UpSetR::upset
    do.call("epsp", c(list(y), dots))

}


ratios_key <- dplyr::data_frame(
    n.tags = 3:80,
    x1 = seq(from = .7, to =  .05, length.out = length(n.tags)),
    x2 = 1 - x1
)
