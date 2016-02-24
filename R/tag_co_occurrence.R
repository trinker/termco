#' Explore Tag Co-Occurrence
#'
#' Explore tag co-occurrence.  The resulting list comes with a plot method that
#' allows the user to use a network graph to view the connections between
#' tags as well as the average number of other tags that co-occur with each of
#' the regex tags.  This can provide information regarding the discriminatory
#' power of each regex that corresponds to a tag.
#'
#' @param x A \code{\link[termco]{term_count}} object.
#' @param \ldots ignored.
#' @return Returns a list of:
#' \item{ave_tag}{A 2 column data.frame of tags and the average number of other tags that co-occur with it.}
#' \item{cor}{A min-max scaled correlation matrix between tags; diagonals set to 0.}
#' \item{min_max_adjacency}{A min-max scaled adjacency matrix between tags; diagonals set to 0.}
#' \item{node_size}{The diagonals from the adjacency matrix; the number of times a tag occurred.}
#' @export
#' @author Steve T. Simpson and Tyler Rinker <tyler.rinker@@gmail.com>.
#' @examples
#' ## Example 1
#' regs <- frequent_terms(presidential_debates_2012[["dialogue"]])[[1]]
#' regs <- setNames(as.list(regs), regs)
#'
#' model <- with(presidential_debates_2012,
#'     term_count(dialogue, TRUE, regs)
#' )
#'
#' x <- tag_co_occurrence(model)
#' names(x)
#' setNames(
#'     lapply(names(x), function(a) {if(is.matrix(x[[a]])){ round(x[[a]], 2)} else { x[[a]] }}),
#'     names(x)
#' )
#' heatmap(x[["cor"]])
#' heatmap(x[["min_max_adjacency"]])
#' barplot(sort(x[["node_size"]], TRUE), las=2)
#' barplot(setNames(x[["ave_tag"]][[2]], x[["ave_tag"]][[1]]), las=2)
#'
#' ## Note. Restart the graphics device between plot calls if working
#' ##       interactively. See `?plot.tag_co_occurrence` for details.
#' plot(x)
#' plot(x, cor=FALSE)
#' plot(x, min.edge.cutoff = .1, node.color = "#1CDB4F")
#' plot(x, min.edge.cutoff = .2, node.color = "gold", digits = 3)
#' plot(x, bar = FALSE)
#'
#' ## Example 2
#' regs2 <- frequent_terms(presidential_debates_2012[["dialogue"]], n=50)[[1]]
#' regs2 <- setNames(as.list(regs2), regs2)
#'
#' model2 <- with(presidential_debates_2012,
#'     term_count(dialogue, TRUE, regs2)
#' )
#'
#' x2 <- tag_co_occurrence(model2)
#' plot(x2)
#' plot(x2, bar = FALSE, min.edge.cutoff = .13)
#' plot(x2, bar = FALSE, min.edge.cutoff = .18, node.color = "#ead453")
#' plot(x2, node.weight = 3)
#' plot(x2, edge.weight = 20, node.weight = 5)
#'
#' plot(x2, edge.color = "gray80", node.color = "grey50", font.color = "white",
#'     background.color = "black")
#'
#' ## Small Number of Tags Example
#' plot(tag_co_occurrence(markers), node.weight = 30)
tag_co_occurrence <- function(x, ...){

    tag <- NULL
    validate_term_count(x, TRUE)

    ## tag clustering
    adjmat <- crossprod(as.matrix(x[, attributes(x)[["term.vars"]]]))

    node_size <- diag(adjmat)
    diag(adjmat) <- 0

    adjmat <- minmax_scale(adjmat)

    cc <- cor(as.matrix(x[, attributes(x)[["term.vars"]]]))
    diag(cc) <- 0
    cc <- minmax_scale(cc)
    diag(cc) <- 0

    tags <- textshape::bind_list(classify(x, Inf), "id", "tag")[,
        tag := ifelse(is.na(tag), "<<no tag>>", tag)]
    data.table::setkey(tags, "tag")

    tags2 <- data.table::copy(tags)
    data.table::setkey(tags2, "id")

    ave_tags <- textshape::bind_vector(unlist(lapply(unique(tags[["tag"]]), function(x){
        .N <- n <- NULL
        tag <- data.table::data.table(tag=x)
        ids <- data.table::data.table(id=tags[tag][["id"]])
        out <- unlist(tags2[ids][, list(n=.N), by = "id"][, list(ave = mean(n))])
        names(out) <- x
        out
    })), "tag", "ave")[order(-ave)][, tag := factor(tag, levels=tag)][]

    out <- list(ave_tag = data.frame(ave_tags, stringsAsFactors = FALSE),
        cor = cc, min_max_adjacency = adjmat, node_size = node_size)
    class(out) <- "tag_co_occurrence"
    out

}


#' Plots a tag_co_occurrence Object
#'
#' Plots a tag_co_occurrence object
#'
#' @param x A tag_co_occurrence object.
#' @param cor logical.  If \code{TRUE} the correlation matrix is used for the
#' network graph, otherwise the adjacency matrix is used.
#' @param edge.weight A weight for the edges.
#' @param node.weight A weight for the nodes.
#' @param edge.color A color for the edges.
#' @param node.color A color for the nodes.
#' @param bar.color A color for the bar fill; defaults to \code{node.color}.
#' @param font.color A color for the node and axis text.
#' @param bar.font.color A color for the bar/dotplot (mean co-occurrences).
#' @param background.color The plot background color.
#' @param bar.font.size A font size for the bar/dotplot (mean co-occurrences).
#' Default tries to calculate based on number of bars.
#' @param node.font.size The \code{vertex.label.cex} for the node labels.
#' @param digits The number of digits to print for bar/dotplot font (mean
#' co-occurrences).
#' @param min.edge.cutoff A minimum value to use as a cut-off in the network plot.
#' If a value in the correlation/adjacency matrix is below this value, not egde
#' will be plotted for the tag (node) connection.
#' @param plot.widths A vector of proportions of length 2 and totalling 1
#' corresponding to the relative width of the network and  bar/dotplot.
#' @param bar logical.  If \code{TRUE} a bar plot is used as the second plot,
#' otherwise a dotplot is used.
#' @param type The graph type (network & bar/dotplot).  Choices are:
#' \code{"bar"}, \code{"network"}, or \code{"both"} corresponding to the graph
#' type to print.
#' @param \ldots Other arguments passed to \code{\link[igraph]{plot.igraph}}.
#' @note The \code{\link[graphics]{par}} function is called in this function.
#' This will globally reset the graphics device resulting in odd plots in
#' the next call to \code{plot}.  If working interactively, restart the graphics
#' device before plotting again.
#' @method plot tag_co_occurrence
#' @export
#' @export plot.tag_co_occurrence
plot.tag_co_occurrence <- function(x, cor = FALSE, edge.weight = 8, node.weight=8,
    edge.color = "gray80", node.color = "orange", bar.color = node.color, font.color = "gray55",
    bar.font.color = ifelse(bar, "gray96", bar.color), background.color = NULL,
    bar.font.size = TRUE, node.font.size = 1.08, digits = 1, min.edge.cutoff = .15,
    plot.widths = c(.65, .35), bar = TRUE, type = "both", ...){

    x[["ave_tag"]] <- x[["ave_tag"]][x[["ave_tag"]][["tag"]] != "<<no tag>>", ]
    x[["ave_tag"]][["tag"]] <- factor(x[["ave_tag"]][["tag"]], levels=rev(x[["ave_tag"]][["tag"]]))

    if (isTRUE(bar.font.size)) {
        bar.font.size <- constrain(round((1/length(x[["ave_tag"]][["tag"]])) * 100), 2.5, 9)
    }

    ave_tags_plot <- ggplot2::ggplot(x[["ave_tag"]], ggplot2::aes_string(x="tag")) +
        {if (isTRUE(bar)) {
            ggplot2::geom_bar(stat = "identity", ggplot2::aes_string(y='ave'), fill=bar.color)
        } else {
            ggplot2::geom_point(stat = "identity", ggplot2::aes_string(y='ave'), size = 2, color=bar.color)
        }} +
        ggplot2::coord_flip() +
        ggplot2::scale_y_continuous(expand = c(0, 0), limits = c(0, max(x[["ave_tag"]][["ave"]]) * 1.01)) +
        ggplot2::geom_text(ggplot2::aes(label=f(ave, digits), y=.02), color=bar.font.color, size=bar.font.size, hjust=0) +
        ggplot2::labs(y = "Average Number of\nCo-Occurring Other Tags", x=NULL) +
        ggplot2::theme_minimal() +
        ggplot2::annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf, color = font.color)+
        ggplot2::annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf, color = font.color) +
        ggplot2::theme(
            panel.grid.minor = ggplot2::element_blank(),
            axis.text = ggplot2::element_text(color = font.color),
            axis.title = ggplot2::element_text(color = font.color, size=12)
        ) +
        {if (isTRUE(bar)) {
             ggplot2::theme(panel.grid.major = ggplot2::element_blank())
         } else {
             ggplot2::theme(panel.grid.major.x = ggplot2::element_blank())
        }}

    if (!is.null(background.color)){
        ave_tags_plot <- ave_tags_plot +
            ggplot2::theme(plot.background = ggplot2::element_rect(fill=background.color))
    }

    if (type == "bar") print(ave_tags_plot)#; return(invisible(ave_tags_plot))

    if (isTRUE(cor)){
        mat <- x[["min_max_adjacency"]]
    } else {
        mat <- x[["cor"]]
    }

    graph <- igraph::graph.adjacency(mat, weighted=TRUE, mode="lower")
    igraph::E(graph)$width  <- igraph::E(graph)$weight * edge.weight
    #igraph::V(graph)$node.size <- minmax_scale(x[["node_size"]]) + 1

    #Create figure window and layout
    widths <- 100*round(plot.widths/sum(plot.widths), 2)

    if (type == "network") {

        if (!is.null(background.color)){
            graphics::par(mar=c(1, 1, 1, 1), new = TRUE, bg=background.color)
        } else {
            graphics::par(mar=c(1, 1, 1, 1), new = TRUE)

        }
        igraph::plot.igraph(
            igraph::delete.edges(graph, igraph::E(graph)[ weight < min.edge.cutoff]),
            vertex.color = node.color,
            vertex.label.family = "sans",
            vertex.label.font = 1,
            vertex.label.cex = node.font.size,
            edge.color = edge.color,
            vertex.frame.color = NA,
            vertex.size = node.weight*(1+minmax_scale(x[["node_size"]])),
            vertex.label.color = font.color, ...
        )
        #return(invisible(graph))

    }

    if (type == "both") {
        #Draw base plot
        if (!is.null(background.color)){
            graphics::plot.new()
            graphics::par(mar=c(1, 1, 1, 1), new = TRUE, bg=background.color)
            graphics::layout(matrix(c(rep(1, widths[1]), rep(2, widths[2])), nrow = 1,  byrow = TRUE))
        } else {
            graphics::layout(matrix(c(rep(1, widths[1]), rep(2, widths[2])), nrow = 1,  byrow = TRUE))
            graphics::plot.new()
            graphics::par(mar=c(1, 1, 1, 1), new = TRUE)

        }

        igraph::plot.igraph(
            igraph::delete.edges(graph, igraph::E(graph)[ weight < min.edge.cutoff]),
            vertex.color = node.color,
            vertex.label.family = "sans",
            vertex.label.font = 1,
            vertex.label.cex = node.font.size,
            edge.color = edge.color,
            vertex.frame.color = NA,
            vertex.size = node.weight*(1+minmax_scale(x[["node_size"]])),
            vertex.label.color = font.color, ...
        )

        #Draw ggplot
        graphics::plot.new()
        vps <- gridBase::baseViewports()
        print(ave_tags_plot, vp = grid::vpStack(vps$figure, vps$plot))
    }
    #return(invisible(graph, ave_tags_plot))
}



