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
#' \item{min_max_cor}{A min-max scaled correlation matrix between tags; diagonals set to 0.}
#' \item{min_max_adjacency}{A min-max scaled adjacency matrix between tags; diagonals set to 0.}
#' \item{node_size}{The diagonals from the adjacency matrix; the number of times a tag occurred.}
#' @export
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
#' heatmap(x[["min_max_cor"]])
#' heatmap(x[["min_max_adjacency"]])
#' barplot(sort(x[["node_size"]], TRUE), las=2)
#' barplot(setNames(x[["ave_tag"]][[2]], x[["ave_tag"]][[1]]), las=2)
#'
#' plot(x)
#' plot(x, cor=FALSE)
#' plot(x, min.edge.cutoff = .1, node.color = "#1CDB4F")
#' plot(x, bar = FALSE)
#'
#' ## Example 2
#' regs2 <- frequent_terms(presidential_debates_2012[["dialogue"]], n=40)[[1]]
#' regs2 <- setNames(as.list(regs2), regs2)
#'
#' model2 <- with(presidential_debates_2012,
#'     term_count(dialogue, TRUE, regs2)
#' )
#'
#' x2 <- tag_co_occurrence(model2)
#' plot(x2)
#' plot(x2, bar = FALSE, min.edge.cutoff = .13)
#' plot(x2, node.weight = 3)
#' plot(x2, edge.weight = 20, node.weight = 5)
tag_co_occurrence <- function(x, ...){

    tag <- NUL
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
        min_max_cor = cc, min_max_adjacency = adjmat, node_size = node_size)
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
#' @param bar.font.size A font size for the bar/dotplot (mean co-occurrences).
#' Default tries to calculate based on number of bars.
#' @param digits The number of digits to print for bar/dotplot font (mean
#' co-occurrences).
#' @param min.edge.cutoff A minimum value to use as a cut-off in the network plot.
#' If a value in the correlation/adjacency matrix is below this value, not egde
#' will be plotted for the tag (node) connection.
#' @param plot.widths A vector of proportions of length 2 and totalling 1
#' corresponding to the relative width of the network and  bar/dotplot.
#' @param bar logical.  If \code{TRUE} a bar plot is used as the second plot,
#' otherwise a dotplot is used.
#' @param \ldots Other arguments passed to \code{\link[igraph]{plot.igraph}}.
#' @method plot tag_co_occurrence
#' @export
plot.tag_co_occurrence <- function(x, cor = TRUE, edge.weight = 8, node.weight=8,
    edge.color = "gray80", node.color = "orange", bar.color = node.color, font.color = "gray55",
    bar.font.color = ifelse(bar, "gray96", bar.color), bar.font.size = TRUE, digits = 1,
    min.edge.cutoff = .15, plot.widths = c(.65, .35), bar = TRUE, ...){

    x[["ave_tag"]] <- x[["ave_tag"]][x[["ave_tag"]][["tag"]] != "<<no tag>>", ]
    x[["ave_tag"]][["tag"]] <- factor(x[["ave_tag"]][["tag"]], levels=rev(x[["ave_tag"]][["tag"]]))

    if (isTRUE(bar.font.size)) bar.font.size <- round((1/length(x[["ave_tag"]][["tag"]])) * 110)

    ave_tags_plot <- ggplot2::ggplot(x[["ave_tag"]], ggplot2::aes_string(x="tag")) +
        {if (isTRUE(bar)) {
            ggplot2::geom_bar(stat = "identity", ggplot2::aes_string(y='ave'), fill=bar.color)
        } else {
            ggplot2::geom_point(stat = "identity", ggplot2::aes_string(y='ave'), size = 2, color=bar.color)
        }} +
        ggplot2::coord_flip() +
        ggplot2::scale_y_continuous(expand = c(0, 0), limits = c(0, max(x[["ave_tag"]][["ave"]]) * 1.01)) +
        ggplot2::geom_text(ggplot2::aes(label=f(ave, digits), y=.02), color=bar.font.color, size=bar.font.size, hjust=0) +
        ggplot2::labs(y = "Average Number of Tags\nPer Text Element", x=NULL) +
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

    if (isTRUE(cor)){
        mat <- x[["min_max_adjacency"]]
    } else {
        mat <- x[["min_max_cor"]]
    }

    graph <- igraph::graph.adjacency(mat, weighted=TRUE, mode="lower")
    igraph::E(graph)$width  <- igraph::E(graph)$weight * edge.weight
    #igraph::V(graph)$node.size <- minmax_scale(x[["node_size"]]) + 1

    #Create figure window and layout
    graphics::plot.new()
    grid::grid.newpage()
    grid::pushViewport(grid::viewport(layout = grid::grid.layout(1, ncol=2, widths = grid::unit(plot.widths, "npc"))))

    #Draw base plot
    grid::pushViewport(grid::viewport(layout.pos.col = 1, width = grid::unit(.8, "npc")))
    graphics::par(fig = gridBase::gridFIG(), mar=c(1, 1, 1, 1), new = TRUE)

    igraph::plot.igraph(
        igraph::delete.edges(graph, igraph::E(graph)[ weight < min.edge.cutoff]),
        vertex.color = node.color,
        vertex.label.family = "sans",
        vertex.label.font = 1,
        vertex.label.cex = .8,
        edge.color = edge.color,
        vertex.frame.color = NA,
        vertex.size = node.weight*(1+minmax_scale(x[["node_size"]])),
        vertex.label.color = font.color, ...
    )

    grid::popViewport()

    #Draw ggplot
    grid::pushViewport(grid::viewport(layout.pos.col = 2, width = grid::unit(.2, "npc")))
    print(ave_tags_plot, newpage = FALSE)
    grid::popViewport()

}




