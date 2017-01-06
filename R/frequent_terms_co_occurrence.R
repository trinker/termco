#' Plot Co-Occurrence of Frequent Terms
#'
#' Generate a \code{\link[termco]{tag_co_occurrence}} object from frquent terms.
#'
#' @param x A vector of character strings.
#' @param bound ligical.  If \code{TRUE} each side of the frequent term is
#' wrapped with a word boundary before performing thre regex search.  Otherwise,
#' the search is fuzzy matched.
#' @param \ldots Other arguments passed to \code{\link[termco]{frequent_terms}}.
#' @return Returns a \code{\link[termco]{tag_co_occurrence}} object from frequent terms.
#' @export
#' @examples
#' \dontrun{
#' frequent_terms_co_occurrence(presidential_debates_2012[["dialogue"]])
#' frequent_terms_co_occurrence(presidential_debates_2012[["dialogue"]], bound = FALSE)
#'
#' x <- frequent_terms_co_occurrence(presidential_debates_2012[["dialogue"]], n=50)
#' x
#' plot(x, min.edge.cutoff = .1, node.color = "gold")
#' plot(x, min.edge.cutoff = .075, node.color = "#1CDB4F")
#'
#' ## Load Required Add-on Packages
#' if (!require("pacman")) install.packages("pacman")
#' pacman::p_load(igraph, qrage)
#' pacman::p_load_gh("mattflor/chorddiag", "trinker/textshape")
#'
#' ## Matrix Manipulation Function
#' remove_diags <- function(mat, rm.lower = FALSE, order = TRUE, ...) {
#'     diag(mat) <- 0
#'     if (isTRUE(rm.lower)) mat[lower.tri(mat)] <- 0
#'     if (order) {
#'         ord <- order(rowSums(mat))
#'         mat <- mat[ord, ord]
#'     }
#'     mat
#' }
#'
#' ##--------------
#' ## Chord Diagram
#' ##--------------
#' chorddiag::chorddiag(
#'     remove_diags(x[["adjacency"]]),
#'     margin = 150,
#'     showTicks =FALSE,
#'     groupnamePadding = 5,
#'     groupThickness = .05,
#'     chordedgeColor = NA
#' )
#'
#' add_diags <- function(x, y, ...){
#'     diag(x) <- y
#'     x
#' }
#'
#' order_tags <- function(x, ...){
#'     ord <- order(rowSums(x))
#'     x[ord, ord]
#' }
#'
#' remove_lower <- function(x, ...){
#'     x[lower.tri(x)] <- 0
#'     x
#' }
#'
#'
#' chorddiag::chorddiag(
#'     add_diags(x[["adjacency"]],x[["node_size"]])  %>% order_tags() %>% remove_lower(),
#'     margin = 150,
#'     showTicks =FALSE,
#'     groupnamePadding = 5,
#'     groupThickness = .05,
#'     chordedgeColor = NA
#' )
#'
#' chorddiag::chorddiag(
#'     x[["adjacency"]] %>% order_tags() %>% remove_lower(),
#'     margin = 150,
#'     showTicks =FALSE,
#'     groupnamePadding = 5,
#'     groupThickness = .05,
#'     chordedgeColor = NA
#' )
#'
#' ##--------------
#' ## Network Graph
#' ##--------------
#' graph <- igraph::graph.adjacency(
#'     remove_diags(x[["adjacency"]], order=FALSE),
#'     weighted = TRUE
#' )
#'
#' linkdf <- stats::setNames(get.data.frame(graph), c("source", "target", "value"))
#'
#' qrage::qrage(
#'     links = linkdf,
#'     nodeValue = textshape::tidy_vector(x[['node_size']]),
#'     cut = 0.1
#' )
#' }
frequent_terms_co_occurrence <- function(x, bound = TRUE, ...){


    term_list <- as.list(frequent_terms(x, ...)[['term']])
    names(term_list) <- unlist(term_list)
    if (isTRUE(bound)) term_list <- lapply(term_list, function(x) paste0("\\b", x, "\\b"))
    cnts <- term_count(x, grouping.var = TRUE, term.list = term_list)
    out <- tag_co_occurrence(cnts, ...)

    class(out) <- c("frequent_terms_co_occurrence", class(out))

    out
}



#' Prints a frequent_terms_co_occurrence Object
#'
#' Prints a frequent_terms_co_occurrence object
#'
#' @param x A frequent_terms_co_occurrence object
#' @param \ldots Other arguments passed to \code{\link[termco]{plot.tag_co_occurrence}}.
#' @method print frequent_terms_co_occurrence
#' @export
print.frequent_terms_co_occurrence <- function(x, ...){

    graphics::plot(x, ...)

}
