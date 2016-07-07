#' Plot Term Count as Correspondence Analysis
#'
#' A wrapper for the \pkg{ca}'s \code{ca} + \code{plot} or \code{plot3d.ca}
#' functions for plotting a simple correspondence analysis.
#'
#' @param x A termco object.
#' @param D3 logical.  If \code{TRUE} plots in 3-d.
#' @param \ldots  Other arguments passed to \code{plot} and \code{plot3d.ca}.
#' @return Plots a correspondence analysis.
#' @keywords correspondence ca
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
#' plot_ca(markers, D3 = FALSE)
#' \dontrun{
#' plot_ca(markers)
#' }
plot_ca <- function(x, D3 = TRUE, ...){
    val <- validate_term_count(x)
    if (!isTRUE(val)) stop("Not a termco object")
    y <- x[, attributes(x)[["term.vars"]], drop =FALSE]
    y <- as.data.frame(y, stringsAsFactors = FALSE)
    rownames(y) <- paste2(x[, attributes(x)[["group.vars"]], drop =FALSE])
    fit <- ca::ca(y)
    if (!isTRUE(D3)) {
        graphics::plot(fit, ...)
    } else {
        ca::plot3d.ca(fit, ...)
    }
}
