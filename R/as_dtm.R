#' Convert a \code{term_count} Object Into a \code{DocumentTermMatrix}/\code{TermDocumentMatrix}
#'
#' Convenience functions to convert a \code{term_count} object into either a \code{tm::DocumentTermMatrix} or \code{tm::TermDocumentMatrix} object.  Grouping variables are used as the row/column names for the \code{DocumentTermMatrix}/\code{TermDocumentMatrix}.
#'
#' @param x A \code{term_count} object.
#' @param weighting A weighting function capable of handling a \code{tm::DocumentTermMatrix}. It defaults to \code{weightTf} for term frequency weighting. Available weighting functions shipped with the \pkg{tm} package are \code{weightTf}, \code{weightTfIdf}, \code{weightBin}, and \code{weightSMART}.
#' @param \ldots ignored.
#' @return Returns a \code{tm::DocumentTermMatrix} or \code{tm::TermDocumentMatrix} object.
#' @keywords documenttermmatrix, termdocumentmatrix
#' @export
#' @rdname as_dtm
#' @examples
#' as_dtm(markers)
#' as_dtm(markers,weighting = tm::weightTfIdf)
#' as_tdm(markers)
#'
#' cosine_distance <- function (x, ...) {
#'     x <- t(slam::as.simple_triplet_matrix(x))
#'     stats::as.dist(1 - slam::crossprod_simple_triplet_matrix(x)/(sqrt(slam::col_sums(x^2) %*%
#'         t(slam::col_sums(x^2)))))
#' }
#'
#'
#' mod <- hclust(cosine_distance(as_dtm(markers)))
#' plot(mod)
#' rect.hclust(mod, k = 5, border = "red")
#'
#' (clusters <- cutree(mod, 5))
as_dtm <- function(x, weighting = tm::weightTf, ...){
    UseMethod('as_dtm')
}


#' @export
#' @method as_dtm term_count
as_dtm.term_count <- function(x, weighting = tm::weightTf, ...){
    y <- as.matrix(term_cols(x))
    rownames(y) <- paste2(group_cols(x))
    tm::as.DocumentTermMatrix(slam::as.simple_triplet_matrix(y), weighting = weighting)
}

#' @export
#' @method as_dtm tbl_df
as_dtm.tbl_df <- function(x, weighting = tm::weightTf, ...){
    tm::as.DocumentTermMatrix(slam::as.simple_triplet_matrix(as.matrix(x)), weighting = weighting)
}


#' @export
#' @rdname as_dtm
as_tdm <- function(x, weighting = tm::weightTf, ...){
    UseMethod('as_tdm')
}

#' @export
#' @method as_tdm term_count
as_tdm.term_count <- function(x, weighting = tm::weightTf, ...){
    y <- as.matrix(term_cols(x))
    rownames(y) <- paste2(group_cols(x))
    tm::as.TermDocumentMatrix(slam::as.simple_triplet_matrix(t(y)), weighting = weighting)
}

#' @export
#' @method as_tdm tbl_df
as_tdm.tbl_df <- function(x, weighting = tm::weightTf, ...){

    tm::as.TermDocumentMatrix(slam::as.simple_triplet_matrix(as.matrix(x)), weighting = weighting)

}
