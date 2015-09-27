#' Model Accuracy
#'
#' Check a model's tagging/categorizing accuracy against known expert coded
#' outcomes.
#'
#' @param x The model classification \code{\link[base]{list}}/\code{\link[base]{vector}}
#' (typically the results of \code{classify}).
#' @param known The known expert coded \code{\link[base]{list}}/\code{\link[base]{vector}} of outcomes.
#' @return Returns a list of five elements:
#' \item{exact}{A numeric vector between 0-1 (0 no match; 1 perfect match) comparing \code{x} to \code{known} for exact matching.}
#' \item{any.in}{A numeric vector between 0-1 (0 no match; 1 perfect match) comparing \code{x} to \code{known} for non-location specific matching (\code{\%in\%} is used).  This ignores the differences in length between \code{x} and \code{known}.}
#' \item{logical}{A logical version of \code{exact} with \code{TRUE} being equal to 1 and all else being \code{FALSE}.  This can be used to locate perfect and/or non matches.}
#' \item{prop.correct}{The proportion of \code{logical} equal to \code{TRUE}.}
#' \item{adj.score}{An adjusted score of \code{prop.correct} that considers partial matching for when multiple tags are assigned.  This averages the results of \code{exact} and \code{any.in}.}
#' @keywords accuracy model fit
#' @export
#' @examples
#' library(dplyr)
#' data(presidential_debates_2012)
#'
#' discoure_markers <- list(
#'     response_cries = c("oh", "ah", "aha", "ouch", "yuk"),
#'     back_channels = c("uh[- ]huh", "uhuh", "yeah"),
#'     summons = "hey",
#'     justification = "because"
#' )
#'
#' ## Only Single Tag Allowed Per Text Element
#' mod1 <- presidential_debates_2012 %>%
#'     with(., term_count(dialogue, TRUE, discoure_markers)) %>%
#'     classify()
#'
#' fake_known <- mod1
#' set.seed(1)
#' fake_known[sample(1:length(fake_known), 300)] <- "random noise"
#'
#' accuracy(mod1, fake_known)
#'
#' ## Multiple Tags Allowed
#' mod2 <- presidential_debates_2012 %>%
#'     with(., term_count(dialogue, TRUE, discoure_markers)) %>%
#'     classify(n = 2)
#'
#' fake_known2 <- mod2
#' set.seed(30)
#' fake_known2[sample(1:length(fake_known2), 500)] <- c("random noise", "back_channels")
#'
#' accuracy(mod2, fake_known2)
accuracy <- function(x, known){

    stopifnot(length(x) == length(known))

    if (!is.list(x)) x <- as.list(x)
    if (!is.list(known)) known <- as.list(known)
    x <- lapply(x, function(a) sapply(a, function(b) {b[is.na(b)] <- "No_Code_Given"; b}))
    known <- lapply(known, function(a) sapply(a, function(b) {b[is.na(b)] <- "No_Code_Given"; b}))

    out <- acc_test(x, known)
    logic <- out[["exact"]] == 1
    propcor <- sum(logic)/length(logic)
    score <- mean(c(
        sum(out[["exact"]])/length(out[["exact"]]),
        sum(out[["any.in"]])/length(out[["any.in"]])
    ))
    out <- list(exact = unname(out[[1]]), any.in = unname(out[[2]]),
        logical = logic, prop.correct = propcor, adj.score = score)
    class(out) <- "accuracy"
    out

}

#' Prints an accuracy Object
#'
#' Prints an accuracy object
#'
#' @param x The accuracy object.
#' @param \ldots ignored
#' @method print accuracy
#' @export
print.accuracy <- function(x, ...){

    cat(sprintf("N:        %s\n", length(x[["logical"]])))
    cat(sprintf("Correct:  %s%%\n", digit_format(100*x[["prop.correct"]], 1)))
    cat(sprintf("Adjusted: %s\n", digit_format(100*x[["adj.score"]], 1)))

}

acc_test <- function(x, y){

    out <- unlist(Map(function(a, b){dists(a, b)}, x, y))
    out2 <- unlist(Map(function(a, b){dists2(a, b)}, x, y))
    #1-(((1 - (1/(1 + exp(out)))) * 2) - 1)
    list(exact = out, any.in = out2)
}



dists <- function(x, y) {

    #nas <- unlist(lapply(list(x, y), function(z){
    #    any(sapply(z, is.na))
    #}))

    #if(isTRUE(all(nas))) return(1)
    #if(isTRUE(any(nas))) return(0)
#if (any(is.na(y))) browser()
    suppressWarnings(sum(x == y)/(.5*(length(x) + length(y))))
}

dists2 <- function(x, y) {

    #nas <- unlist(lapply(list(x, y), function(z){
    #    any(sapply(z, is.na))
    #}))

    #if(isTRUE(all(nas))) return(1)
    #if(isTRUE(any(nas))) return(0)

    sum(x %in% y)/(.5*(length(x) + length(y)))
}












