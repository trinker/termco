#' Get a Text Stored in Various Objects
#'
#' Extract the text supplied to the
#' \code{\link[termco]{term_count}} object.
#'
#' @param x A \code{\link[termco]{term_count}} object.
#' @param \ldots ignored.
#' @return Returns a vector or list of text strings.
#' @export
#' @rdname get_text
#' @examples
#' library(dplyr)
#'
#' discoure_markers <- list(
#'     response_cries = c("\\boh\\b", "\\bah\\b", "\\baha", "\\bouch", "yuk"),
#'     back_channels = c("uh[- ]huh", "uhuh", "yeah"),
#'     summons = "\\bhey",
#'     justification = "because"
#' )
#'
#' model <- presidential_debates_2012 %>%
#'     with(term_count(dialogue, grouping.var = TRUE, discoure_markers))
#'
#' get_text(model, 'summons')
#' get_text(model, 'response_cries')
#' get_text(model, c('summons', 'response_cries'))
get_text <- function(x, ...){
    UseMethod("get_text")
}



#' @export
#' @rdname get_text
#' @method get_text term_count
get_text.term_count <- function(x, ...){
    attributes(x)[['text.var']][['text.var']][which(classify(x)%in% unlist(...))]
}

