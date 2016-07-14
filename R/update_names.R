#' Rename a term_count Object's Term Columns
#'
#' Safely rename a \code{term_count} object's term columns and attributes.
#'
#' @param x A \code{term_count} object.
#' @param old A vector of the current names.
#' @param new A vector of new names corresponding to the order of \code{old} names.
#' @return Returns a renamed \code{term_count} object.
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
#'
#' update_names(markers, old = c('back_channels', 'summons'), new = c('bcs', 's'))
#' update_names(markers, old = c('person'), new = c('people'))
#' update_names(markers, old = c('person', 'back_channels', 'summons'), new = c('people', 'bcs', 's'))
#' attributes(update_names(markers, old = c('back_channels', 'summons'), new = c('bcs', 's')))
update_names <- function(x, old, new){

    y <- validate_term_count(x, FALSE)
    if (!isTRUE(y)) stop("`x` does not appear to be a valid `term_count` object.  Was the object altered after creation?")

    vars <- c(attributes(x)[["group.vars"]], attributes(x)[["term.vars"]])
    if(any(!old %in% vars)){
        stop("The following `old` names were not found:\n", paste(old[!old %in% vars], collapse=", "))
    }

    y <- data.table::copy(x)
    data.table::setnames(y, old, new)

    if (any(old %in% attributes(x)[["term.vars"]])){
        matches <- match(attributes(y)[["term.vars"]], old)
        attributes(y)[["term.vars"]][!is.na(matches)] <- new[matches[!is.na(matches)]]
    }

    if(any(old %in% attributes(x)[["group.vars"]])){
        matches <- match(attributes(y)[["group.vars"]], old)
       attributes(y)[["group.vars"]][!is.na(matches)] <- new[matches[!is.na(matches)]]
    }

    y[]
}
