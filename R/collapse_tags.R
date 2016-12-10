#' Collapse \code{term_count} Tags
#'
#' Collapse (sum) tags/columns of a \code{term_count} object or remove columns
#' without changing termco class.
#'
#' @param x A \code{term_count} object.
#' @param mapping A list of named vectors where the vector names are the collapsed
#' column names and the vectors are the names of the columns to collapse.  Setting
#' a column name to \code{NULL} deletes these columns from the output.
#' @param \ldots ignored.
#' @return Returns a \code{term_count} object.
#' @export
#' @examples
#' mapping <- list(
#'     babbling = c('response_cries', 'back_channels'), #combines these columns
#'     NULL = 'justification'                           #remove this column(s)
#' )
#'
#' data(markers); markers
#' collapse_tags(markers, mapping)
collapse_tags <- function(x, mapping, ...){

    validate_term_count(x, TRUE)
    validate_mapping(mapping, x)

    if (methods::is(x, 'hierarchical_term_count')){
        cov <- coverage(x)
    } else {
        cov <- NULL
    }

    ## remove class
    x <- rm_class(x, "hierarchical_term_count")
    y <- attributes(x)

    ## remove columns if any NULL
    if(any(names(mapping) == "NULL")) {
        removes <- unlist(mapping[names(mapping) == "NULL"], use.names=FALSE)
        x <- x[colnames(x)[!colnames(x) %in% removes]]
        y[["term.vars"]] <- excluder(y[["term.vars"]], removes)
        mapping <- mapping[excluder(names(mapping), "NULL")]
    }

    ## remove one to one mapping
    if (any(names(mapping) == mapping)) {
        mapping <- mapping[names(mapping) != mapping]
    }

    ## combine columns
    if (length(mapping) > 0){
        collapses <- paste(Map(function(x, y){paste0(x, " = sum(", paste(y, collapse=", "),
            ")")}, names(mapping), mapping), collapse=", ")
        x <- eval(parse(text=paste0("dplyr::mutate(dplyr::rowwise(x), ", collapses, ")")))

        ## change term.vars attribute and remove columns
        removes <- unlist(mapping, use.names=FALSE)
        x <- x[excluder(colnames(x), excluder(removes, names(mapping)))]
        y[["term.vars"]] <- c(excluder(y[["term.vars"]], removes), names(mapping))
    }

    x <- dplyr::tbl_df(x)
    class(x) <- y[['class']]
    y <- y[c("group.vars", "term.vars", "weight", "pretty", "counts", "text.var", "model", "regex")]
    for (i in seq_along(y)){
        attributes(x)[[names(y)[i]]] <- y[[i]]
    }

    if (!is.null(cov)) {
        class(x) <- c("collapsed_hierarchical_term_count", class(x))
        attributes(x)[['pre_collapse_coverage']] <- cov
    }
    x
}


excluder <- function(x, y){
    x[!x %in% y]
}

validate_mapping <- function(mapping, x, ...){
    if (is.null(names(mapping))) stop(error("Name the vector of columns to collapse within the `mapping` list"))
    if (any(names(mapping) == "")) stop(error("Name all of the vectors of columns to collapse within the `mapping` list"))
    if (any(!unlist(lapply(mapping, is.atomic)))) stop(error("An element within `mapping` is not an atomic vector"))
    matches <- !unlist(mapping) %in% attributes(x)[["term.vars"]]
    if (any(matches)) {
        wrong_terms <- paste(unlist(mapping)[matches], collapse=", ")
        stop(paste0("The following column(s) listed in `mapping` were not found in the attribute `term.vars` from `x`:\n\n  - ",
            wrong_terms, "\n\nPlease remove from `mapping` or correct the typo."))
    }
    return(invisible(TRUE))
}


examp <- "\n\nHere is an example mapping:\n\n    example_map <- list(\n        cat = c('lion', 'cub', 'feline'),\n        dog = c('wolf', 'pup', 'clifford')\n    )\n"

error <- function(x, y = examp){
    paste0(x, y)
}


