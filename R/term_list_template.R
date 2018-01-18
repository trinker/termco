#' Generate a Basic Term List Template
#'
#' Generates a basic template for a term list.  Optionally prints to an external file.
#'
#' @param categories A vector of categories (names corresponding to the regexes).
#' @param file Path to external term list.
#' @param hierarchical logical.  If \code{TRUE} the term list is made to be hierarchical.
#' @param copy2clip logical.  If code{TRUE} uses \code{\link[clipr]{write_clip}}
#' to copy the output to the clipboard.  This argument can be set globally by
#' setting \code{options(termco.copy2clip = TRUE)}.
#' @param \ldots ignored.
#' @export
#' @examples
#' cats <- c("Summons", "Justification", "Exclamation", "Empty")
#' term_list_template(cats)
#' term_list_template(cats, hierarchical = FALSE)
term_list_template <- function(categories = NULL, file = NULL, hierarchical = TRUE,
    copy2clip = getOption("termco.copy2clip"), ...) {

    if (is.null(copy2clip)) copy2clip <- FALSE

    if (isTRUE(hierarchical)) {spc <- '        '} else {spc <- '    '}

    if (!is.null(categories)) {
        categories <- paste(sprintf('%s%s = c()', spc, categories), collapse = ',\n')
    } else {
        categories <- ''
    }

    if (isTRUE(hierarchical)) {
        x <- sprintf(hierarchical_template, categories)
    } else {
        x <- sprintf(nonhierarchical_template, categories)
    }

    if (!is.null(file)) {
        cat(x, '\n', file = file)
    }

    if (isTRUE(copy2clip)) {
        clipr::write_clip(x)
    }

    ## print out object of class term_list_template
    class(x) <- 'term_list_template'

    x
}

#' Prints a term_list_template Object
#'
#' Prints a term_list_template object
#'
#' @param x A term_list_template object.
#' @param \ldots ignored.
#' @method print term_list_template
#' @export
print.term_list_template <- function(x, ...){
    cat(x, '\n')
}


hierarchical_template <- paste(c("list(", "## Tier 1", "    list(", "%s        ", "    ),",
"", "## Tier 2", "    list(", "", "    )", ")"), collapse = '\n')

nonhierarchical_template <- paste(c("list(", "%s", ")"), collapse = '\n')
