#' Generate List of Exploration \code{search_term} + \code{frequent_terms} + \code{plot} Function Calls
#'
#' \code{probe_colo_plot_list} - The task of determining the regexes used to
#' feed a \code{term_count} object's \code{term.list} requires careful
#' exploration of term use in context.  This function generates a list of
#' function calls for \code{search_term} + \code{frequent_terms} + \code{plot}
#' with a user predefined data set and term list.  This allows the user to use
#' bar plot explorations to explore a list of terms (such as from
#' \code{frequent_terms}) and the accompanying terms that frequently collocate
#' with these terms.
#'
#' @param terms A vector of regex terms to explore (often populated from
#' \code{frequent_terms}.
#' @param data.name A character vector of a data set's name that will serve as
#' the search context.
#' @param copy2clip logical.  If code{TRUE} uses \code{\link[clipr]{write_clip}}
#' to copy the output to the clipboard.  This option is most useful when trying
#' to build a list regular expression model for easy pasting between testing
#' a regex and putting it into the model.  This argument can be set globally by
#' setting \code{options(termco.copy2clip = TRUE)}.
#' @param ldots A string (starting with a comma) of additional arguments to
#' include in the \code{frequent_terms} function of the list of function calls.
#' @return \code[Vprobe_colo_plot_list} - Returns a string with the concatenated
#' function calls.  The print method separates the concatenated string into new
#' line function calls.  If \code{copy2clip = TRUE} the calls are easily pasted
#' for use in exploration of the terms in the text data set.
#' @note To actually make a \file{.pdf} file of the plots use the
#' \code{probe_colo_plot} function directly.  Also note that \code{probe_colo_plot_list}
#' takes a character name for \code{data.name} whereas \code{probe_colo_plot}
#' takes a an actual vector object for \code{data}.
#' @family probe functions
#' @export
#' @rdname probe_colo__plot
#' @examples
#' probe_colo_plot_list(c("thank", "\\bthe", "ee"), "sam_i_am")
#' probe_colo_plot_list(
#'     c("thank", "\\bthe", "ee"),
#'     "sam_i_am",
#'     ldots = ", n = 10, min.char = 5"
#' )
#'
#' txt <- presidential_debates_2012[["dialogue"]]
#' terms <- frequent_terms(txt)[["term"]]
#' probe_colo_plot_list(terms, "txt")
#'
#' \dontrun{
#' probe_colo_list(terms, "txt", copy2clip = TRUE)
#'
#' #make an external file of plots
#' probe_colo_plot(terms, txt)
#' }
probe_colo_plot_list <- function(terms, data.name, copy2clip = getOption("termco.copy2clip"), ldots = ""){

    terms <- stringi::stri_escape_unicode(terms)

    m <- paste(
        paste0('plot(search_term_collocations(',  data.name, ', "', terms, "\"", ldots, "))"),
        collapse = "\n"
    )
    if (isTRUE(copy2clip)) clipr::write_clip(m)
    class(m) <- c("probe_list", class(m))
    m
}


#' Generate List of Exploration \code{search_term} + \code{frequent_terms} + \code{plot} Function Calls
#'
#' \code{probe_colo_plot} - Make the plots of \code{probe_colo_plot_list}
#' directly to an external \file{.pdf} file.
#' @param data A vector of character strings.
#' @param file A  \file{.pdf} file to plot to.
#' @param width The width of the graphics region in inches.
#' @param height The height of the graphics region in inches.
#' @param \ldots Other arguments passed to frequent terms.
#' @family probe functions
#' @export
#' @rdname probe_colo__plot
probe_colo_plot <- function(terms, data, file = "Rplots.pdf", width = 5.5, height = 7, ...){

    terms <- stringi::stri_escape_unicode(terms)

    pdf(file=file, width = width, height = height)
    for(i in terms){
       print(plot(search_term_collocations(data, i, ...)) + ggplot2::ggtitle(i))
       Sys.sleep(1)
    }
    dev.off()
}
