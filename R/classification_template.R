#' Generate a Basic Classification Analysis Template
#'
#' Generates a basic template for a classification using \pkg{termco} tools.
#'
#' @param path Path to classification template script.
#' @param file.ext A supported file extension for the text data.  This dictates
#' the packages used in the template for reading in data.  Currently,
#' '.csv' (\pkg{readr})and '.fst' (\pkg{fst}) are supported.  This can be
#' manually changed after the template is created.
#' @param categories.file A path to the categories model (term list) file.  Defaults
#' to \file{'models/categories.R'}.
#' @param verbose logical.  If \code{TRUE} a message is printed if the template
#' inclusion was successful.
#' @param \ldots ignored.
#' @export
#' @examples
#' \dontrun{
#' classification_template()
#' }
classification_template <- function(
    path = 'scripts/02_classification.R', file.ext = 'fst',
    categories.file = 'models/categories.R', verbose = TRUE, ...
){


    if (dirname(path) != '.' && !dir.exists(dirname(path))) {
        dir.create(dirname(path), recursive = TRUE)
    }

    if (path == Sys.getenv("R_HOME")) stop("path can not be `R_HOME`")
    if (file.exists(path)) {
        message(paste0("\"", path, "\" already exists:\nDo you want to overwrite?\n"))
        ans <- utils::menu(c("Yes", "No"))
        if (ans == "2") {
            stop("template write aborted")
        } else {
            unlink(path, recursive = TRUE, force = FALSE)
        }
    }

    script <- system.file("extra_docs/02_classification.R", package = "termco")

    read_package <- switch(file.ext,
        csv = 'readr',
        fst = 'fst',
        {
            warning('not a supported file.ext; either "csv" or "fst"\n Using \'csv\'; this can be manually changed after file is created')
            'csv'
        }
    )


    out <- textclean::mgsub(
        readLines(script),
        c('{{read_package}}', '{{read_file_type}}', '{{categories_file}}'),
        c(read_package, file.ext, categories.file)
    )

    cat(out, file = path, sep = '\n')


    if (file.exists(path) && verbose) {
        message(sprintf('\'%s\' exists!\nLooks like everything went according to plan!', path))
    }

    if (!file.exists(path)) {
        warning(sprintf('\'%s\' does not appear to exist\nLooks like something went wrong', path))
    }
}
