#' Make Regex to Locate Strings Containing Co-ocuring Substrings
#'
#' Make a regex to locate strings that contain >= 2 substrings with optional
#' negation.
#'
#' @param \ldots Terms that cooccur/collocate
#' @param not A substring to exclude from consideration.
#' @param copy2clip logical.  If code{TRUE} uses \code{\link[clipr]{write_clip}}
#' to copy the output to the clipboard.  This option is most useful when trying
#' to build a list regular expression model for easy pasting between testing
#' a regex and putting it into the model.  This argument can be set globally by
#' setting \code{options(termco.copy2clip = TRUE)}.
#' @return Returns a regular expression.  If Windows attempts to copy to
#' clipboard as well.
#' @keywords collocate cooccur
#' @export
#' @examples
#' \dontrun{
#' colo('overall', 'course')
#' colo('overall', 'course', "eval")
#' colo('overall', 'course', not="instructor")
#'
#' search_term(sam_i_am, colo("^i\\b", "like"))
#' search_term(sam_i_am, colo("^i\\b", "like", "not"))
#' search_term(sam_i_am, colo("^i\\b", "like|not"))
#' search_term(sam_i_am, colo("^i\\b", "like", not="not"))
#' }
colo <- function(..., not=NULL, copy2clip = getOption("termco.copy2clip")) {
    if (is.null(copy2clip)) copy2clip <- FALSE
    if (is.null(not)){
          if (length(substitute(...())) == 1) {
              if (isTRUE(copy2clip)) {
                  clipr::write_clip(paste0("\"", substitute(...())[[1]], "\""))
              }
              return(substitute(...())[[1]])
          }
    	  cooc(..., copy2clip = copy2clip)
    } else {
    	  cooc_not(..., not=not, copy2clip = copy2clip)
    }
}


cooc <- function(..., copy2clip){

    x <- substitute(...())

    if (length(x) == 2) {
        z <- sprintf("((%s.*%s)|(%s.*%s))", x[1], x[2], x[2], x[1])
    } else {
        z <- paste0("^", paste(sprintf("(?=.*%s)", x), collapse=""))
    }
    if (copy2clip) {
        z2 <- paste0("\"", z, "\"")
        clipr::write_clip(gsub("\\", "\\\\", z2, fixed=TRUE))
    }
    z
}



cooc_not <- function(..., not, copy2clip){
    x <- substitute(...())
    z <- paste0(sprintf("^(?!.*(%s))", not), paste(sprintf("(?=.*(%s))", x), collapse=""))
    if (copy2clip) {
        z2 <- paste0("\"", z, "\"")
        clipr::write_clip(gsub("\\", "\\\\", z2, fixed=TRUE))
    }
    z
}




