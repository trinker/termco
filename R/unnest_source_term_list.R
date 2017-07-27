#' Format Term List as Individual Regexes
#'
#' For validation purposes it is useful to test an uncollapsed and unnested
#' term list to validate how well individual regexes are performing without
#' iterative hierarchical processing.  This function allows the user to read
#' in an external term list file and format as a term list object made up of the
#' individual regular expressions for each category.  The regular expression
#' maintain category names and are numbered sequentially.  This list can then be
#' modeled and passed to the validation tools to understand regular expression
#' performance that might not be clear when one builds a larger model containing
#' many tiers.  After validating, the user can use the information to rearrange
#' the regular expressions within categories at various tiers depending on the
#' level of confidence associated with a regex.
#'
#' @param file Path to external term list.
#' @param indices Indices of the elements to retain (used to take part of a
#' term list).
#' @param \ldots ignored.
#' @return Returns an unnested & uncollapsed term list.
#' @export
#' @examples
#' \dontrun{
#' library(dplyr)
#' library(textshape)
#'
#' good_cats <- system.file("termco_docs/categories.R", package = "termco")
#'
#' good_cats %>%
#'     unnest_source_term_list()
#'
#' good_cats %>%
#'     unnest_source_term_list() %>%
#'     tidy_list('category', 'regex')
#' }
unnest_source_term_list <- function (file, indices = NULL, ...) {

    trms <- unlist(source_term_list(file, indices = indices, ...), recursive=FALSE)
    out <- as.list(unlist(lapply(unique(names(trms)), function(x){unlist(trms[x])})))
    read_term_list(term.list = out)

}

