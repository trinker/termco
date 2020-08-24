#' Extract Terms from Relative Locations
#'
#' \code{term_before} - View the frequency of terms before a regex/term.
#'
#' @param text.var The text string variable.
#' @param term A regex term to provide the search position.
#' @param ignore.case logical.  If \code{FALSE}, the pattern matching is case
#' sensitive and if \code{TRUE}, case is ignored during matching.
#' @param \ldots ignored.
#' @return Returns a data.frame of terms and frequencies
#' @export
#' @rdname term_before
#' @examples
#' term_before(presidential_debates_2012$dialogue, 'president')
#' term_after(presidential_debates_2012$dialogue, 'president')
#' term_after(presidential_debates_2012$dialogue, 'oil')
#' term_first(presidential_debates_2012$dialogue)
#'
#' x <- term_before(presidential_debates_2012$dialogue, 'president')
#' plot(x)
#'
#' \dontrun{
#' library(dplyr); library(lexicon)
#'
#' pos_df_pronouns[['pronoun']][1:5] %>%
#'     lapply(function(x){
#'         term_after(presidential_debates_2012$dialogue, paste0("\\b", x, "\\b"))
#'     }) %>%
#'     setNames(pos_df_pronouns[['pronoun']][1:5])
#'
#' term_first(presidential_debates_2012$dialogue) %>%
#'     filter(!term %in% tolower(sw_dolch) & !grepl("'", term))
#' }
term_before <- function(text.var, term, ignore.case = TRUE, ...){
    regex <- paste0(
        ifelse(ignore.case, "(?i)", ""),
        '[A-Za-z\'-]+(?=,?\\s', term, ')'
    )
    trms <- stats::na.omit(unlist(stringi::stri_extract_all_regex(text.var, regex)))
    if (length(trms) == 0) return(NULL)
    if (ignore.case) trms <- tolower(trms)
    out <- tibble::tibble(textshape::tidy_table(as.table(sort(table(trms), TRUE)), "term", "frequency"))
    class(out) <- c("term_loc", class(out))
    out
}


#' Extract Terms from Relative Locations
#'
#' \code{term_after} - View the frequency of terms after a regex/term.
#'
#' @export
#' @rdname term_before
term_after <- function(text.var, term, ignore.case = TRUE, ...){
    regex <- paste0(
        ifelse(ignore.case, "(?i)", ""),
        '(?<=', term, ',?\\s)[A-Za-z\'-]+'
    )
    trms <- stats::na.omit(unlist(stringi::stri_extract_all_regex(text.var, regex)))
    if (length(trms) == 0) return(NULL)
    if (ignore.case) trms <- tolower(trms)
    out <- tibble::tibble(textshape::tidy_table(as.table(sort(table(trms), TRUE)), "term", "frequency"))
    class(out) <- c("term_loc", class(out))
    out
}

#' Extract Terms from Relative Locations
#'
#' \code{term_first} - View the frequency of terms starting each string.
#'
#' @export
#' @rdname term_before
term_first <- function(text.var, ignore.case = TRUE, ...){
    regex <- paste0(ifelse(ignore.case, "(?i)", ""), '^[A-Za-z\'-]+')
    trms <- stats::na.omit(unlist(stringi::stri_extract_all_regex(text.var, regex)))
    if (length(trms) == 0) return(NULL)
    if (ignore.case) trms <- tolower(trms)
    out <- tibble::tibble(textshape::tidy_table(as.table(sort(table(trms), TRUE)), "term", "frequency"))
    class(out) <- c("term_loc", class(out))
    out
}



#' Plots a term_loc Object
#'
#' Plots a term_loc object.
#'
#' @param x The \code{term_loc} object.
#' @param as.cloud logical.  If \code{TRUE} a wordcloud will be plotted rather
#' than a bar plot.
#' @param random.order logical.  Should the words be place randomly around the
#' cloud or if \code{FALSE} the more frequent words are in the center of the cloud.
#' @param rot.per The precentage of rotated words.
#' @param \ldots Other arguments passed to \code{\link[wordcloud]{wordcloud}}.
#' @method plot term_loc
#' @export
plot.term_loc <- function(x,  as.cloud = FALSE, random.order = FALSE,
    rot.per = 0, ...){

    if (isTRUE(as.cloud)) {
        wordcloud::wordcloud(x[[1]], x[[2]], random.order = random.order,
            rot.per = rot.per, ...)
    } else {

        x[["term"]] <- factor(x[["term"]], levels=rev(x[["term"]]))

        ggplot2::ggplot(x, ggplot2::aes_string(x='term', weight='frequency')) +
            ggplot2::geom_bar() +
            ggplot2::coord_flip() +
            ggplot2::ylab("Count") +
            ggplot2::xlab("Terms") +
    	      ggplot2::scale_y_continuous(expand = c(0, 0),
    	          labels = function(x) format(x, big.mark = ",", scientific = FALSE, trim = TRUE),
    	          limits = c(0, 1.01 * x[1, "frequency"][[1]])) +
            ggplot2::theme_bw() +
            ggplot2::theme(
            panel.grid.major.y = ggplot2::element_blank(),
            #legend.position="bottom",
            legend.title = ggplot2::element_blank(),
            panel.border = ggplot2::element_blank(),
            axis.line = ggplot2::element_line(color="grey70")
        )
    }

}


