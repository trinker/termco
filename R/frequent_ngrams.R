#' Ngram Collocations
#'
#' Find a important ngram (2-3) collocations.  Wraps \code{\link[quanteda]{collocations}}
#' to provide stopword, min/max characters, and stemming with a generic \code{plot}
#' function.
#'
#' @param text.var A vector of character strings.
#' @param n The number of rows to include.
#' @param gram.length The length of ngram to generate (2-3).
#' @param stopwords A vector of stopwords to exclude.
#' @param min.char The minimum number of characters a word must be (including
#' apostrophes) for inclusion.
#' @param max.char The maximum number of characters a word must be (including
#' apostrophes) for inclusion.
#' @param order.by The name of the measure column to order by: \code{"frequency"},
#' \code{"lambda"}, \code{"z"}.
#' @param stem logical.  If \code{TRUE} the \code{\link[SnowballC]{wordStem}}
#' is used with \code{language = "porter"} as the default.  Note that stopwords
#' will be stemmed as well.
#' @param language The stem language to use (see  \code{\link[SnowballC]{wordStem}}).
#' @param \ldots Other arguments passed to \code{\link[quanteda]{collocations}}.
#' @return Retuns a data.frame of terms and frequencies.
#' @importFrom stopwords stopwords
#' @importFrom data.table := .SD .N
#' @keywords term word frequency
#' @seealso \code{\link[quanteda]{collocations}}
#' @export
#' @examples
#' \dontrun{
#' x <- presidential_debates_2012[["dialogue"]]
#'
#' frequent_ngrams(x)
#' frequent_ngrams(x, n = 50)
#' frequent_ngrams(x, stopwords = c(stopwords::stopwords("english"), "american", "governor"))
#' frequent_ngrams(x, gram.length = 3)
#' frequent_ngrams(x, gram.length = 3, stem = TRUE)
#' frequent_ngrams(x, order.by = "lambda")
#'
#' plot(frequent_ngrams(x))
#' plot(frequent_ngrams(x, n = 40))
#' plot(frequent_ngrams(x, order.by = "lambda"))
#' plot(frequent_ngrams(x, gram.length = 3))
#' }
#' \dontrun{
#' ## ngram feature extraction
#' if (!require("pacman")) install.packages("pacman")
#' pacman::p_load(termco, dplyr, textshape, magrittr)
#' 
#' ngrams <- presidential_debates_2012 %$%
#'     frequent_ngrams(dialogue, n=10) %>%
#'     pull(collocation) %>%
#'     as_term_list() 
#' 
#' 
#' ngram_features <- presidential_debates_2012 %>%
#'     with(term_count(dialogue, person, ngrams)) %>%
#'     as_dtm() 
#' 
#' ngram_features
#' 
#' ## tidied features
#' ngram_features %>%
#'     textshape::tidy_dtm()
#' }
frequent_ngrams <- function(text.var, n = 20, gram.length = 2:3,
    stopwords = stopwords::stopwords("english"), min.char = 4,
    max.char = Inf, order.by = "frequency", stem = FALSE, language = "porter", ...) {

    wc <- collocation <- id <- terms <- len <- low <- high <- sw <- keep <- NULL

    ## initial checks for: order.by column
    orders <- c("frequency", "lambda", "z")
    if (!order.by %in% orders) {
        stop(sprintf("`order.by` must be one of: %s", paste(paste0("\"", orders, "\""), collapse=", ")))
    }

    ## initial checks for: order.by column
    stopifnot(length(gram.length) %in% 1:2)

    ## stemming
    if (isTRUE(stem)) {
        text.var <- stringi::stri_split_regex(text.var, "[[:space:]]|(?!')(?=[[:punct:]])")
        lens <- sapply(text.var, length)
        text.var <- SnowballC::wordStem(unlist(text.var), language = language)
        if (! is.null(stopwords)) stopwords <- SnowballC::wordStem(stopwords, language = language)
        starts <- c(1, utils::head(cumsum(lens), -1) + 1)
        text.var <- sapply(Map(function(s, e) {text.var[s:e]}, starts, c(starts[-1] - 1, length(text.var))), paste, collapse = " ")

    }

    ## use quanteda to calculate collocations
    #y <- quanteda::collocations(text.var, size=gram.length, n=200000, method = "all", ...)
    y <- termco_collocations(text.var, size=gram.length, order.by = order.by,
        min.char = min.char, max.char = max.char, ...)

    # y[["keeps"]] <- rowSums(!is.na(y)) > 0
    # y <- y[which(keeps), ][, keeps := NULL]
    #
    # ## change names to be consistent w/ frequent_ngrams
    # names(y) <- gsub("^count$", "frequency", gsub("(^word)(\\d)", "term\\2", names(y)))

    # ## drop empty columns
    # keeps <- !sapply(y, function(x) all(x == ""))
    # keeps <- names(keeps)[keeps]
    # y <- y[, .SD, .SDcols=keeps]


    ## grabbing n rows
    if (n > nrow(y)) n <- nrow(y)
    y <- y[seq_len(n), ]

    class(y) <- c('frequent_ngrams', class(y))
    attributes(y)[["gram.length"]] <- gram.length
    y
}


#' Plots a frequent_ngrams Object
#'
#' Plots a frequent_ngrams object.
#'
#' @param x The \code{frequent_ngrams} object.
#' @param drop.redundant.yaxis.text logical.  If \code{TRUE} the second y axis text/ticks,
#' in the heat plot are dropped.
#' @param plot logical.  If \code{TRUE} the output is plotted.
#' @param \ldots ignored.
#' @return Returns a list of the three \pkg{ggplot2} objects that make the
#' combined plot.
#' @method plot frequent_ngrams
#' @export
plot.frequent_ngrams <- function(x, drop.redundant.yaxis.text = TRUE,
    plot = TRUE, ...){

    collocation <- Grams <- Method <- Scaled <- Measure <- NULL

    data.table::setDT(x)
    # x[, Grams := Reduce(function(...) paste(..., sep = "-"), .SD[, mget(termcols)])]
    x[, Grams := gsub(' ', '-', collocation, fixed = TRUE)][, collocation := NULL]
    x[["Grams"]] <- factor(x[["Grams"]], levels=rev(x[["Grams"]]))
    # x[, eval(parse(text=paste0("c(", paste(paste0("\"", termcols, "\""), collapse=", "), ")"))) := NULL]

    plot1 <- ggplot2::ggplot(x, ggplot2::aes_string(x='Grams', weight='frequency')) +
        ggplot2::geom_bar() +
        ggplot2::coord_flip() +
        ggplot2::ylab("Count") +
    	  ggplot2::scale_y_continuous(expand = c(0, 0), limits = c(0, 1.01 * max(x[["frequency"]]))) +
        ggplot2::theme_bw() +
        ggplot2::theme(
            panel.grid.major.y = ggplot2::element_blank(),
            #legend.position="bottom",
            legend.title = ggplot2::element_blank(),
            panel.border = ggplot2::element_blank(),
            axis.line = ggplot2::element_line(color="grey70")
        )

    x[, 'length' := NULL]

    ## coerce measures all to double
    cols <- colnames(x)[!colnames(x) %in% 'Grams']
    x[ , (cols) := lapply(.SD, as.numeric), .SDcols = cols][]

    dat_long <- data.table::melt(x[], id = c("Grams"),
        variable.name = "Method", value.name = "Measure")[,
        Grams := factor(Grams, levels = levels(x[["Grams"]]))][,
        Method := factor(Method, levels = utils::head(colnames(x), -1))][,
        Scaled := scale(Measure), by = "Method"][]

    heat_plot <- ggplot2::ggplot(dat_long,
        ggplot2::aes_string(y = "Grams", x = "Method", fill="Scaled")) +
        ggplot2::geom_tile() +
        ggplot2::scale_fill_gradient2(
            high="red",
            low="blue",
            space = "Lab"
        ) +
        ggplot2::ylab(NULL) +
        ggplot2::xlab("Measure") +
        ggplot2::theme_bw() +
        ggplot2::theme(
            panel.grid = ggplot2::element_blank(),
            axis.title.x = ggplot2::element_text(size=11),
            panel.border = ggplot2::element_rect(color="grey88")
        ) +
        ggplot2::guides(fill = ggplot2::guide_colorbar(barwidth = .5, barheight = 10))

    if (isTRUE(drop.redundant.yaxis.text)){
        heat_plot <- heat_plot +
            ggplot2::theme(
                axis.text.y = ggplot2::element_blank(),
                axis.ticks.y = ggplot2::element_blank()
            )
    }

    plotout <- gridExtra::arrangeGrob(plot1, heat_plot, ncol=2)
    if (isTRUE(plot)) gridExtra::grid.arrange(plotout)
    return(invisible(list(bar = plot1, heat = heat_plot)))
}


## have to filter out any that were less than n in length
termco_collocations <- function(x, size = 2:3, order.by = 'frequency',
    stopwords = stopwords::stopwords("english"), min.char = 4, max.char = Inf, ...){

    terms <- wc <- collocation <- id <- terms <- len <- low <- high <- sw <- keep <- NULL

    ## stopped tokenizing first because of error in quanteda
    tokens <- quanteda::tokens(tolower(x), remove_punct = TRUE, remove_symbols = TRUE)

    #meth <- c('lr', 'chi2', 'pmi', 'dice')
    meth <- 'lambda'

    ngrams <- lapply(meth, function(y) {
        quanteda.textstats::textstat_collocations(tokens, method = y, size = size)
    })

    out <- Reduce(function(x, y) dplyr::left_join(x, y, by=c('collocation', 'count', 'length')), ngrams)

    out <- data.table::data.table(out)

    if (order.by == 'frequency') order.by <- 'count'

    expr <- parse(text = paste0('order(-', order.by, ')'))

    out <- out[, wc := stringi::stri_count_words(collocation)][
        wc >= min(size) & wc <= max(size),][
        eval(expr), ][,
        wc := NULL][, id := 1:.N,][,
        collocation := as.character(collocation)][]

    sw <- data.table::setkey(data.table::data.table(terms = stopwords, sw = TRUE), "terms")

    cols <- out[,c('collocation', 'id'), with = FALSE]
    cols <- cols[,
        terms := stringi::stri_split_fixed(collocation, ' ')][,
        id := 1:.N][,
        collocation := NULL][,
        list(terms = unlist(terms)), by = 'id'][,
        len := nchar(terms)][,
        low := len < min.char][,
        high := len > max.char][]

    data.table::setkey(cols, 'terms')

    keeps <- sw[cols][, sw := ifelse(is.na(sw), FALSE, sw)][,
        list(sw = sum(sw), low = sum(low), high = sum(high)), by = 'id'][,
        keep := sw ==0 & low ==0 & high == 0,, by = 'id'][,
        .SD[all(keep)], by = id][, 'id', with = FALSE]

    data.table::setkey(keeps, 'id')
    data.table::setkey(out, 'id')

    out <- out[keeps, nomatch=0][, id := NULL][]
    data.table::setnames(out, "count","frequency")

    three <- c('collocation', 'length', 'frequency')

    data.table::setcolorder(out, c(three, colnames(out)[!colnames(out) %in% three]))

    out
}



