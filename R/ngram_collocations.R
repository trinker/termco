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
#' \code{"G2"}, \code{"X2"}, \code{"pmi"}, \code{"dice"}.
#' @param stem logical.  If \code{TRUE} the \code{\link[SnowballC]{wordStem}}
#' is used with \code{language = "porter"} as the default.  Note that stopwords
#' will be stemmed as well.
#' @param language The stem language to use (see  \code{\link[SnowballC]{wordStem}}).
#' @param \ldots Other arguments passed to \code{\link[quanteda]{collocations}}.
#' @return Retuns a data.frame of terms and frequencies.
#' @importFrom tm stopwords
#' @importFrom data.table := .SD .N
#' @keywords term word frequency
#' @seealso \code{\link[quanteda]{collocations}}
#' @export
#' @examples
#' \dontrun{
#' x <- presidential_debates_2012[["dialogue"]]
#'
#' ngram_collocations(x)
#' ngram_collocations(x, n = 50)
#' ngram_collocations(x, stopwords = c(tm::stopwords("en"), "american", "governor"))
#' ngram_collocations(x, gram.length = 3)
#' ngram_collocations(x, gram.length = 3, stem = TRUE)
#' ngram_collocations(x, order.by = "dice")
#'
#' plot(ngram_collocations(x))
#' plot(ngram_collocations(x, n = 40))
#' plot(ngram_collocations(x, order.by = "dice"))
#' plot(ngram_collocations(x, gram.length = 3))
#' }
ngram_collocations <- function(text.var, n = 20, gram.length = 2:3,
    stopwords = tm::stopwords("en"), min.char = 4,
    max.char = Inf, order.by = "frequency", stem = FALSE, language = "porter", ...) {

    wc <- collocation <- id <- terms <- len <- low <- high <- sw <- keep <- NULL

    ## initial checks for: order.by column
    orders <- c("frequency", "G2", "X2", "pmi", "dice")
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
    y <- termco_collocations(text.var, size=gram.length, order.by = order.by, ...)

    # y[["keeps"]] <- rowSums(!is.na(y)) > 0
    # y <- y[which(keeps), ][, keeps := NULL]
    #
    # ## change names to be consistent w/ ngram_collocations
    # names(y) <- gsub("^count$", "frequency", gsub("(^word)(\\d)", "term\\2", names(y)))

    # ## drop empty columns
    # keeps <- !sapply(y, function(x) all(x == ""))
    # keeps <- names(keeps)[keeps]
    # y <- y[, .SD, .SDcols=keeps]


    ## grabbing n rows
    if (n > nrow(y)) n <- nrow(y)
    y <- y[seq_len(n), ]

    class(y) <- c('ngram_collocations', class(y))
    attributes(y)[["gram.length"]] <- gram.length
    y
}


#' Plots a ngram_collocations Object
#'
#' Plots a ngram_collocations object.
#'
#' @param x The \code{ngram_collocations} object.
#' @param drop.redundant.yaxis.text logical.  If \code{TRUE} the second y axis text/ticks,
#' in the heat plot are dropped.
#' @param plot logical.  If \code{TRUE} the output is plotted.
#' @param \ldots ignored.
#' @return Returns a list of the three \pkg{ggplot2} objects that make the
#' combined plot.
#' @method plot ngram_collocations
#' @export
plot.ngram_collocations <- function(x, drop.redundant.yaxis.text = TRUE,
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


    dat_long <- data.table::melt(x, id = c("Grams"),
        variable.name = "Method", value.name = "Measure")[,
        Grams := factor(Grams, levels = levels(x[["Grams"]]))][,
        Method := factor(Method, levels = utils::head(colnames(x), -1))][,
        Scaled := scale(Measure), by = "Method"]

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
    stopwords = tm::stopwords("en"), min.char = 4, max.char = Inf, ...){

    terms <- wc <- collocation <- id <- terms <- len <- low <- high <- sw <- keep <- NULL

    tokens <- quanteda::tokens(tolower(x), remove_punct = TRUE, remove_symbols = TRUE)

    ngrams <- lapply(c('lr', 'chi2', 'pmi', 'dice'), function(x){
        quanteda::textstat_collocations(tokens, method = x, max_size = max(size),
            min_count = 2)
    })

    out <- Reduce(function(x, y) dplyr::left_join(x, y, by=c('collocation', 'count')), ngrams)

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
    out
}



