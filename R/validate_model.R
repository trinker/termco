#' Manual Assessment of a Model
#'
#' \code{validate_model} - Check how well a regex model is tagging using human
#' interaction to assess the model.
#'
#' @param x A \code{term_count} model object (i.e., \code{grouping.var = TRUE}
#' was used in \code{term_count}).
#' @param n The number of samples to take from each regex tag assignment.  Tags
#' with less than \code{n} will use the full number available.
#' @param width The width of the text display.
#' @param tags The number of classifications per row/element to allow.  Ties are
#' broken probabilistically by default.
#' @param \ldots Other arguments passed to \code{\link[termco]{classify}}.
#' @return \code{validate_model} - Returns a \code{data.frame} of the class
#' \code{'validate_model'}.  Note that the pretty print is a tag summarized
#' version of the model accuracy standard error, and confidence intervals.
#' @keywords validate
#' @note This function assigns tags using the \code{\link[termco]{classify}}
#' function.  One element may recieve multiple tags.
#' @export
#' @rdname validate_model
#' @examples
#' \dontrun{
#' data(presidential_debates_2012)
#'
#' discoure_markers <- list(
#'     response_cries = c("\\boh", "\\bah", "\\baha", "\\bouch", "yuk"),
#'     back_channels = c("uh[- ]huh", "uhuh", "yeah"),
#'     summons = "hey",
#'     justification = "because"
#' )
#'
#' ## A model (note: `grouping.var = TRUE` to make a model)
#' (x <- with(presidential_debates_2012,
#'     term_count(dialogue, grouping.var = TRUE, term.list = discoure_markers)
#' ))
#'
#' ## Requires interaction
#' out <- validate_model(x)
#' out
#' plot(out)
#'
#' ## Assign tasks externally
#' assign_validation_task(x, checks = 3,
#'     coders = c('fred', 'jade', 'sally', 'jim', 'shelly'), out='testing')
#' assign_validation_task(x, checks = 3,
#'     coders = c('fred', 'jade', 'sally', 'jim', 'shelly'), as.list = FALSE,
#'     out='testing2')
#' }
validate_model <- function(x, n = 20, width = 50, tags = 1, ...){

    terms <- ifelse(inherits(x, 'token_count'), "token.vars", "term.vars")
    type <- ifelse(inherits(x, 'token_count'), "token", "term")

    if (!'term_count' %in% class(x)) {
        stop("`x` does not appear to be a '", type, "_count' object")
    }
    if (!attributes(x)[["model"]]) {
        stop(paste0("`x` does not appear to be a 'model'; use `grouping.var =TRUE` in `", type, "_count` to create a model"))
    }

    text.var <- attributes(x)[["text.var"]][["text.var"]]

    assigned_tags <- classify(x, n = tags, ...)
    potentials <- lapply(attributes(x)[[terms]], function(x) {
        which(  unlist(lapply(assigned_tags, function(y) x %in% y)))
    })
    names(potentials) <- attributes(x)[[terms]]
    # changed to include classify on 10/27/2016
    # potentials <- apply(x[, attributes(x)[["term.vars"]], drop = FALSE], 2, function(x) which(x > 0))

    items <- textshape::tidy_list(lapply(potentials, function(x){
        sample(x, ifelse(length(x) <= n, length(x), n))
    }), "tag", "index")

    results <- Map(tag_assessment, text.var[items[[2]]], items[[1]], seq_along(items[[1]]), length(items[[1]]), width = width)

    out <- data.frame(tag = items[[1]], correct = 2 - as.numeric(results), stringsAsFactors = FALSE)

    text <- new.env(hash = FALSE)
    text[["text.var"]] <- text.var

    class(out) <- c('validate_model', class(out))

    attributes(out)[["text.var"]] <- text
    attributes(out)[["indices"]] <- items[[2]]
    attributes(out)[['tag.counts']] <- textshape::tidy_table(table(unlist(as_terms(x))), 'tag', 'n')
    out
}

#' Summary of an validate_model Object
#'
#' Summary of an validate_model object
#'
#' @param object An validate_model object.
#' @param adjust.discrete logical.  Should an additional ammount be deducted
#' from the limits to account for dicrete data
#' @param ordered logical.  If \code{TRUE} the rows are ordered by tag accuracy.
#' @param \ldots ignored.
#' @references \url{http://onlinestatbook.com/2/estimation/proportion_ci.html}
#' @method summary validate_model
#' @export
summary.validate_model <- function(object, adjust.discrete = FALSE, ordered = TRUE, ...){

    tag <- accuracy <- NULL

    dat <- data.table::setDT(data.table::copy(object))
    out <- textshape::tidy_list(invisible(lapply(split(dat[[2]], dat[[1]]),
        proportion_confidence, adjust.discrete = adjust.discrete)), 'tag')

    out <- out[attributes(object)[['tag.counts']], on = 'tag']
    data.table::setcolorder(out, c("tag", "accuracy", "n", "sampled", "se", "lower", "upper"))

    if (isTRUE(ordered)) out <- out[order(-accuracy, na.last=TRUE)]
    out <- out[, 'tag' := factor(tag, levels = tag)][]

    class(out) <- c('summary.validate_model', class(out))
    ovrall <- proportion_confidence(dat[["correct"]], adjust.discrete = adjust.discrete)
    ovrall[['n']] <- sum(attributes(object)[['tag.counts']][['n']], na.rm = TRUE)
    attributes(out)[["overall"]] <- data.table::setcolorder(ovrall, c("accuracy", "n", "sampled", "se", "lower", "upper"))
    out
}

#' Prints a summary.validate_model Object
#'
#' Prints a summary.validate_model object
#'
#' @param x A summary.validate_model object.
#' @param digits The number of digits to display n percents.
#' @param \ldots ignored.
#' @method print summary.validate_model
#' @export
print.summary.validate_model <- function(x, digits = 1, ...){

    accuracy <- NULL

    lower <- upper <- se <- tag <- NULL
    cat(paste0(paste(rep("-", 7), collapse=""), "\n"))
    cat("Overall:\n")
    cat(paste0(paste(rep("-", 7), collapse=""), "\n"))
    print(data.table::data.table(attributes(x)[['overall']])[,
        'accuracy' := pp(100*accuracy, digits = digits)][,
        'lower' := pp(100*lower, digits = digits)][,
        'upper' := pp(100*upper, digits = digits)][,
        'se' := f(se, digits = digits + 1)][])

    cat("\n\n")
    cat(paste0(paste(rep("-", 15), collapse=""), "\n"))
    cat("Individual Tags:\n")
    cat(paste0(paste(rep("-", 15), collapse=""), "\n"))
    print(data.table::data.table(x)[, 'accuracy' := pp(100*accuracy, digits = digits)][,
        'lower' := pp(100*lower, digits = digits)][,
        'upper' := pp(100*upper, digits = digits)][,
        'se' := f(se, digits = digits + 1)][])
}


#' Prints a validate_model Object
#'
#' Prints a validate_model object
#'
#' @param x A validate_model object.
#' @param digits The number of digits to display n percents.
#' @param \ldots ignored.
#' @method print validate_model
#' @export
print.validate_model <- function(x, digits = 1, ...){
    print(summary(x, digits = digits, ...))
}

#' Plots a validate_model Object
#'
#' Plots a validate_model object
#'
#' @param x A validate_model object.
#' @param digits The number of digits to display n percents.
#' @param size The size of error bars.
#' @param height The height of error bars.
#' @param \ldots ignored.
#' @method plot validate_model
#' @export
plot.validate_model <- function(x, digits = 1, size = .65, height = .3, ...){

    overall <- tag <- NULL

    dat1 <- data.table::data.table(attributes(summary(x))[['overall']])[,
        'tag' := 'Model'][]

    dat <- summary(x)[, 'tag' := factor(tag, levels = rev(tag))][]

    dat2 <- rbind(dat1, dat)[,
        'tag' := factor(tag, levels = c('Model', levels(dat[['tag']])))][][,
        overall := factor(ifelse(tag == 'Model', 'Overall', 'Tags'), levels = c('Overall', 'Tags'))][]


    plot1a <- ggplot2::ggplot(dat2[dat2[['overall']] != 'Tags', ],
            ggplot2::aes_string(x = 'accuracy', y = 'tag',
            xmin = 'lower', xmax = 'upper')) +
        ggplot2::geom_vline(xintercept = .5, linetype='dashed', size = .9, color='blue', alpha = .2) +
        ggplot2::geom_errorbarh(size = size, height = height,  ggplot2::aes_string(color='overall')) +
        ggplot2::geom_point(ggplot2::aes_string(size='overall', shape='overall', color='overall')) +
        ggplot2::scale_x_continuous(label=function(x) {paste0(round(x, 2) * 100, "%")},
            limits = c(min(0, min(dat[['lower']])), max(1, max(dat[['upper']]))),
            breaks = c(0, .25, .5, .75, 1)) +
        ggplot2::facet_grid(overall~., scales='free', space='free') +
        ggplot2::labs(x = "Accuracy", y = NULL, title="Overall Model Tagging Accuracy") +
        ggplot2::theme_bw() +
        ggplot2::scale_color_manual(values=c("blue", "grey60")) +
        ggplot2::scale_shape_manual(values=c(18, 15)) +
        ggplot2::scale_size_manual(values=c(4, 3)) +
        ggplot2::theme(
            legend.position="none",
            strip.text.y = ggplot2::element_text(angle = 0),
            axis.ticks.x = ggplot2::element_blank(),
            axis.text.x = ggplot2::element_blank()
        ) +
        ggplot2::xlab(NULL)

    plot1b <- ggplot2::ggplot(dat2[dat2[['overall']] == 'Tags', ],
            ggplot2::aes_string(x = 'accuracy', y = 'tag',
            xmin = 'lower', xmax = 'upper')) +
        ggplot2::geom_vline(xintercept = .5, linetype='dashed', size = .9, color='blue', alpha = .2) +
        ggplot2::geom_errorbarh(size = size, height = height,  ggplot2::aes_string(color='overall')) +
        ggplot2::geom_point(ggplot2::aes_string(size='overall', shape='overall', color='overall')) +
        ggplot2::scale_x_continuous(label=function(x) {paste0(round(x, 2) * 100, "%")},
            limits = c(min(0, min(dat[['lower']])), max(1, max(dat[['upper']]))),
            breaks = c(0, .25, .5, .75, 1)) +
        ggplot2::facet_grid(overall~., scales='free', space='free') +
        ggplot2::labs(x = "Accuracy", y = NULL, title="Tag Accuracy") +
        ggplot2::theme_bw() +
        ggplot2::scale_color_manual(values=c("blue", "grey60")) +
        ggplot2::scale_shape_manual(values=c(18, 15)) +
        ggplot2::scale_size_manual(values=c(4, 3)) +
        ggplot2::theme(
            legend.position="none",
            strip.text.y = ggplot2::element_text(angle = 0)
        )

     maxy <-c(0, ceiling(max(dat2[dat2[['overall']] == 'Tags', ][['n']],
            na.rm = TRUE))*1.05)

     cntsplot <- ggplot2::ggplot(dat2[dat2[['overall']] == 'Tags', ],
            ggplot2::aes_string(y = 'tag')) +
        ggplot2::geom_segment(size = 1, color= 'grey60', ggplot2::aes_string(x = 0, xend = 'n', yend = 'tag')) +
        ggplot2::geom_point(stat = 'identity', ggplot2::aes_string( x = 'n'), size = 3, color = 'orange') +
        ggplot2::facet_grid(overall~., scales='free', space='free') +
        ggplot2::labs(y = NULL, x = "Count", title="Tag Counts") +
        ggplot2::scale_x_continuous(limits = maxy, expand = c(0, 0)) +
        ggplot2::theme_bw() +
        ggplot2::theme(
            legend.position="none",
            strip.text.y = ggplot2::element_text(angle = 0)
        )

    gA <- ggplot2::ggplotGrob(plot1a)
    gB <- ggplot2::ggplotGrob(plot1b)
    maxWidth <- grid::unit.pmax(gA$widths[2:5], gB$widths[2:5])
    gA$widths[2:5] <- as.list(maxWidth)
    gB$widths[2:5] <- as.list(maxWidth)
    left_plot <- gridExtra::arrangeGrob(gA, gB, ncol=1, heights = c(.25, .75))


    right_plot <- gridExtra::arrangeGrob(
        ggplot2::ggplot() + ggplot2::theme_minimal(),
        cntsplot +
            ggplot2::theme(
                legend.position="none",
                strip.text.y = ggplot2::element_blank(),
                axis.text.y = ggplot2::element_blank(),
                axis.ticks.y = ggplot2::element_blank(),
                strip.background = ggplot2::element_blank()
            ),
        ncol = 1,
        heights = c(.25, .75)
    )

    outplot <- gridExtra::arrangeGrob(
        left_plot,
        right_plot,
        ncol = 2,
        widths = c(.78, .22)
    )

    gridExtra::grid.arrange(outplot)

    return(invisible(list(plot = outplot, overall = plot1a, tags = plot1b, counts = cntsplot)))
}




#' Manual Assessment of a Model
#'
#' \code{assign_validation_task} - Create human assignments to assess how well a
#' model is functioning.  The coder can use the \code{correct} column to assess
#' how well the \code{tag} fits the \code{text} columns.
#'
#' @param checks The number of coders needed per tag assignment.
#' @param coders A vector of coders to assign tasks to.
#' @param out A directory name to create and output csv file(s) to.
#' @param as.list logical.  Should the assignments be dsplayed as a list of
#' \code{data.frame} or as a single \code{data.frame}?
#' @return \code{assign_validation_task} - Returns a \code{data.frame}/.csv or
#' \code{list} of \code{data.frame}s/.csvs.  Columns in the \code{data.frame}s
#' include:
#' \item{coder}{The assgned coder (person for the task).}
#' \item{index}{The row/element number of the text.}
#' \item{correct}{A blank column for coders to dummy/logical code if the tag assignment for that text was accurate.}
#' \item{tag}{The tag that was assigned to the text.}
#' \item{text}{The text to which the tag was assigned.}
#' @rdname validate_model
#' @export
assign_validation_task <- function(x, n=20, checks = 1, coders = "coder",
    out = NULL, as.list = TRUE, ...){

    text <- index <- variable <- correct <- NULL

    terms <- ifelse(inherits(x, 'token_count'), "token.vars", "term.vars")
    type <- ifelse(inherits(x, 'token_count'), "token", "term")

    if (!attributes(x)[["model"]]) {
        stop(paste0("`x` does not appear to be a 'model'; use `grouping.var =TRUE` in `", type, "_count` to create a model"))
    }

    if (checks > length(coders)) stop("`checks` must be smaller or equal in length to `coders")

    text.var <- attributes(x)[["text.var"]][["text.var"]]
    potentials <- apply(x[, attributes(x)[[terms]], drop = FALSE], 2, function(x) which(x > 0))

    items <- textshape::tidy_list(lapply(potentials, function(x){
        sample(x, ifelse(length(x) <= n, length(x), n))
    }), "tag", "index")

    dat <- data.table::data.table(items, stats::setNames(data.frame(do.call(rbind, lapply(seq_len(nrow(items)), function(i) {
        sample(coders, checks)
    })), stringsAsFactors = FALSE), paste0("check_", seq_len(checks))))

    dat <- data.table::melt(dat, id=c("tag", "index"), measure=paste0("check_", seq_len(checks)), value="coder")[,
        text := text.var[index]][, variable := NULL][, correct := ""][]

    data.table::setcolorder(dat, c('coder', 'index', 'correct', 'tag', 'text'))
    dat <- data.frame(dat, stringsAsFactors = FALSE)

    if (isTRUE(as.list)){
        dat <- split(dat, dat[[1]])
    }

    if (!is.null(out)){
        if(file.exists(out)) {
             warning(sprintf("`%s` exists; please delete or choose a new name", out))
        } else{
             dir.create(out)
             if (isTRUE(as.list)) {
                 invisible(Map(function(x, y){
                     utils::write.csv(x, file = file.path(out, sprintf("%s.csv", y)), row.names=FALSE)
                 }, dat, names(dat)))
             } else {
                 utils::write.csv(dat, file = file.path(out, "codes.csv"), row.names=FALSE)
             }
        }
    } else {
        dat
    }
}


proportion_confidence_not_20 <- function(x, N){

    if (length(x) > N) stop("`x` can not be longer than `N`")
    pm <- function(x, y) c(x - y, x + y)
    Mx <- mean(x)
    n <- length(x)
    Sp <- sqrt((Mx * (1 - Mx))/n) * sqrt(( N - n ) / ( N - 1 ))
    CI <- pm(Mx, (1.96 * Sp))
    data.frame(accuracy = Mx, sampled = n, se = Sp, lower = CI[1], upper = CI[2])
}

proportion_confidence <- function(x, adjust.discrete = TRUE){
    pm <- function(x, y, z = 0) c((x - y) - z, (x + y) + z)
    Mx <- mean(x)
    N <- length(x)
    Sp <- sqrt((Mx * (1 - Mx))/N)
    CI <- pm(Mx, (1.96 * Sp), ifelse(isTRUE(adjust.discrete), .5/N, 0))
    CI <- ifelse(CI > 1, 1, ifelse(CI < 0, 0, CI))
    data.frame(accuracy = Mx, sampled = N, se = Sp, lower = CI[1], upper = CI[2])
}


tag_assessment <- function(text.var, tag, number, total, width = 50){
    lines <- paste(rep("-", width), collapse="")
    text <- strwrap(text.var, width)
    tag <- sprintf("\nTag: %s", tag)
    numb <- sprintf("[%s of %s]", number, total)
    clear <- paste(rep("\n", 20), collapse="")
    message(paste(c(clear, numb, lines,  text, tag, lines,  "\n\nDoes this tag fit?"), collapse="\n"))
    utils::menu(c("Yes", "No"))
}
