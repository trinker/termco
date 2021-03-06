#' Model Evaluation
#'
#' Get accuracy, precision, and recall for multi-class, multi-tag, predictions.
#'
#' @param x The model classification \code{\link[base]{list}}/\code{\link[base]{vector}}
#' (typically the results of \code{classify}).
#' @param known The known expert coded \code{\link[base]{list}}/\code{\link[base]{vector}} of outcomes.
#' @return Returns a list of seven elements:
#' \item{N}{The number of elements being assessed}
#' \item{confusion_matrix}{A list of confusion matrices for each tag}
#' \item{tag_accuracy}{A tag named vector of accuracies computed from the confusion matrices; (tp + tn)/(tp + tn + fp + fn)}
#' \item{tag_precision}{A tag named vector of precisions computed from the confusion matrices; tp/(tp + fp)}
#' \item{tag_recall}{A tag named vector of accuracies computed from the confusion matrices; tp/(tp + fn)}
#' \item{macro_averaged}{Macro averaged accuracy, precision, and recall; computed accuracy, precision, and recall for each confusion matrix and average}
#' \item{micro_averaged}{Micro averaged accuracy, precision, and recall; add the confusion amtrices and compute accuracy, precision, and recall}
#' @keywords accuracy model fit
#' @references https://www.youtube.com/watch?v=OwwdYHWRB5E&index=31&list=PL6397E4B26D00A269
#' @export
#' @examples
#' known <- list(1:3, 3, NA, 4:5, 2:4, 5, integer(0))
#' tagged <- list(1:3, 3, 4, 5:4, c(2, 4:3), 5, integer(0))
#' evaluate(tagged, known)
#'
#' ## Examples
#' library(dplyr)
#' data(presidential_debates_2012)
#'
#' discoure_markers <- list(
#'     response_cries = c("\\boh", "\\bah", "\\baha", "\\bouch", "yuk"),
#'     back_channels = c("uh[- ]huh", "uhuh", "yeah"),
#'     summons = "hey",
#'     justification = "because"
#' )
#'
#'
#' ## Only Single Tag Allowed Per Text Element
#' mod1 <- presidential_debates_2012 %>%
#'     with(., term_count(dialogue, TRUE, discoure_markers)) %>%
#'     classify()
#'
#' fake_known <- mod1
#' set.seed(1)
#' fake_known[sample(1:length(fake_known), 300)] <- "random noise"
#'
#' evaluate(mod1, fake_known)
#'
#' ## Multiple Tags Allowed
#' mod2 <- presidential_debates_2012 %>%
#'     with(., term_count(dialogue, TRUE, discoure_markers)) %>%
#'     classify(n = 2)
#'
#' fake_known2 <- mod2
#' set.seed(30)
#' fake_known2[sample(1:length(fake_known2), 500)] <- c("random noise", "back_channels")
#'
#' (myacc <- evaluate(mod2, fake_known2))
#' myacc$confusion_matrix
#' myacc$tag_accuracy
evaluate <- function(x, known){

    stopifnot(length(x) == length(known))

    if (!is.list(x)) x <- as.list(x)
    if (!is.list(known)) known <- as.list(known)
    x <- lapply(x, function(a) sapply(a, function(b) {b[is.na(b)] <- "No_Code_Given"; b}))
    known <- lapply(known, function(a) sapply(a, function(b) {b[is.na(b)] <- "No_Code_Given"; b}))
    out <- evaluation(x, known)
    class(out) <- "evaluate"
    out

}

#' Prints an evaluate Object
#'
#' Prints an evaluate object
#'
#' @param x The evaluate object.
#' @param digits The number of digits to print.
#' @param \ldots ignored
#' @method print evaluate
#' @export
print.evaluate <- function(x, digits = 3, ...){
#browser()
    x[['tag_level']][-1] <- lapply(x[['tag_level']][-1], digit_format, digits)
    tag_level <- sub("^\\s", "", utils::capture.output(print(x[['tag_level']], row.names=FALSE)))
    bar <- paste(rep("-", nchar(tag_level)[1]), collapse="")
    cat(bar, "\n")
    cat("Tag Level Measures\n")
    cat(bar, "\n")
    cat(paste(tag_level, collapse='\n'))
    cat('\n\n')

    n <- pn(x[["N"]])
    ner <- paste0("N:", paste(rep(" ", 15 + digits -nchar(n)), collapse = ""), n, "\n\n")
    bar2 <- paste(rep("-", nchar(ner)-2), collapse="")
    cat(bar2, "\n")
    cat("Summary Measures\n")
    cat(bar2, "\n")
    cat(ner)
    cat(        "Macro-Averaged  \n")

    filler1 <- ifelse(nchar(digit_format(x[['macro_averaged']], digits)) - 1 - digits == 0, " ", "")
    filler2 <- ifelse(nchar(digit_format(x[['micro_averaged']], digits)) - 1 - digits == 0, " ", "")
    cat(paste(
        paste0(c("  Accuracy:    ",
                 "  F-score:     ",
                 "  Precision:   ",
                 "  Recall:      "),
        filler1, digit_format(x[['macro_averaged']], digits)
    ), collapse="\n"))
    cat("\n")
    cat(        "\nMicro-Averaged  \n")
    cat(paste(
        paste0(c("  Accuracy:    ",
                 "  F-score:     ",
                 "  Precision:   ",
                 "  Recall:      "),
        filler2, digit_format(x[['micro_averaged']], digits)
    ), collapse="\n"))
    cat("\n\n")
}

confusion_matrices <- function(pred, actual){

    classes <- sort(unique(unlist(list(actual, pred))))
    cm <- matrix(rep(0, 4), ncol=2, dimnames = list(c('Pred-1', 'Pred-0'), c('Actual-1', 'Actual-0')))

    stats::setNames(lapply(classes, function(x){

        vals <- table(paste0(
            as.numeric(sapply(pred, function(y) x %in% y)),
            as.numeric(sapply(actual, function(y) x %in% y))
        ))

        try(cm[1, 1] <- vals[names(vals) == '11'], silent=TRUE)
        try(cm[1, 2] <- vals[names(vals) == '10'], silent=TRUE)
        try(cm[2, 1] <- vals[names(vals) == '01'], silent=TRUE)
        try(cm[2, 2] <- vals[names(vals) == '00'], silent=TRUE)
        cm
    }), classes)
}



evaluation <- function(pred, actual){

    out <- list()
    precision <- function(x) {out <- x[1,1]/sum(x[1,]); ifelse(is.nan(out), 0, out)}
    recall <- function(x) {out <- x[1,1]/sum(x[, 1]); ifelse(is.nan(out), 0, out)}
    F_score_fun <- function(prec, recall){out <- 2*((prec*recall)/(prec+recall));ifelse(is.nan(out), 0, out)}
    out[['N']] <- length(pred)
    cm <- out[['confusion_matrix']] <- confusion_matrices(pred, actual)

    tag_accuracy <- unlist(lapply(cm, function(x){
            sum(diag(x))/sum(x)
        }))

    tag_precision <- unlist(lapply(cm, function(x){
            precision(x)
        }))

    tag_recall <- unlist(lapply(cm, function(x){
            recall(x)
        }))

    F_score <- unlist(Map(F_score_fun, tag_precision, tag_recall))

    out[['tag_level']] <- data.frame(
        tag = names(tag_precision),
        precision = tag_precision,
        recall = tag_recall,
        F_score = F_score,
        accuracy = tag_accuracy,
        row.names = NULL
    )

    out[['macro_averaged']] <- list(
        accuracy = mean(tag_accuracy),
        F = mean(F_score),
        precision = mean(tag_precision),
        recall = mean(tag_recall)
    )

    summed_cm <- Reduce(`+`, cm)

    out[['micro_averaged']] <- list(
        accuracy = sum(diag(summed_cm))/sum(summed_cm),
        F = F_score_fun(precision(summed_cm), recall(summed_cm)),
        precision = precision(summed_cm),
        recall = recall(summed_cm)
    )

    out
}
