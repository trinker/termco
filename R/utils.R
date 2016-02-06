validate_term_count <- function(x, warn = FALSE){

    nms2 <- unlist(list(attributes(x)[["term.vars"]], "n.words"))
    nms <- unlist(list(attributes(x)[["group.vars"]], nms2))
    check <- all(nms %in% colnames(x)) && all(sapply(x[, nms2], is.numeric))
    check2 <- all(sapply(c("group.vars", "term.vars", "weight", "pretty"), function(y){
        !is.null(attributes(x)[[y]])
    }))
    check3 <- !any(colnames(x) %in% c(nms2, nms, "n.words"))
    if (!check | !check2 | check3) {
        if (isTRUE(warn)){
            warning("Does not appear to be a `term_count` object.\n",
                "  Has the object or column names been altered/added?",
                immediate. = TRUE
            )
        }
        return(FALSE)
    }
    TRUE
}

rm_class <- function(x, remove = "term_count", ...){
    class(x) <- class(x)[!class(x) %in% remove]
    x
}

propify <- function(x, fun, ...){

    validate_term_count(x)
    termcols <- attributes(x)[["term.vars"]]

    if (attributes(x)[["weight"]] != "count") {
        x <- attributes(x)[["counts"]]
    } else {
        counts <- new.env(FALSE)
        counts[["term_counts"]] <- as.data.frame(x)
        attributes(x)[["counts"]] <- counts
    }

    fun2 <- function(y) fun(y, x[["n.words"]])

    dat <- x[termcols]
    x[termcols] <- lapply(dat, fun2)
    class(x)[class(x) %in% "term_count"] <- "term_count_weighted"
    attributes(x)[["weight"]] <- "proportion"
    x
}

prop <- function(a, b) a/b
perc <- function(a, b) 100*(a/b)


countfun <- function(x, y, ignore.case = TRUE){
    stringi::stri_count_regex(y, x,
        opts_regex=stringi::stri_opts_regex(case_insensitive = ignore.case))
}


comb <- function(a, b, digits, zero.replace = "0", weight = "percent") {
    const <- ifelse(weight == "percent", 100, 1)
    x <- sprintf("%s(%s%%)", a, digit_format(const * (a/b), digits))
    x[a == 0] <- zero.replace
    x
}

digit_format <- function (x, digits = getOption("digit_digits")) {
    if (is.null(digits)) digits <- 3
    if (length(digits) > 1) {
        digits <- digits[1]
        warning("Using only digits[1]")
    }
    x <- round(as.numeric(x), digits)
    if (digits > 0)
        x <- sprintf(paste0("%.", digits, "f"), x)
    out <- gsub("^0(?=\\.)|(?<=-)0", "", x, perl = TRUE)
    out[out == "NA"] <- NA
    out
}


is.count <- function(x, ...){
    validate_term_count(x)
    attributes(x)[["weight"]] == "count"
}



paste2 <- function (multi.columns, sep = ".", handle.na = TRUE, trim = TRUE) {
    if (is.matrix(multi.columns)) {
        multi.columns <- data.frame(multi.columns, stringsAsFactors = FALSE)
    }
    if (trim)
        multi.columns <- lapply(multi.columns, function(x) {
            gsub("^\\s+|\\s+$", "", x)
        })
    if (!is.data.frame(multi.columns) & is.list(multi.columns)) {
        multi.columns <- do.call("cbind", multi.columns)
    }
    if (handle.na) {
        m <- apply(multi.columns, 1, function(x) {
            if (any(is.na(x))) {
                NA
            } else {
                paste(x, collapse = sep)
            }
        })
    } else {
        m <- apply(multi.columns, 1, paste, collapse = sep)
    }
    names(m) <- NULL
    return(m)
}

pn <- function(x, big.mark = ",", ...) {

    prettyNum(x, big.mark, ...)
}

pp <- function(x, digits = 1, ...) {

    paste0(round(100*x, digits = digits), "%")
}


