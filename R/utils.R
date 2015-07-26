validate_term_count <- function(x){

    nms2 <- unlist(list(attributes(x)[["term.vars"]], "n.words"))
    nms <- unlist(list(attributes(x)[["group.vars"]], nms2))
    check <- all(nms %in% colnames(x)) && all(sapply(x[, nms2], is.numeric))
    check2 <- all(sapply(c("group.vars", "term.vars", "weight", "pretty"), function(y){
        !is.null(attributes(x)[[y]])
    }))
    if (!check | !check2) {
        stop("Does not appear to be a `term_count` object.\n",
            "  Has the object or column names been altered?"
        )
    }
    TRUE
}


propify <- function(x, fun, ...){

    validate_term_count(x)
    termcols <- attributes(x)[["term.vars"]]

    if (attributes(x)[["weight"]] != "count") {
        x <- attributes(x)[["counts"]]
    } else {
        attributes(x)[["counts"]] <- x
    }

    fun2 <- function(y) fun(y, x[["n.words"]])

    dat <- dplyr::select_(x, .dots = termcols)
    x[termcols] <- dplyr::mutate_each_(dat, dplyr::funs(fun2), termcols)
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


comb <- function(a, b, digits, zero.replace = "0") {
    x <- sprintf("%s(%s%%)", a, digit_format(100 * (a/b), digits))
    x[a == 0] <- zero.replace
    x
}

digit_format <- function (x, digits = getOption("digit_digits")) {
    if (is.null(digits))
        digits <- 3
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
