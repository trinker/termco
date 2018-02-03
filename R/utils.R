validate_term_count <- function(x, warn = FALSE, ...){

    terms <- ifelse(inherits(x, 'token_count'), "token.vars", "term.vars")
    nwords <- ifelse(inherits(x, 'token_count'), "n.tokens", "n.words")
    type <- ifelse(inherits(x, 'token_count'), "token", "term")

    nms2 <- unlist(list(attributes(x)[[terms]], nwords))
    nms <- unlist(list(attributes(x)[["group.vars"]], nms2))
    check <- all(nms %in% colnames(x)) && all(sapply(x[, nms2], is.numeric))
    check2 <- all(sapply(c("group.vars", terms, "weight", "pretty"), function(y){
        !is.null(attributes(x)[[y]])
    }))
    check3 <- !any(colnames(x) %in% c(nms2, nms, nwords))
    if (!check | !check2 | check3) {
        if (isTRUE(warn)){
            warning(paste0("Does not appear to be a `", type, "_count` object.\n"),
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

propify_token <- function(x, fun, ...){

    validate_token_count(x)
    termcols <- attributes(x)[["token.vars"]]

    if (attributes(x)[["weight"]] != "count") {
        x <- attributes(x)[["counts"]]
    } else {
        counts <- new.env(FALSE)
        counts[["term_counts"]] <- as.data.frame(x)
        attributes(x)[["counts"]] <- counts
    }

    fun2 <- function(y) fun(y, x[["n.tokens"]])

    dat <- x[termcols]
    x[termcols] <- lapply(dat, fun2)
    class(x)[class(x) %in% "token_count"] <- "termco_count_weighted"
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

pp <- function(x, digits = getOption("digit_digits")) {

    f(x, digits = digits, e="%")
}


f <- function(x, digits = getOption("digit_digits"), s, e) {

    if (is.null(digits)) digits <- 3

    if(length(digits) > 1) {
        digits <- digits[1]
        warning("Using only digits[1]")
    }

    x <- round(as.numeric(x), digits)

    if (digits > 0) x <- sprintf(paste0("%.", digits, "f"), x)
    out <- gsub("^0(?=\\.)|(?<=-)0", "", x, perl=TRUE)
    out[out == "NA"] <- NA
    if (!missing(s)) out <- paste0(s, out)
    if (!missing(e)) out <- paste0(out, e)
    out
}



grep_return_null <- function(pattern, x, ignore.case = TRUE){
    x[!stringi::stri_detect_regex(x, pattern, opts_regex = stringi::stri_opts_regex(case_insensitive = ignore.case))]
}

general_rescale <- function(x, lower, upper){

    rng <-  range(x, na.rm = TRUE, finite = TRUE)
    if (diff(rng) == 0) return(stats::setNames(rep(upper, length(x)), names(x)))
    (x - rng[1])/diff(rng) * diff(range(c(lower, upper))) + lower

}


minmax_scale <- function(x) {
	if(max(x) - min(x) == 0) return(stats::setNames(rep(1, length(x)), names(x)))
    (x - min(x))/(max(x) - min(x))
}


spacer <- function(x){
    mc <- max(nchar(x))
    paste0(sapply(mc - sapply(x, nchar), function(y) paste(rep(" ", y), collapse = "")), x)
}

constrain <- function(x, lower, upper) ifelse(x < lower, lower, ifelse(x > upper, upper, x))

.mgsub <- function (pattern, replacement, text.var, fixed = TRUE,
	order.pattern = fixed, perl = TRUE, ...) {

    if (fixed && order.pattern) {
        ord <- rev(order(nchar(pattern)))
        pattern <- pattern[ord]
        if (length(replacement) != 1) replacement <- replacement[ord]
    }
    if (length(replacement) == 1) replacement <- rep(replacement, length(pattern))

    for (i in seq_along(pattern)){
        text.var <- gsub(pattern[i], replacement[i], text.var, fixed = fixed, perl = perl, ...)
    }

    text.var
}

is_nested_single_tier <- function(x) is.list(x[[1]]) && length(x) == 1

warn_unnest <- function(x, type = 'term', ...){
    if(is_nested_single_tier(x)) {
        x <- x[[1]]
        warning(paste(c(
            sprintf('The %s list supplied has a hierarchical structure but only has a single tier.', type),
            'Unlisting one level!'),
            collapse = '\n'), call. = FALSE)
    }
    x
}


## assess if object has metatags attribute  and if so that a column is named `tag`
check_meta_tags <- function(x, ...){
    if (is.null(attributes(x)[['metatags']])) return(FALSE)
    if (!'tag' %in% colnames(attributes(x)[['metatags']]) ) {
        type <- ifelse(is.null(attributes(x)[['tokens']]), 'term', 'token')
        warning(paste0(
            sprintf('`%s_count` object has a `metatags` attribute with no `tag` column.)', type),
            'The `metatags` attribute will not be used'
        ), .call = FALSE)
        return(FALSE)
    }
    return(TRUE)
}



tags2meta <- function(tags, meta.sep = '__', meta.names = c('meta'), ...){

    if (!any(grepl(meta.sep[1], tags))) return(NULL)

    tgs <- as.list(tags)

    for (i in seq_along(meta.sep)){
        for (j in seq_along(tgs)) {
            tgs[[j]] <- unlist(strsplit(tgs[[j]], split = meta.sep[i], fixed = TRUE))
        }
    }


    lens <- lengths(tgs)

    if (stats::sd(lens) != 0) {
        warning(paste0(
            'It appears you\'re trying to use tag separators to denote metatags...\n',
             'However the following element(s) did not contain exactly the same number of of separators as the others:\n\n',
            paste(paste0('    - ', tags[!lens %in%  as.numeric(names(which.max(table(lens))))]), collapse = '\n'),
            '\n\n`attributes(x)[["metatags"]]` will be `NULL`.'
        ), call. = FALSE)
        return(NULL)
    }

    if (length(meta.names) != (lens[1]-1)){
        #warning('Length of `meta.names` not equal to number of metatag breaks.  Adding names.')
        nms <- rep(NA, lens[1]-1 )
        nms[seq_along(meta.names)] <- meta.names
        nms[is.na(nms)] <- paste0('meta_', which(is.na(nms)))
        meta.names <- nms
    }

    out <- data.frame(
        tag = tags,
        stats::setNames(data.frame(do.call(rbind, tgs), stringsAsFactors = FALSE), c(meta.names, 'sub_tag')),
        stringsAsFactors = FALSE
    )

    out <- dplyr::tbl_df(out)

    class(out) <- c('metatags', class(out))

    out
}


term_token_validate <- function(x, ...){
    
    terms <- ifelse(inherits(x, 'token_count'), "token.vars", "term.vars")
    type <- ifelse(inherits(x, 'token_count'), "token", "term")

    if (!isTRUE(validate_term_count(x, FALSE))) {
        stop(paste0(
            '`x` does not appear to be a valid `',
            type,
            '_count` object.  Was the object altered after creation?'
        ))
    }
    
    return(terms)
}
