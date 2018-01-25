test_regex <- function(regex, stringi = FALSE, ...){
    UseMethod('test_regex')
}

test_regex.character <- function(regex, stringi = FALSE, ...){
    is_valid_regex(regex, stringi = stringi)
}

test_regex.term_list <- function(regex, stringi = FALSE, ...){
    test_term_list_regex_h(regex, stringi = stringi, ...)
}

test_regex.list <- function(regex, stringi = FALSE, ...){
    regex <- as_term_list(regex, collapse = FALSE)
    test_term_list_regex_h(regex, stringi = stringi, ...)
}


is_valid_regex <- function (regex, stringi = FALSE, ...) {

    if (stringi){
        out <- suppressWarnings(try(stringi::stri_replace_all_regex("hello", regex, ""),
        silent = TRUE))
    } else {
        out <- suppressWarnings(try(gsub(regex, "", "hello", perl = TRUE),
        silent = TRUE))
    }
    !inherits(out, "try-error")
}


test_term_list_regex_h <- function(cats, stringi = stringi, ...){

    if ("termco_nested" %in% class(cats)) {

        outs <- lapply(cats, function(y) lapply(y, is_valid_regex, stringi = FALSE))
        tiers <- sapply(outs, function(x) sum(!unlist(x)) > 0)
        if (sum(!tiers) == 0) return(cats)

        tags <- unlist(lapply(outs[tiers], function(x){

            paste(sprintf('(%s) %s', seq_along(names(x)[!unlist(x)]), names(x)[!unlist(x)]), collapse = '; ')

        }))
        message <- paste(sprintf('Tier %s has invalid regex(es) within the following categories: \n    %s', which(tiers), tags), collapse = '\n\n')
        warning(message, call. = FALSE)

        return(cats)

    } else {

        outs <- lapply(cats, is_valid_regex, stringi = FALSE)
        if (sum(!unlist(outs)) == 0) return(cats)

        tags <- paste(sprintf('(%s) %s', seq_along(names(outs)[!unlist(outs)]), names(outs)[!unlist(outs)]), collapse = '\n    ')

        message <- sprintf('Invalid regex(es) within the following categories: \n\n    %s', tags)
        warning(message, call. = FALSE)

        return(cats)

    }
}
