#' Split Data Into Training and Test
#'
#' Split a data set into training and testing data.
#'
#' @param data A \code{\link[base]{data.frame}} or \code{\link[base]{vector}}.
#' @param n.train An integer (number of) or proportion (proportion of) dictating
#' how many observations to place in the training set.
#' @param \ldots ignored.
#' @return Returns a named list of split data; a \code{train} data set and a
#' \code{test} data set.
#' @export
#' @rdname split_data
#' @examples
#' (split_dat <- split_data(mtcars))
#' split_dat$train
#' split_dat$test
#' split_data(mtcars, .8)
#'
#' split_data(mtcars, 20)
#' split_data(LETTERS)
#' split_data(LETTERS, .4)
#' split_data(LETTERS, 10)
split_data <- function(data, n.train = .5, ...){

    UseMethod("split_data")

}

#' @export
#' @rdname split_data
#' @method split_data data.frame
split_data.data.frame <- function(data, n.train = .5, ...){

    if (n.train < 1){
        n.train <- round(nrow(data) * n.train)
    }
    inds <- sample.int(nrow(data), n.train)

    out <- list(train = data[inds, ], test = data[!seq_len(nrow(data)) %in% inds, ])
    class(out) <- unique(c("split_data", class(out)))
    out
}

#' @export
#' @rdname split_data
#' @method split_data default
split_data.default <- function(data, n.train = .5, ...){

    if (n.train < 1){
        n.train <- round(length(data) * n.train)
    }
    inds <- sample.int(length(data), n.train)

    out <- list(train = data[inds], test = data[!seq_along(data) %in% inds])
    class(out) <- unique(c("split_data", class(out)))
    out
}



#' Prints a split_data Object
#'
#' Prints a split_data object
#'
#' @param x A split_data object
#' @param n Number of elements to print
#' @param \ldots ignored.
#' @method print split_data
#' @export
print.split_data <- function(x, n = 6, ...){
    cat("split_data:\n")
    Map(function(x, y) {
       if(is.data.frame(x)) len <- nrow(x) else len <- length(x)
       cat(sprintf("\n%s: n = %s\n", y, len))
       print(utils::head(x))
       if (len > n ) cat("|...\n")
    }, x, names(x))
}







