% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/split_data.R
\name{split_data}
\alias{split_data}
\alias{split_data.data.frame}
\alias{split_data.default}
\title{Split Data Into Training and Test}
\usage{
split_data(data, n.train = 0.5, ...)

\method{split_data}{data.frame}(data, n.train = 0.5, ...)

\method{split_data}{default}(data, n.train = 0.5, ...)
}
\arguments{
\item{data}{A \code{\link[base]{data.frame}} or \code{\link[base]{vector}}.}

\item{n.train}{An integer (number of) or proportion (proportion of) dictating
how many observations to place in the training set.}

\item{\ldots}{ignored.}
}
\value{
Returns a named list of split data; a \code{train} data set and a
\code{test} data set.
}
\description{
Split a data set into training and testing data.
}
\examples{
(split_dat <- split_data(mtcars))
split_dat$train
split_dat$test
split_data(mtcars, .8)

split_data(mtcars, 20)
split_data(LETTERS)
split_data(LETTERS, .4)
split_data(LETTERS, 10)
}
