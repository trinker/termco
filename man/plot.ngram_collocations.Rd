% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/DEPRECATED.R
\name{plot.ngram_collocations}
\alias{plot.ngram_collocations}
\title{Plots a ngram_collocations Object}
\usage{
\method{plot}{ngram_collocations}(x, drop.redundant.yaxis.text = TRUE, plot = TRUE, ...)
}
\arguments{
\item{x}{The \code{ngram_collocations} object.}

\item{drop.redundant.yaxis.text}{logical.  If \code{TRUE} the second y axis text/ticks,
in the heat plot are dropped.}

\item{plot}{logical.  If \code{TRUE} the output is plotted.}

\item{\ldots}{ignored.}
}
\value{
Returns a list of the three \pkg{ggplot2} objects that make the
combined plot.
}
\description{
Plots a ngram_collocations object.
}
