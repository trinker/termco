#' Regular Expression Counts of Terms and Substrings
#'
#' A small suite of functions used to count terms and substrings in strings.
#' @docType package
#' @name termco
#' @aliases termco package-termco
NULL



#' 2012 U.S. Presidential Debates
#'
#' A dataset containing a cleaned version of all three presidential debates for
#' the 2012 election.
#'
#' @details
#' \itemize{
#'   \item person. The speaker
#'   \item tot. Turn of talk
#'   \item dialogue. The words spoken
#'   \item time. Variable indicating which of the three debates the dialogue is from
#' }
#'
#' @docType data
#' @keywords datasets
#' @name presidential_debates_2012
#' @usage data(presidential_debates_2012)
#' @format A data frame with 2912 rows and 4 variables
NULL




#' Discourse Marker Search of Presidential Debates
#'
#' A \code{\link[termco]{term_count}} dataset containing discourse markers from
#' the 2012 presidential debates.
#'
#' @details
#' \itemize{
#'   \item person. The speaker
#'   \item time. Variable indicating which of the three debates the dialogue is from
#'   \item n.words. The number of words
#'   \item response_cries. The number of response cries: \code{c("oh", "ah", "aha", "ouch", "yuk")}
#'   \item back_channels. The number of back channels: \code{c("uh[- ]huh", "uhuh", "yeah")}
#'   \item summons. The number of summons: \code{"hey"}
#'   \item justification. The number of justification: \code{"because"}
#' }
#'
#' @docType data
#' @keywords datasets
#' @name markers
#' @usage data(markers)
#' @format A data frame with 10 rows and 7 variables
NULL


#' Sam I Am Text
#'
#' A dataset containing a character vector of the text from Seuss's 'Sam I Am'.
#'
#' @docType data
#' @keywords datasets
#' @name sam_i_am
#' @usage data(sam_i_am)
#' @format A character vector with 169 elements
#' @references Seuss, Dr. (1960). Green Eggs and Ham.
NULL


