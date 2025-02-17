% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/predicates.R
\name{is_error}
\alias{is_error}
\alias{are_error,}
\alias{is_not_error,}
\alias{are_not_error}
\alias{is_not_error}
\alias{are_error}
\title{Is object a condition?}
\usage{
is_error(x)

is_not_error(x)

are_error(x)

are_not_error(x)
}
\arguments{
\item{x}{An object to test.}
}
\value{
\itemize{
\item Calls to \code{is_error} are guaranteed to return a scalar boolean (ie. a single \code{TRUE} or \code{FALSE} value). If an argument of length > 1 is given, \code{FALSE} is returned.
\item \code{are_error} is a wrapper around \code{\link[purrr]{map_lgl}(vec, \(i) is_error(i, ...))}. A boolean vector of the same length as the input is guaranteed.
\item Calls to \code{is_not_error}/\code{are_not_error} negate the output of \code{is_error}/\code{are_error}.}
}
\description{
This is a re-export of \code{\link[rlang:is_condition]{rlang::is_error()}}, modified to have standardised naming and standardised vector handling.
Documentation is atuomatically generated from the original package documentation. See the \code{\link[rlang:is_condition]{original}} for full details.

Is object a condition?.
}
