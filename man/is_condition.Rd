% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/predicates.R
\name{is_condition}
\alias{is_condition}
\alias{are_condition,}
\alias{is_not_condition,}
\alias{are_not_condition}
\alias{is_not_condition}
\alias{are_condition}
\title{Is object a condition?}
\usage{
is_condition(x)

is_not_condition(x)

are_condition(x)

are_not_condition(x)
}
\arguments{
\item{x}{An object to test.}
}
\value{
\itemize{
\item Calls to \code{is_condition} are guaranteed to return a scalar boolean (ie. a single \code{TRUE} or \code{FALSE} value). If an argument of length > 1 is given, \code{FALSE} is returned.
\item \code{are_condition} is a wrapper around \code{\link[purrr]{map_lgl}(vec, \(i) is_condition(i, ...))}. A boolean vector of the same length as the input is guaranteed.
\item Calls to \code{is_not_condition}/\code{are_not_condition} negate the output of \code{is_condition}/\code{are_condition}.}
}
\description{
This is a re-export of \code{\link[rlang:is_condition]{rlang::is_condition()}}, modified to have standardised naming and standardised vector handling.
Documentation is atuomatically generated from the original package documentation. See the \code{\link[rlang:is_condition]{original}} for full details.

Is object a condition?.
}
