% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/predicates.R
\name{is_primitive}
\alias{is_primitive}
\alias{are_primitive,}
\alias{is_not_primitive,}
\alias{are_not_primitive}
\alias{is_not_primitive}
\alias{are_primitive}
\title{Is object a function?}
\usage{
is_primitive(x)

is_not_primitive(x)

are_primitive(x)

are_not_primitive(x)
}
\arguments{
\item{x}{Object to be tested.}
}
\value{
\itemize{
\item Calls to \code{is_primitive} are guaranteed to return a scalar boolean (ie. a single \code{TRUE} or \code{FALSE} value). If an argument of length > 1 is given, \code{FALSE} is returned.
\item \code{are_primitive} is a wrapper around \code{\link[purrr]{map_lgl}(vec, \(i) is_primitive(i, ...))}. A boolean vector of the same length as the input is guaranteed.
\item Calls to \code{is_not_primitive}/\code{are_not_primitive} negate the output of \code{is_primitive}/\code{are_primitive}.}
}
\description{
This is a re-export of \code{\link[rlang:is_function]{rlang::is_primitive()}}, modified to have standardised naming and standardised vector handling.
Documentation is atuomatically generated from the original package documentation. See the \code{\link[rlang:is_function]{original}} for full details.

The R language defines two different types of functions: primitive
functions, which are low-level, and closures, which are the regular
kind of functions.
}
