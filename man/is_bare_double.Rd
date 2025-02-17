% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/predicates.R
\name{is_bare_double}
\alias{is_bare_double}
\alias{are_bare_double,}
\alias{is_not_bare_double,}
\alias{are_not_bare_double}
\alias{is_not_bare_double}
\alias{are_bare_double}
\title{Bare type predicates}
\usage{
is_bare_double(x, n = NULL)

is_not_bare_double(x, n = NULL)

are_bare_double(x, n = NULL)

are_not_bare_double(x, n = NULL)
}
\arguments{
\item{x}{Object to be tested.}

\item{n}{Expected length of a vector.}
}
\value{
\itemize{
\item Calls to \code{is_bare_double} are guaranteed to return a scalar boolean (ie. a single \code{TRUE} or \code{FALSE} value). If an argument of length > 1 is given, \code{FALSE} is returned.
\item \code{are_bare_double} is a wrapper around \code{\link[purrr]{map_lgl}(vec, \(i) is_bare_double(i, ...))}. A boolean vector of the same length as the input is guaranteed.
\item Calls to \code{is_not_bare_double}/\code{are_not_bare_double} negate the output of \code{is_bare_double}/\code{are_bare_double}.}
}
\description{
This is a re-export of \code{\link[rlang:bare-type-predicates]{rlang::is_bare_double()}}, modified to have standardised naming and standardised vector handling.
Documentation is atuomatically generated from the original package documentation. See the \code{\link[rlang:bare-type-predicates]{original}} for full details.

These predicates check for a given type but only return \code{TRUE} for
bare R objects. Bare objects have no class attributes. For example,
a data frame is a list, but not a bare list.
}
