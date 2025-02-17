% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/predicates.R
\name{is_scalar_character}
\alias{is_scalar_character}
\alias{are_scalar_character,}
\alias{is_not_scalar_character,}
\alias{are_not_scalar_character}
\alias{is_not_scalar_character}
\alias{are_scalar_character}
\title{Scalar type predicates}
\usage{
is_scalar_character(x)

is_not_scalar_character(x)

are_scalar_character(x)

are_not_scalar_character(x)
}
\arguments{
\item{x}{object to be tested.}
}
\value{
\itemize{
\item Calls to \code{is_scalar_character} are guaranteed to return a scalar boolean (ie. a single \code{TRUE} or \code{FALSE} value). If an argument of length > 1 is given, \code{FALSE} is returned.
\item \code{are_scalar_character} is a wrapper around \code{\link[purrr]{map_lgl}(vec, \(i) is_scalar_character(i, ...))}. A boolean vector of the same length as the input is guaranteed.
\item Calls to \code{is_not_scalar_character}/\code{are_not_scalar_character} negate the output of \code{is_scalar_character}/\code{are_scalar_character}.}
}
\description{
This is a re-export of \code{\link[rlang:scalar-type-predicates]{rlang::is_scalar_character()}}, modified to have standardised naming and standardised vector handling.
Documentation is atuomatically generated from the original package documentation. See the \code{\link[rlang:scalar-type-predicates]{original}} for full details.

These predicates check for a given type and whether the vector is
"scalar", that is, of length 1.

In addition to the length check, \code{is_string()} and \code{is_bool()}
return \code{FALSE} if their input is missing. This is useful for
type-checking arguments, when your function expects a single string
or a single \code{TRUE} or \code{FALSE}.
}
