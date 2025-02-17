% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/predicates.R
\name{is_missing}
\alias{is_missing}
\alias{are_missing,}
\alias{is_not_missing,}
\alias{are_not_missing}
\alias{is_not_missing}
\alias{are_missing}
\title{Generate or handle a missing argument}
\usage{
is_missing(x)

is_not_missing(x)

are_missing(x)

are_not_missing(x)
}
\arguments{
\item{x}{An object that might be the missing argument.}
}
\value{
\itemize{
\item Calls to \code{is_missing} are guaranteed to return a scalar boolean (ie. a single \code{TRUE} or \code{FALSE} value). If an argument of length > 1 is given, \code{FALSE} is returned.
\item \code{are_missing} is a wrapper around \code{\link[purrr]{map_lgl}(vec, \(i) is_missing(i, ...))}. A boolean vector of the same length as the input is guaranteed.
\item Calls to \code{is_not_missing}/\code{are_not_missing} negate the output of \code{is_missing}/\code{are_missing}.}
}
\description{
This is a re-export of \code{\link[rlang:missing_arg]{rlang::is_missing()}}, modified to have standardised naming and standardised vector handling.
Documentation is atuomatically generated from the original package documentation. See the \code{\link[rlang:missing_arg]{original}} for full details.

These functions help using the missing argument as a regular R
object.
\itemize{
\item{ \code{missing_arg()} generates a missing argument.
}\item{ \code{is_missing()} is like \code{\link[base:missing]{base::missing()}} but also supports
testing for missing arguments contained in other objects like
lists. It is also more consistent with default arguments which
are never treated as missing (see section below).
}\item{ \code{maybe_missing()} is useful to pass down an input that might be
missing to another function, potentially substituting by a
default value. It avoids triggering an "argument is missing" error.
}}.
}
