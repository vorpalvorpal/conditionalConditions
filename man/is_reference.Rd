% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/predicates.R
\name{is_reference}
\alias{is_reference}
\alias{are_reference,}
\alias{is_not_reference,}
\alias{are_not_reference}
\alias{is_not_reference}
\alias{are_reference}
\title{Is an object referencing another?}
\usage{
is_reference(x, y)

is_not_reference(x, y)

are_reference(x, y)

are_not_reference(x, y)
}
\arguments{
\item{x, y}{R objects.}
}
\value{
\itemize{
\item Calls to \code{is_reference} are guaranteed to return a scalar boolean (ie. a single \code{TRUE} or \code{FALSE} value). If an argument of length > 1 is given, \code{FALSE} is returned.
\item \code{are_reference} is a wrapper around \code{\link[purrr]{map_lgl}(vec, \(i) is_reference(i, ...))}. A boolean vector of the same length as the input is guaranteed.
\item Calls to \code{is_not_reference}/\code{are_not_reference} negate the output of \code{is_reference}/\code{are_reference}.}
}
\description{
This is a re-export of \code{\link[rlang:is_reference]{rlang::is_reference()}}, modified to have standardised naming and standardised vector handling.
Documentation is atuomatically generated from the original package documentation. See the \code{\link[rlang:is_reference]{original}} for full details.

There are typically two situations where two symbols may refer to
the same object.
\itemize{
\item{ R objects usually have copy-on-write semantics. This is an
optimisation that ensures that objects are only copied if
needed. When you copy a vector, no memory is actually copied
until you modify either the original object or the copy is
modified.

Note that the copy-on-write optimisation is an implementation
detail that is not guaranteed by the specification of the R
language.
}\item{ Assigning an \link[rlang:is_copyable]{uncopyable} object (like an
environment) creates a reference. These objects are never copied
even if you modify one of the references.
}}.
}
