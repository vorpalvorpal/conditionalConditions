% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/predicates.R
\name{is_count}
\alias{is_count}
\alias{are_count,}
\alias{is_not_count,}
\alias{are_not_count}
\alias{is_not_count}
\alias{are_count}
\title{Check if an argument is a count}
\usage{
is_count(
  x,
  na.ok = FALSE,
  positive = FALSE,
  tol = sqrt(.Machine$double.eps),
  null.ok = FALSE
)

is_not_count(
  x,
  na.ok = FALSE,
  positive = FALSE,
  tol = sqrt(.Machine$double.eps),
  null.ok = FALSE
)

are_count(
  x,
  na.ok = FALSE,
  positive = FALSE,
  tol = sqrt(.Machine$double.eps),
  null.ok = FALSE
)

are_not_count(
  x,
  na.ok = FALSE,
  positive = FALSE,
  tol = sqrt(.Machine$double.eps),
  null.ok = FALSE
)
}
\arguments{
\item{x}{any\cr
Object to check.}

\item{na.ok}{\code{logical(1)}\cr
Are missing values allowed? Default is \code{FALSE}.}

\item{positive}{\code{logical(1)}\cr
Must \code{x} be positive (>= 1)?
Default is \code{FALSE}, allowing 0.}

\item{tol}{\code{double(1)}\cr
Numerical tolerance used to check whether a double or complex can be converted.
Default is \code{sqrt(.Machine$double.eps)}.}

\item{null.ok}{\code{logical(1)}\cr
If set to \code{TRUE}, \code{x} may also be \code{NULL}.
In this case only a type check of \code{x} is performed, all additional checks are disabled.}
}
\value{
\itemize{
\item Calls to \code{is_count} are guaranteed to return a scalar boolean (ie. a single \code{TRUE} or \code{FALSE} value). If an argument of length > 1 is given, \code{FALSE} is returned.
\item \code{are_count} is a wrapper around \code{\link[purrr]{map_lgl}(vec, \(i) is_count(i, ...))}. A boolean vector of the same length as the input is guaranteed.
\item Calls to \code{is_not_count}/\code{are_not_count} negate the output of \code{is_count}/\code{are_count}.}
}
\description{
This is a re-export of \code{\link[checkmate:checkCount]{checkmate::test_count()}}, modified to have standardised naming and standardised vector handling.
Documentation is atuomatically generated from the original package documentation. See the \code{\link[checkmate:checkCount]{original}} for full details.

A count is defined as non-negative integerish value.
}
