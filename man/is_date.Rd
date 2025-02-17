% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/predicates.R
\name{is_date}
\alias{is_date}
\alias{are_date,}
\alias{is_not_date,}
\alias{are_not_date}
\alias{is_not_date}
\alias{are_date}
\title{Check that an argument is a Date}
\usage{
is_date(
  x,
  lower = NULL,
  upper = NULL,
  any.missing = TRUE,
  all.missing = TRUE,
  len = NULL,
  min.len = NULL,
  max.len = NULL,
  unique = FALSE,
  null.ok = FALSE
)

is_not_date(
  x,
  lower = NULL,
  upper = NULL,
  any.missing = TRUE,
  all.missing = TRUE,
  len = NULL,
  min.len = NULL,
  max.len = NULL,
  unique = FALSE,
  null.ok = FALSE
)

are_date(
  x,
  lower = NULL,
  upper = NULL,
  any.missing = TRUE,
  all.missing = TRUE,
  len = NULL,
  min.len = NULL,
  max.len = NULL,
  unique = FALSE,
  null.ok = FALSE
)

are_not_date(
  x,
  lower = NULL,
  upper = NULL,
  any.missing = TRUE,
  all.missing = TRUE,
  len = NULL,
  min.len = NULL,
  max.len = NULL,
  unique = FALSE,
  null.ok = FALSE
)
}
\arguments{
\item{x}{any\cr
Object to check.}

\item{lower}{\code{\link[base]{Date}}\cr
All non-missing dates in \code{x} must be >= this date. Comparison is done via \code{\link[base]{Ops.Date}}.}

\item{upper}{\code{\link[base]{Date}}\cr
All non-missing dates in \code{x} must be before <= this date. Comparison is done via \code{\link[base]{Ops.Date}}.}

\item{any.missing}{\code{logical(1)}\cr
Are vectors with missing values allowed? Default is \code{TRUE}.}

\item{all.missing}{\code{logical(1)}\cr
Are vectors with no non-missing values allowed? Default is \code{TRUE}.
Note that empty vectors do not have non-missing values.}

\item{len}{\code{integer(1)}\cr
Exact expected length of \code{x}.}

\item{min.len}{\code{integer(1)}\cr
Minimal length of \code{x}.}

\item{max.len}{\code{integer(1)}\cr
Maximal length of \code{x}.}

\item{unique}{\code{logical(1)}\cr
Must all values be unique? Default is \code{FALSE}.}

\item{null.ok}{\code{logical(1)}\cr
If set to \code{TRUE}, \code{x} may also be \code{NULL}.
In this case only a type check of \code{x} is performed, all additional checks are disabled.}
}
\value{
\itemize{
\item Calls to \code{is_date} are guaranteed to return a scalar boolean (ie. a single \code{TRUE} or \code{FALSE} value). If an argument of length > 1 is given, \code{FALSE} is returned.
\item \code{are_date} is a wrapper around \code{\link[purrr]{map_lgl}(vec, \(i) is_date(i, ...))}. A boolean vector of the same length as the input is guaranteed.
\item Calls to \code{is_not_date}/\code{are_not_date} negate the output of \code{is_date}/\code{are_date}.}
}
\description{
This is a re-export of \code{\link[checkmate:checkDate]{checkmate::test_date()}}, modified to have standardised naming and standardised vector handling.
Documentation is atuomatically generated from the original package documentation. See the \code{\link[checkmate:checkDate]{original}} for full details.

Checks that an object is of class \code{\link[base]{Date}}.
}
