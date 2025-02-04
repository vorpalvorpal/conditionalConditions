#' Predicate Functions
#'
#' These functions are re-exports of predicate functions from various packages,
#' standardised to use is_ prefix naming convention. All functionality and
#' documentation is preserved from the original functions.
#'
#' Re-exports are included from:
#' * checkmate: test_* functions renamed to is_*
#' * rlang: is_* functions
#' * fs: is_* functions and *_exists functions (latter renamed to is_existing_*)
#' * lubridate: is.* functions renamed to is_*
#'
#' Note that in cases where multiple packages provide similar functionality
#' (e.g., is_date), precedence is given to earlier packages in the list above.
#'
#' @name predicates
#' @keywords internal
NULL


#' @title is_path_for_output function from checkmate
#'
#' @description Re-export of \code{checkmate::test_path_for_output}.
#' See the original package documentation for full details.
#' Check if a file path can be used safely to create a file and write to it.
#' 
#' This is checked:
#' list("\n", " ", list(), list("Does ", list("dirname(x)"), " exist?"), "\n", " ", list(), list("Does no file under path ", list("x"), " exist?"), "\n", " ", list(), list("Is ", list("dirname(x)"), " writable?"), "\n")
#' Paths are relative to the current working directory.
#' 
#'
#' @param x [any]\cr
#' Object to check.
#' @param overwrite [\code{logical(1)}]\cr
#' If \code{TRUE}, an existing file in place is allowed if it
#' it is both readable and writable.
#' Default is \code{FALSE}.
#' @param extension [\code{character(1)}]\cr
#' Extension of the file, e.g. \dQuote{txt} or \dQuote{tar.gz}.
#' @name is_path_for_output
#' @importFrom checkmate test_path_for_output
#' @export
is_path_for_output <- checkmate::test_path_for_output


#' @title is_posixct function from checkmate
#'
#' @description Re-export of \code{checkmate::test_posixct}.
#' See the original package documentation for full details.
#' Checks that an object is of class list(list("POSIXct")).
#' 
#'
#' @param x [any]\cr
#' Object to check.
#' @param lower [\code{\link{Date}}]\cr
#' All non-missing dates in \code{x} must be >= this POSIXct time. Must be provided in the same timezone as \code{x}.
#' @param upper [\code{\link{Date}}]\cr
#' All non-missing dates in \code{x} must be <= this POSIXct time. Must be provided in the same timezone as \code{x}.
#' @param any.missing [\code{logical(1)}]\cr
#' Are vectors with missing values allowed? Default is \code{TRUE}.
#' @param all.missing [\code{logical(1)}]\cr
#' Are vectors with no non-missing values allowed? Default is \code{TRUE}.
#' Note that empty vectors do not have non-missing values.
#' @param len [\code{integer(1)}]\cr
#' Exact expected length of \code{x}.
#' @param min.len [\code{integer(1)}]\cr
#' Minimal length of \code{x}.
#' @param max.len [\code{integer(1)}]\cr
#' Maximal length of \code{x}.
#' @param unique [\code{logical(1)}]\cr
#' Must all values be unique? Default is \code{FALSE}.
#' @param sorted [\code{logical(1)}]\cr
#' Elements must be sorted in ascending order. Missing values are ignored.
#' @param null.ok [\code{logical(1)}]\cr
#' If set to \code{TRUE}, \code{x} may also be \code{NULL}.
#' In this case only a type check of \code{x} is performed, all additional checks are disabled.
#' @name is_posixct
#' @importFrom checkmate test_posixct
#' @export
is_posixct <- checkmate::test_posixct


#' @title is_class function from checkmate
#'
#' @description Re-export of \code{checkmate::test_class}.
#' See the original package documentation for full details.
#' Check the class membership of an argument
#' 
#'
#' @param x [any]\cr
#' Object to check.
#' @param classes [\code{character}]\cr
#' Class names to check for inheritance with \code{\link{inherits}}.
#' \code{x} must inherit from all specified classes.
#' @param ordered [\code{logical(1)}]\cr
#' Expect \code{x} to be specialized in provided order.
#' Default is \code{FALSE}.
#' @param null.ok [\code{logical(1)}]\cr
#' If set to \code{TRUE}, \code{x} may also be \code{NULL}.
#' In this case only a type check of \code{x} is performed, all additional checks are disabled.
#' @name is_class
#' @importFrom checkmate test_class
#' @export
is_class <- checkmate::test_class


#' @title is_flag function from checkmate
#'
#' @description Re-export of \code{checkmate::test_flag}.
#' See the original package documentation for full details.
#' A flag is defined as single logical value.
#' 
#'
#' @param x [any]\cr
#' Object to check.
#' @param na.ok [\code{logical(1)}]\cr
#' Are missing values allowed? Default is \code{FALSE}.
#' @param null.ok [\code{logical(1)}]\cr
#' If set to \code{TRUE}, \code{x} may also be \code{NULL}.
#' In this case only a type check of \code{x} is performed, all additional checks are disabled.
#' @name is_flag
#' @importFrom checkmate test_flag
#' @export
is_flag <- checkmate::test_flag


#' @title is_os function from checkmate
#'
#' @description Re-export of \code{checkmate::test_os}.
#' See the original package documentation for full details.
#' Check the operating system
#' 
#'
#' @param os [\code{character(1)}]\cr
#' Check the operating system to be in a set with possible elements \dQuote{windows},
#' \dQuote{mac}, \dQuote{linux} and \dQuote{solaris}.
#' @name is_os
#' @importFrom checkmate test_os
#' @export
is_os <- checkmate::test_os


#' @title is_existing_test_file function from checkmate
#'
#' @description Re-export of \code{checkmate::test_file_exists}.
#' See the original package documentation for full details.
#' Check existence and access rights of files
#' 
#'
#' @param x [any]\cr
#' Object to check.
#' @param access [\code{character(1)}]\cr
#' Single string containing possible characters \sQuote{r}, \sQuote{w} and \sQuote{x} to
#' force a check for read, write or execute access rights, respectively.
#' Write and executable rights are not checked on Windows.
#' @param extension [\code{character}]\cr
#' Vector of allowed file extensions, matched case insensitive.
#' @name is_existing_test_file
#' @importFrom checkmate test_file_exists
#' @export
is_existing_test_file <- checkmate::test_file_exists


#' @title is_permutation function from checkmate
#'
#' @description Re-export of \code{checkmate::test_permutation}.
#' See the original package documentation for full details.
#' In contrast to list(list("checkSetEqual")), the function tests for a true
#' permutation of the two vectors and also considers duplicated values.
#' Missing values are being treated as actual values by default.
#' Does not work on raw values.
#' 
#'
#' @param x [any]\cr
#' Object to check.
#' @param y [\code{atomic}]\cr
#' Vector to compare with. Atomic vector of type other than raw.
#' @param na.ok [\code{logical(1)}]\cr
#' Are missing values allowed? Default is \code{TRUE}.
#' @name is_permutation
#' @importFrom checkmate test_permutation
#' @export
is_permutation <- checkmate::test_permutation


#' @title is_r6 function from checkmate
#'
#' @description Re-export of \code{checkmate::test_r6}.
#' See the original package documentation for full details.
#' Check if an argument is an R6 class
#' 
#'
#' @param x [any]\cr
#' Object to check.
#' @param classes [\code{character}]\cr
#' Class names to check for inheritance with \code{\link{inherits}}.
#' \code{x} must inherit from all specified classes.
#' @param ordered [\code{logical(1)}]\cr
#' Expect \code{x} to be specialized in provided order.
#' Default is \code{FALSE}.
#' @param cloneable [\code{logical(1)}]\cr
#' If \code{TRUE}, check that \code{x} has a \code{clone} method. If \code{FALSE}, ensure that
#' \code{x} is not cloneable.
#' @param public [\code{character}]\cr
#' Names of expected public slots. This includes active bindings.
#' @param private [\code{character}]\cr
#' Names of expected private slots.
#' @param null.ok [\code{logical(1)}]\cr
#' If set to \code{TRUE}, \code{x} may also be \code{NULL}.
#' In this case only a type check of \code{x} is performed, all additional checks are disabled.
#' @name is_r6
#' @importFrom checkmate test_r6
#' @export
is_r6 <- checkmate::test_r6


#' @title is_data_frame function from checkmate
#'
#' @description Re-export of \code{checkmate::test_data_frame}.
#' See the original package documentation for full details.
#' Check if an argument is a data frame
#' 
#'
#' @param x [any]\cr
#' Object to check.
#' @param types [\code{character}]\cr
#' Character vector of class names. Each list element must inherit
#' from at least one of the provided types.
#' The types \dQuote{logical}, \dQuote{integer}, \dQuote{integerish}, \dQuote{double},
#' \dQuote{numeric}, \dQuote{complex}, \dQuote{character}, \dQuote{factor}, \dQuote{atomic}, \dQuote{vector}
#' \dQuote{atomicvector}, \dQuote{array}, \dQuote{matrix}, \dQuote{list}, \dQuote{function},
#' \dQuote{environment} and \dQuote{null} are supported.
#' For other types \code{\link{inherits}} is used as a fallback to check \code{x}'s inheritance.
#' Defaults to \code{character(0)} (no check).
#' @param any.missing [\code{logical(1)}]\cr
#' Are missing values allowed? Default is \code{TRUE}.
#' @param all.missing [\code{logical(1)}]\cr
#' Are columns with only missing values allowed? Default is \code{TRUE}.
#' @param min.rows [\code{integer(1)}]\cr
#' Minimum number of rows.
#' @param max.rows [\code{integer(1)}]\cr
#' Maximum number of rows.
#' @param min.cols [\code{integer(1)}]\cr
#' Minimum number of columns.
#' @param max.cols [\code{integer(1)}]\cr
#' Maximum number of columns.
#' @param nrows [\code{integer(1)}]\cr
#' Exact number of rows.
#' @param ncols [\code{integer(1)}]\cr
#' Exact number of columns.
#' @param row.names [\code{character(1)}]\cr
#' Check for row names. Default is \dQuote{NULL} (no check).
#' See \code{\link{checkNamed}} for possible values.
#' Note that you can use \code{\link{checkSubset}} to check for a specific set of names.
#' @param col.names [\code{character(1)}]\cr
#' Check for column names. Default is \dQuote{NULL} (no check).
#' See \code{\link{checkNamed}} for possible values.
#' Note that you can use \code{\link{checkSubset}} to test for a specific set of names.
#' @param null.ok [\code{logical(1)}]\cr
#' If set to \code{TRUE}, \code{x} may also be \code{NULL}.
#' In this case only a type check of \code{x} is performed, all additional checks are disabled.
#' @name is_data_frame
#' @importFrom checkmate test_data_frame
#' @export
is_data_frame <- checkmate::test_data_frame


#' @title is_function function from checkmate
#'
#' @description Re-export of \code{checkmate::test_function}.
#' See the original package documentation for full details.
#' Check if an argument is a function
#' 
#'
#' @param x [any]\cr
#' Object to check.
#' @param args [\code{character}]\cr
#' Expected formal arguments. Checks that a function has no arguments if
#' set to \code{character(0)}.
#' Default is \code{NULL} (no check).
#' @param ordered [\code{logical(1)}]\cr
#' Flag whether the arguments provided in \code{args} must be the first
#' \code{length(args)} arguments of the function in the specified order.
#' Default is \code{FALSE}.
#' @param nargs [\code{integer(1)}]\cr
#' Required number of arguments, without \code{...}.
#' Default is \code{NULL} (no check).
#' @param null.ok [\code{logical(1)}]\cr
#' If set to \code{TRUE}, \code{x} may also be \code{NULL}.
#' In this case only a type check of \code{x} is performed, all additional checks are disabled.
#' @name is_function
#' @importFrom checkmate test_function
#' @export
is_function <- checkmate::test_function


#' @title is_environment function from checkmate
#'
#' @description Re-export of \code{checkmate::test_environment}.
#' See the original package documentation for full details.
#' Check if an argument is an environment
#' 
#'
#' @param x [any]\cr
#' Object to check.
#' @param contains [\code{character}]\cr
#' Vector of object names expected in the environment.
#' Defaults to \code{character(0)}.
#' @param null.ok [\code{logical(1)}]\cr
#' If set to \code{TRUE}, \code{x} may also be \code{NULL}.
#' In this case only a type check of \code{x} is performed, all additional checks are disabled.
#' @name is_environment
#' @importFrom checkmate test_environment
#' @export
is_environment <- checkmate::test_environment


#' @title is_integerish function from checkmate
#'
#' @description Re-export of \code{checkmate::test_integerish}.
#' See the original package documentation for full details.
#' An integerish value is defined as value safely convertible to integer.
#' This includes integers and numeric values which sufficiently close to an
#' integer w.r.t. a numeric tolerance `tol`.
#' 
#'
#' @param x [any]\cr
#' Object to check.
#' @param tol [\code{double(1)}]\cr
#' Numerical tolerance used to check whether a double or complex can be converted.
#' Default is \code{sqrt(.Machine$double.eps)}.
#' @param lower [\code{numeric(1)}]\cr
#' Lower value all elements of \code{x} must be greater than or equal to.
#' @param upper [\code{numeric(1)}]\cr
#' Upper value all elements of \code{x} must be lower than or equal to.
#' @param any.missing [\code{logical(1)}]\cr
#' Are vectors with missing values allowed? Default is \code{TRUE}.
#' @param all.missing [\code{logical(1)}]\cr
#' Are vectors with no non-missing values allowed? Default is \code{TRUE}.
#' Note that empty vectors do not have non-missing values.
#' @param len [\code{integer(1)}]\cr
#' Exact expected length of \code{x}.
#' @param min.len [\code{integer(1)}]\cr
#' Minimal length of \code{x}.
#' @param max.len [\code{integer(1)}]\cr
#' Maximal length of \code{x}.
#' @param unique [\code{logical(1)}]\cr
#' Must all values be unique? Default is \code{FALSE}.
#' @param sorted [\code{logical(1)}]\cr
#' Elements must be sorted in ascending order. Missing values are ignored.
#' @param names [\code{character(1)}]\cr
#' Check for names. See \code{\link{checkNamed}} for possible values.
#' Default is \dQuote{any} which performs no check at all.
#' Note that you can use \code{\link{checkSubset}} to check for a specific set of names.
#' @param typed.missing [\code{logical(1)}]\cr
#' If set to \code{FALSE} (default), all types of missing values (\code{NA}, \code{NA_integer_},
#' \code{NA_real_}, \code{NA_character_} or \code{NA_character_}) as well as empty vectors are allowed
#' while type-checking atomic input.
#' Set to \code{TRUE} to enable strict type checking.
#' @param null.ok [\code{logical(1)}]\cr
#' If set to \code{TRUE}, \code{x} may also be \code{NULL}.
#' In this case only a type check of \code{x} is performed, all additional checks are disabled.
#' @name is_integerish
#' @importFrom checkmate test_integerish
#' @export
is_integerish <- checkmate::test_integerish


#' @title is_atomic_vector function from checkmate
#'
#' @description Re-export of \code{checkmate::test_atomic_vector}.
#' See the original package documentation for full details.
#' An atomic vector is defined slightly different from specifications in
#' list(list("is.atomic")) and list(list("is.vector")):
#' An atomic vector is either list("logical"), list("integer"), list("numeric"),
#' list("complex"), list("character") or list("raw") and can have any attributes except a
#' dimension attribute (like matrices).
#' I.e., a list("factor") is an atomic vector, but a matrix or list("NULL") are not.
#' In short, this is basically equivalent to list("is.atomic(x) && !is.null(x) && is.null(dim(x))").
#' 
#'
#' @param x [any]\cr
#' Object to check.
#' @param any.missing [\code{logical(1)}]\cr
#' Are vectors with missing values allowed? Default is \code{TRUE}.
#' @param all.missing [\code{logical(1)}]\cr
#' Are vectors with only missing values allowed? Default is \code{TRUE}.
#' @param len [\code{integer(1)}]\cr
#' Exact expected length of \code{x}.
#' @param min.len [\code{integer(1)}]\cr
#' Minimal length of \code{x}.
#' @param max.len [\code{integer(1)}]\cr
#' Maximal length of \code{x}.
#' @param unique [\code{logical(1)}]\cr
#' Must all values be unique? Default is \code{FALSE}.
#' @param names [\code{character(1)}]\cr
#' Check for names. See \code{\link{checkNamed}} for possible values.
#' Default is \dQuote{any} which performs no check at all.
#' @name is_atomic_vector
#' @importFrom checkmate test_atomic_vector
#' @export
is_atomic_vector <- checkmate::test_atomic_vector


#' @title is_number function from checkmate
#'
#' @description Re-export of \code{checkmate::test_number}.
#' See the original package documentation for full details.
#' Check if an argument is a single numeric value
#' 
#'
#' @param x [any]\cr
#' Object to check.
#' @param na.ok [\code{logical(1)}]\cr
#' Are missing values allowed? Default is \code{FALSE}.
#' @param lower [\code{numeric(1)}]\cr
#' Lower value all elements of \code{x} must be greater than or equal to.
#' @param upper [\code{numeric(1)}]\cr
#' Upper value all elements of \code{x} must be lower than or equal to.
#' @param finite [\code{logical(1)}]\cr
#' Check for only finite values? Default is \code{FALSE}.
#' @param null.ok [\code{logical(1)}]\cr
#' If set to \code{TRUE}, \code{x} may also be \code{NULL}.
#' In this case only a type check of \code{x} is performed, all additional checks are disabled.
#' @name is_number
#' @importFrom checkmate test_number
#' @export
is_number <- checkmate::test_number


#' @title is_logical function from checkmate
#'
#' @description Re-export of \code{checkmate::test_logical}.
#' See the original package documentation for full details.
#' Check if an argument is a vector of type logical
#' 
#'
#' @param x [any]\cr
#' Object to check.
#' @param any.missing [\code{logical(1)}]\cr
#' Are vectors with missing values allowed? Default is \code{TRUE}.
#' @param all.missing [\code{logical(1)}]\cr
#' Are vectors with no non-missing values allowed? Default is \code{TRUE}.
#' Note that empty vectors do not have non-missing values.
#' @param len [\code{integer(1)}]\cr
#' Exact expected length of \code{x}.
#' @param min.len [\code{integer(1)}]\cr
#' Minimal length of \code{x}.
#' @param max.len [\code{integer(1)}]\cr
#' Maximal length of \code{x}.
#' @param unique [\code{logical(1)}]\cr
#' Must all values be unique? Default is \code{FALSE}.
#' @param names [\code{character(1)}]\cr
#' Check for names. See \code{\link{checkNamed}} for possible values.
#' Default is \dQuote{any} which performs no check at all.
#' Note that you can use \code{\link{checkSubset}} to check for a specific set of names.
#' @param typed.missing [\code{logical(1)}]\cr
#' If set to \code{FALSE} (default), all types of missing values (\code{NA}, \code{NA_integer_},
#' \code{NA_real_}, \code{NA_character_} or \code{NA_character_}) as well as empty vectors are allowed
#' while type-checking atomic input.
#' Set to \code{TRUE} to enable strict type checking.
#' @param null.ok [\code{logical(1)}]\cr
#' If set to \code{TRUE}, \code{x} may also be \code{NULL}.
#' In this case only a type check of \code{x} is performed, all additional checks are disabled.
#' @name is_logical
#' @importFrom checkmate test_logical
#' @export
is_logical <- checkmate::test_logical


#' @title is_named function from checkmate
#'
#' @description Re-export of \code{checkmate::test_named}.
#' See the original package documentation for full details.
#' Check if an argument is named
#' 
#'
#' @param x [any]\cr
#' Object to check.
#' @param type [character(1)]\cr
#' Select the check(s) to perform.
#' \dQuote{unnamed} checks \code{x} to be unnamed.
#' \dQuote{named} (default) checks \code{x} to be named which excludes names to be \code{NA} or empty (\code{""}).
#' \dQuote{unique} additionally tests for non-duplicated names.
#' \dQuote{strict} checks for unique names which comply to R's variable name restrictions.
#' Note that for zero-length \code{x} every name check evaluates to \code{TRUE}.
#' @name is_named
#' @importFrom checkmate test_named
#' @export
is_named <- checkmate::test_named


#' @title is_tibble function from checkmate
#'
#' @description Re-export of \code{checkmate::test_tibble}.
#' See the original package documentation for full details.
#' Check if an argument is a tibble
#' 
#'
#' @param x [any]\cr
#' Object to check.
#' @param types [\code{character}]\cr
#' Character vector of class names. Each list element must inherit
#' from at least one of the provided types.
#' The types \dQuote{logical}, \dQuote{integer}, \dQuote{integerish}, \dQuote{double},
#' \dQuote{numeric}, \dQuote{complex}, \dQuote{character}, \dQuote{factor}, \dQuote{atomic}, \dQuote{vector}
#' \dQuote{atomicvector}, \dQuote{array}, \dQuote{matrix}, \dQuote{list}, \dQuote{function},
#' \dQuote{environment} and \dQuote{null} are supported.
#' For other types \code{\link{inherits}} is used as a fallback to check \code{x}'s inheritance.
#' Defaults to \code{character(0)} (no check).
#' @param any.missing [\code{logical(1)}]\cr
#' Are missing values allowed? Default is \code{TRUE}.
#' @param all.missing [\code{logical(1)}]\cr
#' Are matrices with only missing values allowed? Default is \code{TRUE}.
#' @param min.rows [\code{integer(1)}]\cr
#' Minimum number of rows.
#' @param max.rows [\code{integer(1)}]\cr
#' Maximum number of rows.
#' @param min.cols [\code{integer(1)}]\cr
#' Minimum number of columns.
#' @param max.cols [\code{integer(1)}]\cr
#' Maximum number of columns.
#' @param nrows [\code{integer(1)}]\cr
#' Exact number of rows.
#' @param ncols [\code{integer(1)}]\cr
#' Exact number of columns.
#' @param row.names [\code{character(1)}]\cr
#' Check for row names. Default is \dQuote{NULL} (no check).
#' See \code{\link{checkNamed}} for possible values.
#' Note that you can use \code{\link{checkSubset}} to check for a specific set of names.
#' @param col.names [\code{character(1)}]\cr
#' Check for column names. Default is \dQuote{NULL} (no check).
#' See \code{\link{checkNamed}} for possible values.
#' Note that you can use \code{\link{checkSubset}} to test for a specific set of names.
#' @param null.ok [\code{logical(1)}]\cr
#' If set to \code{TRUE}, \code{x} may also be \code{NULL}.
#' In this case only a type check of \code{x} is performed, all additional checks are disabled.
#' @name is_tibble
#' @importFrom checkmate test_tibble
#' @export
is_tibble <- checkmate::test_tibble


#' @title is_directory function from checkmate
#'
#' @description Re-export of \code{checkmate::test_directory}.
#' See the original package documentation for full details.
#' Check for existence and access rights of directories
#' 
#'
#' @param x [any]\cr
#' Object to check.
#' @param access [\code{character(1)}]\cr
#' Single string containing possible characters \sQuote{r}, \sQuote{w} and \sQuote{x} to
#' force a check for read, write or execute access rights, respectively.
#' Write and executable rights are not checked on Windows.
#' @name is_directory
#' @importFrom checkmate test_directory
#' @export
is_directory <- checkmate::test_directory


#' @title is_int function from checkmate
#'
#' @description Re-export of \code{checkmate::test_int}.
#' See the original package documentation for full details.
#' Check if an argument is a single integerish value
#' 
#'
#' @param x [any]\cr
#' Object to check.
#' @param na.ok [\code{logical(1)}]\cr
#' Are missing values allowed? Default is \code{FALSE}.
#' @param lower [\code{numeric(1)}]\cr
#' Lower value all elements of \code{x} must be greater than or equal to.
#' @param upper [\code{numeric(1)}]\cr
#' Upper value all elements of \code{x} must be lower than or equal to.
#' @param tol [\code{double(1)}]\cr
#' Numerical tolerance used to check whether a double or complex can be converted.
#' Default is \code{sqrt(.Machine$double.eps)}.
#' @param null.ok [\code{logical(1)}]\cr
#' If set to \code{TRUE}, \code{x} may also be \code{NULL}.
#' In this case only a type check of \code{x} is performed, all additional checks are disabled.
#' @name is_int
#' @importFrom checkmate test_int
#' @export
is_int <- checkmate::test_int


#' @title is_complex function from checkmate
#'
#' @description Re-export of \code{checkmate::test_complex}.
#' See the original package documentation for full details.
#' Check if an argument is a vector of type complex
#' 
#'
#' @param x [any]\cr
#' Object to check.
#' @param any.missing [\code{logical(1)}]\cr
#' Are vectors with missing values allowed? Default is \code{TRUE}.
#' @param all.missing [\code{logical(1)}]\cr
#' Are vectors with no non-missing values allowed? Default is \code{TRUE}.
#' Note that empty vectors do not have non-missing values.
#' @param len [\code{integer(1)}]\cr
#' Exact expected length of \code{x}.
#' @param min.len [\code{integer(1)}]\cr
#' Minimal length of \code{x}.
#' @param max.len [\code{integer(1)}]\cr
#' Maximal length of \code{x}.
#' @param unique [\code{logical(1)}]\cr
#' Must all values be unique? Default is \code{FALSE}.
#' @param names [\code{character(1)}]\cr
#' Check for names. See \code{\link{checkNamed}} for possible values.
#' Default is \dQuote{any} which performs no check at all.
#' Note that you can use \code{\link{checkSubset}} to check for a specific set of names.
#' @param typed.missing [\code{logical(1)}]\cr
#' If set to \code{FALSE} (default), all types of missing values (\code{NA}, \code{NA_integer_},
#' \code{NA_real_}, \code{NA_character_} or \code{NA_character_}) as well as empty vectors are allowed
#' while type-checking atomic input.
#' Set to \code{TRUE} to enable strict type checking.
#' @param null.ok [\code{logical(1)}]\cr
#' If set to \code{TRUE}, \code{x} may also be \code{NULL}.
#' In this case only a type check of \code{x} is performed, all additional checks are disabled.
#' @name is_complex
#' @importFrom checkmate test_complex
#' @export
is_complex <- checkmate::test_complex


#' @title is_vector function from checkmate
#'
#' @description Re-export of \code{checkmate::test_vector}.
#' See the original package documentation for full details.
#' Check if an argument is a vector
#' 
#'
#' @param x [any]\cr
#' Object to check.
#' @param strict [\code{logical(1)}]\cr
#' May the vector have additional attributes? If \code{TRUE}, mimics the behavior of
#' \code{\link{is.vector}}.
#' Default is \code{FALSE} which allows e.g. \code{factor}s or \code{data.frame}s
#' to be recognized as vectors.
#' @param any.missing [\code{logical(1)}]\cr
#' Are vectors with missing values allowed? Default is \code{TRUE}.
#' @param all.missing [\code{logical(1)}]\cr
#' Are vectors with no non-missing values allowed? Default is \code{TRUE}.
#' Note that empty vectors do not have non-missing values.
#' @param len [\code{integer(1)}]\cr
#' Exact expected length of \code{x}.
#' @param min.len [\code{integer(1)}]\cr
#' Minimal length of \code{x}.
#' @param max.len [\code{integer(1)}]\cr
#' Maximal length of \code{x}.
#' @param unique [\code{logical(1)}]\cr
#' Must all values be unique? Default is \code{FALSE}.
#' @param names [\code{character(1)}]\cr
#' Check for names. See \code{\link{checkNamed}} for possible values.
#' Default is \dQuote{any} which performs no check at all.
#' Note that you can use \code{\link{checkSubset}} to check for a specific set of names.
#' @param null.ok [\code{logical(1)}]\cr
#' If set to \code{TRUE}, \code{x} may also be \code{NULL}.
#' In this case only a type check of \code{x} is performed, all additional checks are disabled.
#' @name is_vector
#' @importFrom checkmate test_vector
#' @export
is_vector <- checkmate::test_vector


#' @title is_array function from checkmate
#'
#' @description Re-export of \code{checkmate::test_array}.
#' See the original package documentation for full details.
#' Check if an argument is an array
#' 
#'
#' @param x [any]\cr
#' Object to check.
#' @param mode [\code{character(1)}]\cr
#' Storage mode of the array. Arrays can hold vectors, i.e. \dQuote{logical},
#' \dQuote{integer}, \dQuote{integerish}, \dQuote{double}, \dQuote{numeric}, \dQuote{complex},
#' \dQuote{character} and \dQuote{list}. You can also specify \dQuote{atomic}
#' here to explicitly prohibit lists. Default is \code{NULL} (no check).
#' If all values of \code{x} are missing, this check is skipped.
#' @param any.missing [\code{logical(1)}]\cr
#' Are missing values allowed? Default is \code{TRUE}.
#' @param d [\code{integer(1)}]\cr
#' Exact number of dimensions of array \code{x}.
#' Default is \code{NULL} (no check).
#' @param min.d [\code{integer(1)}]\cr
#' Minimum number of dimensions of array \code{x}.
#' Default is \code{NULL} (no check).
#' @param max.d [\code{integer(1)}]\cr
#' Maximum number of dimensions of array \code{x}.
#' Default is \code{NULL} (no check).
#' @param null.ok [\code{logical(1)}]\cr
#' If set to \code{TRUE}, \code{x} may also be \code{NULL}.
#' In this case only a type check of \code{x} is performed, all additional checks are disabled.
#' @name is_array
#' @importFrom checkmate test_array
#' @export
is_array <- checkmate::test_array


#' @title is_date function from checkmate
#'
#' @description Re-export of \code{checkmate::test_date}.
#' See the original package documentation for full details.
#' Checks that an object is of class list(list("Date")).
#' 
#'
#' @param x [any]\cr
#' Object to check.
#' @param lower [\code{\link{Date}}]\cr
#' All non-missing dates in \code{x} must be >= this date. Comparison is done via \code{\link{Ops.Date}}.
#' @param upper [\code{\link{Date}}]\cr
#' All non-missing dates in \code{x} must be before <= this date. Comparison is done via \code{\link{Ops.Date}}.
#' @param any.missing [\code{logical(1)}]\cr
#' Are vectors with missing values allowed? Default is \code{TRUE}.
#' @param all.missing [\code{logical(1)}]\cr
#' Are vectors with no non-missing values allowed? Default is \code{TRUE}.
#' Note that empty vectors do not have non-missing values.
#' @param len [\code{integer(1)}]\cr
#' Exact expected length of \code{x}.
#' @param min.len [\code{integer(1)}]\cr
#' Minimal length of \code{x}.
#' @param max.len [\code{integer(1)}]\cr
#' Maximal length of \code{x}.
#' @param unique [\code{logical(1)}]\cr
#' Must all values be unique? Default is \code{FALSE}.
#' @param null.ok [\code{logical(1)}]\cr
#' If set to \code{TRUE}, \code{x} may also be \code{NULL}.
#' In this case only a type check of \code{x} is performed, all additional checks are disabled.
#' @name is_date
#' @importFrom checkmate test_date
#' @export
is_date <- checkmate::test_date


#' @title is_raw function from checkmate
#'
#' @description Re-export of \code{checkmate::test_raw}.
#' See the original package documentation for full details.
#' Check if an argument is a raw vector
#' 
#'
#' @param x [any]\cr
#' Object to check.
#' @param len [\code{integer(1)}]\cr
#' Exact expected length of \code{x}.
#' @param min.len [\code{integer(1)}]\cr
#' Minimal length of \code{x}.
#' @param max.len [\code{integer(1)}]\cr
#' Maximal length of \code{x}.
#' @param names [\code{character(1)}]\cr
#' Check for names. See \code{\link{checkNamed}} for possible values.
#' Default is \dQuote{any} which performs no check at all.
#' Note that you can use \code{\link{checkSubset}} to check for a specific set of names.
#' @param null.ok [\code{logical(1)}]\cr
#' If set to \code{TRUE}, \code{x} may also be \code{NULL}.
#' In this case only a type check of \code{x} is performed, all additional checks are disabled.
#' @name is_raw
#' @importFrom checkmate test_raw
#' @export
is_raw <- checkmate::test_raw


#' @title is_count function from checkmate
#'
#' @description Re-export of \code{checkmate::test_count}.
#' See the original package documentation for full details.
#' A count is defined as non-negative integerish value.
#' 
#'
#' @param x [any]\cr
#' Object to check.
#' @param na.ok [\code{logical(1)}]\cr
#' Are missing values allowed? Default is \code{FALSE}.
#' @param positive [\code{logical(1)}]\cr
#' Must \code{x} be positive (>= 1)?
#' Default is \code{FALSE}, allowing 0.
#' @param tol [\code{double(1)}]\cr
#' Numerical tolerance used to check whether a double or complex can be converted.
#' Default is \code{sqrt(.Machine$double.eps)}.
#' @param null.ok [\code{logical(1)}]\cr
#' If set to \code{TRUE}, \code{x} may also be \code{NULL}.
#' In this case only a type check of \code{x} is performed, all additional checks are disabled.
#' @name is_count
#' @importFrom checkmate test_count
#' @export
is_count <- checkmate::test_count


#' @title is_integer function from checkmate
#'
#' @description Re-export of \code{checkmate::test_integer}.
#' See the original package documentation for full details.
#' Check if an argument is vector of type integer
#' 
#'
#' @param x [any]\cr
#' Object to check.
#' @param lower [\code{numeric(1)}]\cr
#' Lower value all elements of \code{x} must be greater than or equal to.
#' @param upper [\code{numeric(1)}]\cr
#' Upper value all elements of \code{x} must be lower than or equal to.
#' @param any.missing [\code{logical(1)}]\cr
#' Are vectors with missing values allowed? Default is \code{TRUE}.
#' @param all.missing [\code{logical(1)}]\cr
#' Are vectors with no non-missing values allowed? Default is \code{TRUE}.
#' Note that empty vectors do not have non-missing values.
#' @param len [\code{integer(1)}]\cr
#' Exact expected length of \code{x}.
#' @param min.len [\code{integer(1)}]\cr
#' Minimal length of \code{x}.
#' @param max.len [\code{integer(1)}]\cr
#' Maximal length of \code{x}.
#' @param unique [\code{logical(1)}]\cr
#' Must all values be unique? Default is \code{FALSE}.
#' @param sorted [\code{logical(1)}]\cr
#' Elements must be sorted in ascending order. Missing values are ignored.
#' @param names [\code{character(1)}]\cr
#' Check for names. See \code{\link{checkNamed}} for possible values.
#' Default is \dQuote{any} which performs no check at all.
#' Note that you can use \code{\link{checkSubset}} to check for a specific set of names.
#' @param typed.missing [\code{logical(1)}]\cr
#' If set to \code{FALSE} (default), all types of missing values (\code{NA}, \code{NA_integer_},
#' \code{NA_real_}, \code{NA_character_} or \code{NA_character_}) as well as empty vectors are allowed
#' while type-checking atomic input.
#' Set to \code{TRUE} to enable strict type checking.
#' @param null.ok [\code{logical(1)}]\cr
#' If set to \code{TRUE}, \code{x} may also be \code{NULL}.
#' In this case only a type check of \code{x} is performed, all additional checks are disabled.
#' @name is_integer
#' @importFrom checkmate test_integer
#' @export
is_integer <- checkmate::test_integer


#' @title is_data_table function from checkmate
#'
#' @description Re-export of \code{checkmate::test_data_table}.
#' See the original package documentation for full details.
#' Check if an argument is a data table
#' 
#'
#' @param x [any]\cr
#' Object to check.
#' @param key [\code{character}]\cr
#' Expected primary key(s) of the data table.
#' @param index [\code{character}]\cr
#' Expected secondary key(s) of the data table.
#' @param types [\code{character}]\cr
#' Character vector of class names. Each list element must inherit
#' from at least one of the provided types.
#' The types \dQuote{logical}, \dQuote{integer}, \dQuote{integerish}, \dQuote{double},
#' \dQuote{numeric}, \dQuote{complex}, \dQuote{character}, \dQuote{factor}, \dQuote{atomic}, \dQuote{vector}
#' \dQuote{atomicvector}, \dQuote{array}, \dQuote{matrix}, \dQuote{list}, \dQuote{function},
#' \dQuote{environment} and \dQuote{null} are supported.
#' For other types \code{\link{inherits}} is used as a fallback to check \code{x}'s inheritance.
#' Defaults to \code{character(0)} (no check).
#' @param any.missing [\code{logical(1)}]\cr
#' Are missing values allowed? Default is \code{TRUE}.
#' @param all.missing [\code{logical(1)}]\cr
#' Are matrices with only missing values allowed? Default is \code{TRUE}.
#' @param min.rows [\code{integer(1)}]\cr
#' Minimum number of rows.
#' @param max.rows [\code{integer(1)}]\cr
#' Maximum number of rows.
#' @param min.cols [\code{integer(1)}]\cr
#' Minimum number of columns.
#' @param max.cols [\code{integer(1)}]\cr
#' Maximum number of columns.
#' @param nrows [\code{integer(1)}]\cr
#' Exact number of rows.
#' @param ncols [\code{integer(1)}]\cr
#' Exact number of columns.
#' @param row.names [\code{character(1)}]\cr
#' Check for row names. Default is \dQuote{NULL} (no check).
#' See \code{\link{checkNamed}} for possible values.
#' Note that you can use \code{\link{checkSubset}} to check for a specific set of names.
#' @param col.names [\code{character(1)}]\cr
#' Check for column names. Default is \dQuote{NULL} (no check).
#' See \code{\link{checkNamed}} for possible values.
#' Note that you can use \code{\link{checkSubset}} to test for a specific set of names.
#' @param null.ok [\code{logical(1)}]\cr
#' If set to \code{TRUE}, \code{x} may also be \code{NULL}.
#' In this case only a type check of \code{x} is performed, all additional checks are disabled.
#' @name is_data_table
#' @importFrom checkmate test_data_table
#' @export
is_data_table <- checkmate::test_data_table


#' @title is_subset function from checkmate
#'
#' @description Re-export of \code{checkmate::test_subset}.
#' See the original package documentation for full details.
#' Check if an argument is a subset of a given set
#' 
#'
#' @param x [any]\cr
#' Object to check.
#' @param choices [\code{atomic}]\cr
#' Set of possible values. May be empty.
#' @param empty.ok [\code{logical(1)}]\cr
#' Treat zero-length \code{x} as subset of any set \code{choices} (this includes \code{NULL})?
#' Default is \code{TRUE}.
#' @param fmatch [\code{logical(1)}]\cr
#' Use the set operations implemented in \code{\link{fmatch}} in package \pkg{fastmatch}.
#' If \pkg{fastmatch} is not installed, this silently falls back to \code{\link{match}}.
#' \code{\link{fmatch}} modifies \code{y} by reference:
#' A hash table is added as attribute which is used in subsequent calls.
#' @name is_subset
#' @importFrom checkmate test_subset
#' @export
is_subset <- checkmate::test_subset


#' @title is_names function from checkmate
#'
#' @description Re-export of \code{checkmate::test_names}.
#' See the original package documentation for full details.
#' Performs various checks on character vectors, usually names.
#' 
#'
#' @param x [\code{character} || \code{NULL}]\cr
#' Names to check using rules defined via \code{type}.
#' @param type [character(1)]\cr
#' Type of formal check(s) to perform on the names.
#' \describe{
#' \item{unnamed:}{Checks \code{x} to be \code{NULL}.}
#' \item{named:}{Checks \code{x} for regular names which excludes names to be \code{NA} or empty (\code{""}).}
#' \item{unique:}{Performs checks like with \dQuote{named} and additionally tests for non-duplicated names.}
#' \item{strict:}{Performs checks like with \dQuote{unique} and additionally fails for names with UTF-8 characters and names which do not comply to R's variable name restrictions.
#'   As regular expression, this is \dQuote{^[.]*[a-zA-Z]+[a-zA-Z0-9._]*$}.}
#' \item{ids:}{Same as \dQuote{strict}, but does not enforce uniqueness.}
#' }
#' Note that for zero-length \code{x}, all these name checks evaluate to \code{TRUE}.
#' @param subset.of [\code{character}]\cr
#' Names provided in \code{x} must be subset of the set \code{subset.of}.
#' @param must.include [\code{character}]\cr
#' Names provided in \code{x} must be a superset of the set \code{must.include}.
#' @param permutation.of [\code{character}]\cr
#' Names provided in \code{x} must be a permutation of the set \code{permutation.of}.
#' Duplicated names in \code{permutation.of} are stripped out and duplicated names in \code{x}
#' thus lead to a failed check.
#' Use this argument instead of \code{identical.to} if the order of the names is not relevant.
#' @param identical.to [\code{character}]\cr
#' Names provided in \code{x} must be identical to the vector \code{identical.to}.
#' Use this argument instead of \code{permutation.of} if the order of the names is relevant.
#' @param disjunct.from [\code{character}]\cr
#' Names provided in \code{x} must may not be present in the vector \code{disjunct.from}.
#' @param what [\code{character(1)}]\cr
#' Type of name vector to check, e.g. \dQuote{names} (default), \dQuote{colnames} or \dQuote{rownames}.
#' @name is_names
#' @importFrom checkmate test_names
#' @export
is_names <- checkmate::test_names


#' @title is_set_equal function from checkmate
#'
#' @description Re-export of \code{checkmate::test_set_equal}.
#' See the original package documentation for full details.
#' Check if an argument is equal to a given set
#' 
#'
#' @param x [any]\cr
#' Object to check.
#' @param y [\code{atomic}]\cr
#' Set to compare with.
#' @param ordered [\code{logical(1)}]\cr
#' Check \code{x} to have the same length and order as \code{y}, i.e.
#' check using \dQuote{==} while handling \code{NA}s nicely.
#' Default is \code{FALSE}.
#' @param fmatch [\code{logical(1)}]\cr
#' Use the set operations implemented in \code{\link{fmatch}} in package \pkg{fastmatch}.
#' If \pkg{fastmatch} is not installed, this silently falls back to \code{\link{match}}.
#' \code{\link{fmatch}} modifies \code{y} by reference:
#' A hash table is added as attribute which is used in subsequent calls.
#' @name is_set_equal
#' @importFrom checkmate test_set_equal
#' @export
is_set_equal <- checkmate::test_set_equal


#' @title is_double function from checkmate
#'
#' @description Re-export of \code{checkmate::test_double}.
#' See the original package documentation for full details.
#' Check that an argument is a vector of type double
#' 
#'
#' @param x [any]\cr
#' Object to check.
#' @param lower [\code{numeric(1)}]\cr
#' Lower value all elements of \code{x} must be greater than or equal to.
#' @param upper [\code{numeric(1)}]\cr
#' Upper value all elements of \code{x} must be lower than or equal to.
#' @param finite [\code{logical(1)}]\cr
#' Check for only finite values? Default is \code{FALSE}.
#' @param any.missing [\code{logical(1)}]\cr
#' Are vectors with missing values allowed? Default is \code{TRUE}.
#' @param all.missing [\code{logical(1)}]\cr
#' Are vectors with no non-missing values allowed? Default is \code{TRUE}.
#' Note that empty vectors do not have non-missing values.
#' @param len [\code{integer(1)}]\cr
#' Exact expected length of \code{x}.
#' @param min.len [\code{integer(1)}]\cr
#' Minimal length of \code{x}.
#' @param max.len [\code{integer(1)}]\cr
#' Maximal length of \code{x}.
#' @param unique [\code{logical(1)}]\cr
#' Must all values be unique? Default is \code{FALSE}.
#' @param sorted [\code{logical(1)}]\cr
#' Elements must be sorted in ascending order. Missing values are ignored.
#' @param names [\code{character(1)}]\cr
#' Check for names. See \code{\link{checkNamed}} for possible values.
#' Default is \dQuote{any} which performs no check at all.
#' Note that you can use \code{\link{checkSubset}} to check for a specific set of names.
#' @param typed.missing [\code{logical(1)}]\cr
#' If set to \code{FALSE} (default), all types of missing values (\code{NA}, \code{NA_integer_},
#' \code{NA_real_}, \code{NA_character_} or \code{NA_character_}) as well as empty vectors are allowed
#' while type-checking atomic input.
#' Set to \code{TRUE} to enable strict type checking.
#' @param null.ok [\code{logical(1)}]\cr
#' If set to \code{TRUE}, \code{x} may also be \code{NULL}.
#' In this case only a type check of \code{x} is performed, all additional checks are disabled.
#' @name is_double
#' @importFrom checkmate test_double
#' @export
is_double <- checkmate::test_double


#' @title is_true function from checkmate
#'
#' @description Re-export of \code{checkmate::test_true}.
#' See the original package documentation for full details.
#' Simply checks if an argument is list("TRUE").
#' 
#'
#' @param x [any]\cr
#' Object to check.
#' @param na.ok [\code{logical(1)}]\cr
#' Are missing values allowed? Default is \code{FALSE}.
#' @name is_true
#' @importFrom checkmate test_true
#' @export
is_true <- checkmate::test_true


#' @title is_multi_class function from checkmate
#'
#' @description Re-export of \code{checkmate::test_multi_class}.
#' See the original package documentation for full details.
#' Check the class membership of an argument
#' 
#'
#' @param x [any]\cr
#' Object to check.
#' @param classes [\code{character}]\cr
#' Class names to check for inheritance with \code{\link{inherits}}.
#' \code{x} must inherit from any of the specified classes.
#' @param null.ok [\code{logical(1)}]\cr
#' If set to \code{TRUE}, \code{x} may also be \code{NULL}.
#' In this case only a type check of \code{x} is performed, all additional checks are disabled.
#' @name is_multi_class
#' @importFrom checkmate test_multi_class
#' @export
is_multi_class <- checkmate::test_multi_class


#' @title is_null function from checkmate
#'
#' @description Re-export of \code{checkmate::test_null}.
#' See the original package documentation for full details.
#' Check if an argument is NULL
#' 
#'
#' @param x [any]\cr
#' Object to check.
#' @name is_null
#' @importFrom checkmate test_null
#' @export
is_null <- checkmate::test_null


#' @title is_scalar function from checkmate
#'
#' @description Re-export of \code{checkmate::test_scalar}.
#' See the original package documentation for full details.
#' Check if an argument is a single atomic value
#' 
#'
#' @param x [any]\cr
#' Object to check.
#' @param na.ok [\code{logical(1)}]\cr
#' Are missing values allowed? Default is \code{FALSE}.
#' @param null.ok [\code{logical(1)}]\cr
#' If set to \code{TRUE}, \code{x} may also be \code{NULL}.
#' In this case only a type check of \code{x} is performed, all additional checks are disabled.
#' @name is_scalar
#' @importFrom checkmate test_scalar
#' @export
is_scalar <- checkmate::test_scalar


#' @title is_false function from checkmate
#'
#' @description Re-export of \code{checkmate::test_false}.
#' See the original package documentation for full details.
#' Simply checks if an argument is list("FALSE").
#' 
#'
#' @param x [any]\cr
#' Object to check.
#' @param na.ok [\code{logical(1)}]\cr
#' Are missing values allowed? Default is \code{FALSE}.
#' @name is_false
#' @importFrom checkmate test_false
#' @export
is_false <- checkmate::test_false


#' @title is_formula function from checkmate
#'
#' @description Re-export of \code{checkmate::test_formula}.
#' See the original package documentation for full details.
#' Check if an argument is a formula
#' 
#'
#' @param x [any]\cr
#' Object to check.
#' @param null.ok [\code{logical(1)}]\cr
#' If set to \code{TRUE}, \code{x} may also be \code{NULL}.
#' In this case only a type check of \code{x} is performed, all additional checks are disabled.
#' @name is_formula
#' @importFrom checkmate test_formula
#' @export
is_formula <- checkmate::test_formula


#' @title is_disjunct function from checkmate
#'
#' @description Re-export of \code{checkmate::test_disjunct}.
#' See the original package documentation for full details.
#' Check if an argument is disjunct from a given set
#' 
#'
#' @param x [any]\cr
#' Object to check.
#' @param y [\code{atomic}]\cr
#' Other Set.
#' @param fmatch [\code{logical(1)}]\cr
#' Use the set operations implemented in \code{\link{fmatch}} in package \pkg{fastmatch}.
#' If \pkg{fastmatch} is not installed, this silently falls back to \code{\link{match}}.
#' \code{\link{fmatch}} modifies \code{y} by reference:
#' A hash table is added as attribute which is used in subsequent calls.
#' @name is_disjunct
#' @importFrom checkmate test_disjunct
#' @export
is_disjunct <- checkmate::test_disjunct


#' @title is_existing_test_directory function from checkmate
#'
#' @description Re-export of \code{checkmate::test_directory_exists}.
#' See the original package documentation for full details.
#' Check for existence and access rights of directories
#' 
#'
#' @param x [any]\cr
#' Object to check.
#' @param access [\code{character(1)}]\cr
#' Single string containing possible characters \sQuote{r}, \sQuote{w} and \sQuote{x} to
#' force a check for read, write or execute access rights, respectively.
#' Write and executable rights are not checked on Windows.
#' @name is_existing_test_directory
#' @importFrom checkmate test_directory_exists
#' @export
is_existing_test_directory <- checkmate::test_directory_exists


#' @title is_atomic function from checkmate
#'
#' @description Re-export of \code{checkmate::test_atomic}.
#' See the original package documentation for full details.
#' For the definition of list("atomic"), see list(list("is.atomic")).
#' 
#' Note that `NULL` is recognized as a valid atomic value, as in R versions up to version 4.3.x.
#' For details, see list("https://stat.ethz.ch/pipermail/r-devel/2023-September/082892.html").
#' 
#'
#' @param x [any]\cr
#' Object to check.
#' @param any.missing [\code{logical(1)}]\cr
#' Are vectors with missing values allowed? Default is \code{TRUE}.
#' @param all.missing [\code{logical(1)}]\cr
#' Are vectors with no non-missing values allowed? Default is \code{TRUE}.
#' Note that empty vectors do not have non-missing values.
#' @param len [\code{integer(1)}]\cr
#' Exact expected length of \code{x}.
#' @param min.len [\code{integer(1)}]\cr
#' Minimal length of \code{x}.
#' @param max.len [\code{integer(1)}]\cr
#' Maximal length of \code{x}.
#' @param unique [\code{logical(1)}]\cr
#' Must all values be unique? Default is \code{FALSE}.
#' @param names [\code{character(1)}]\cr
#' Check for names. See \code{\link{checkNamed}} for possible values.
#' Default is \dQuote{any} which performs no check at all.
#' Note that you can use \code{\link{checkSubset}} to check for a specific set of names.
#' @name is_atomic
#' @importFrom checkmate test_atomic
#' @export
is_atomic <- checkmate::test_atomic


#' @title is_choice function from checkmate
#'
#' @description Re-export of \code{checkmate::test_choice}.
#' See the original package documentation for full details.
#' Check if an object is an element of a given set
#' 
#'
#' @param x [any]\cr
#' Object to check.
#' @param choices [\code{atomic}]\cr
#' Set of possible values.
#' @param null.ok [\code{logical(1)}]\cr
#' If set to \code{TRUE}, \code{x} may also be \code{NULL}.
#' In this case only a type check of \code{x} is performed, all additional checks are disabled.
#' @param fmatch [\code{logical(1)}]\cr
#' Use the set operations implemented in \code{\link{fmatch}} in package \pkg{fastmatch}.
#' If \pkg{fastmatch} is not installed, this silently falls back to \code{\link{match}}.
#' \code{\link{fmatch}} modifies \code{y} by reference:
#' A hash table is added as attribute which is used in subsequent calls.
#' @name is_choice
#' @importFrom checkmate test_choice
#' @export
is_choice <- checkmate::test_choice


#' @title is_access function from checkmate
#'
#' @description Re-export of \code{checkmate::test_access}.
#' See the original package documentation for full details.
#' Check file system access rights
#' 
#'
#' @param x [any]\cr
#' Object to check.
#' @param access [\code{character(1)}]\cr
#' Single string containing possible characters \sQuote{r}, \sQuote{w} and \sQuote{x} to
#' force a check for read, write or execute access rights, respectively.
#' Write and executable rights are not checked on Windows.
#' @name is_access
#' @importFrom checkmate test_access
#' @export
is_access <- checkmate::test_access


#' @title is_list function from checkmate
#'
#' @description Re-export of \code{checkmate::test_list}.
#' See the original package documentation for full details.
#' Check if an argument is a list
#' 
#'
#' @param x [any]\cr
#' Object to check.
#' @param types [\code{character}]\cr
#' Character vector of class names. Each list element must inherit
#' from at least one of the provided types.
#' The types \dQuote{logical}, \dQuote{integer}, \dQuote{integerish}, \dQuote{double},
#' \dQuote{numeric}, \dQuote{complex}, \dQuote{character}, \dQuote{factor}, \dQuote{atomic}, \dQuote{vector}
#' \dQuote{atomicvector}, \dQuote{array}, \dQuote{matrix}, \dQuote{list}, \dQuote{function},
#' \dQuote{environment} and \dQuote{null} are supported.
#' For other types \code{\link{inherits}} is used as a fallback to check \code{x}'s inheritance.
#' Defaults to \code{character(0)} (no check).
#' @param any.missing [\code{logical(1)}]\cr
#' Are vectors with missing values allowed? Default is \code{TRUE}.
#' @param all.missing [\code{logical(1)}]\cr
#' Are vectors with no non-missing values allowed? Default is \code{TRUE}.
#' Note that empty vectors do not have non-missing values.
#' @param len [\code{integer(1)}]\cr
#' Exact expected length of \code{x}.
#' @param min.len [\code{integer(1)}]\cr
#' Minimal length of \code{x}.
#' @param max.len [\code{integer(1)}]\cr
#' Maximal length of \code{x}.
#' @param unique [\code{logical(1)}]\cr
#' Must all values be unique? Default is \code{FALSE}.
#' @param names [\code{character(1)}]\cr
#' Check for names. See \code{\link{checkNamed}} for possible values.
#' Default is \dQuote{any} which performs no check at all.
#' Note that you can use \code{\link{checkSubset}} to check for a specific set of names.
#' @param null.ok [\code{logical(1)}]\cr
#' If set to \code{TRUE}, \code{x} may also be \code{NULL}.
#' In this case only a type check of \code{x} is performed, all additional checks are disabled.
#' @name is_list
#' @importFrom checkmate test_list
#' @export
is_list <- checkmate::test_list


#' @title is_character function from checkmate
#'
#' @description Re-export of \code{checkmate::test_character}.
#' See the original package documentation for full details.
#' To check for scalar strings, see list(list("checkString")).
#' 
#'
#' @param x [any]\cr
#' Object to check.
#' @param n.chars [\code{integer(1)}]\cr
#' Exact number of characters for each element of \code{x}.
#' @param min.chars [\code{integer(1)}]\cr
#' Minimum number of characters for each element of \code{x}.
#' @param max.chars [\code{integer(1)}]\cr
#' Maximum number of characters for each element of \code{x}.
#' @param pattern [\code{character(1L)}]\cr
#' Regular expression as used in \code{\link{grepl}}.
#' All non-missing elements of \code{x} must comply to this pattern.
#' @param fixed [\code{character(1)}]\cr
#' Substring to detect in \code{x}. Will be used as \code{pattern} in \code{\link{grepl}}
#' with option \code{fixed} set to \code{TRUE}.
#' All non-missing elements of \code{x} must contain this substring.
#' @param ignore.case [\code{logical(1)}]\cr
#' See \code{\link{grepl}}. Default is \code{FALSE}.
#' @param any.missing [\code{logical(1)}]\cr
#' Are vectors with missing values allowed? Default is \code{TRUE}.
#' @param all.missing [\code{logical(1)}]\cr
#' Are vectors with no non-missing values allowed? Default is \code{TRUE}.
#' Note that empty vectors do not have non-missing values.
#' @param len [\code{integer(1)}]\cr
#' Exact expected length of \code{x}.
#' @param min.len [\code{integer(1)}]\cr
#' Minimal length of \code{x}.
#' @param max.len [\code{integer(1)}]\cr
#' Maximal length of \code{x}.
#' @param unique [\code{logical(1)}]\cr
#' Must all values be unique? Default is \code{FALSE}.
#' @param sorted [\code{logical(1)}]\cr
#' Elements must be sorted in ascending order. Missing values are ignored.
#' @param names [\code{character(1)}]\cr
#' Check for names. See \code{\link{checkNamed}} for possible values.
#' Default is \dQuote{any} which performs no check at all.
#' Note that you can use \code{\link{checkSubset}} to check for a specific set of names.
#' @param typed.missing [\code{logical(1)}]\cr
#' If set to \code{FALSE} (default), all types of missing values (\code{NA}, \code{NA_integer_},
#' \code{NA_real_}, \code{NA_character_} or \code{NA_character_}) as well as empty vectors are allowed
#' while type-checking atomic input.
#' Set to \code{TRUE} to enable strict type checking.
#' @param null.ok [\code{logical(1)}]\cr
#' If set to \code{TRUE}, \code{x} may also be \code{NULL}.
#' In this case only a type check of \code{x} is performed, all additional checks are disabled.
#' @name is_character
#' @importFrom checkmate test_character
#' @export
is_character <- checkmate::test_character


#' @title is_matrix function from checkmate
#'
#' @description Re-export of \code{checkmate::test_matrix}.
#' See the original package documentation for full details.
#' Check if an argument is a matrix
#' 
#'
#' @param x [any]\cr
#' Object to check.
#' @param mode [\code{character(1)}]\cr
#' Storage mode of the array. Arrays can hold vectors, i.e. \dQuote{logical},
#' \dQuote{integer}, \dQuote{integerish}, \dQuote{double}, \dQuote{numeric}, \dQuote{complex},
#' \dQuote{character} and \dQuote{list}. You can also specify \dQuote{atomic}
#' here to explicitly prohibit lists. Default is \code{NULL} (no check).
#' If all values of \code{x} are missing, this check is skipped.
#' @param any.missing [\code{logical(1)}]\cr
#' Are missing values allowed? Default is \code{TRUE}.
#' @param all.missing [\code{logical(1)}]\cr
#' Are matrices with only missing values allowed? Default is \code{TRUE}.
#' @param min.rows [\code{integer(1)}]\cr
#' Minimum number of rows.
#' @param max.rows [\code{integer(1)}]\cr
#' Maximum number of rows.
#' @param min.cols [\code{integer(1)}]\cr
#' Minimum number of columns.
#' @param max.cols [\code{integer(1)}]\cr
#' Maximum number of columns.
#' @param nrows [\code{integer(1)}]\cr
#' Exact number of rows.
#' @param ncols [\code{integer(1)}]\cr
#' Exact number of columns.
#' @param row.names [\code{character(1)}]\cr
#' Check for row names. Default is \dQuote{NULL} (no check).
#' See \code{\link{checkNamed}} for possible values.
#' Note that you can use \code{\link{checkSubset}} to check for a specific set of names.
#' @param col.names [\code{character(1)}]\cr
#' Check for column names. Default is \dQuote{NULL} (no check).
#' See \code{\link{checkNamed}} for possible values.
#' Note that you can use \code{\link{checkSubset}} to test for a specific set of names.
#' @param null.ok [\code{logical(1)}]\cr
#' If set to \code{TRUE}, \code{x} may also be \code{NULL}.
#' In this case only a type check of \code{x} is performed, all additional checks are disabled.
#' @name is_matrix
#' @importFrom checkmate test_matrix
#' @export
is_matrix <- checkmate::test_matrix


#' @title is_numeric function from checkmate
#'
#' @description Re-export of \code{checkmate::test_numeric}.
#' See the original package documentation for full details.
#' Vectors of storage type list("integer") and list("double") count as list("numeric"), c.f. list(list("is.numeric")).
#' To explicitly check for real integer or double vectors, see list(list("checkInteger")), list(list("checkIntegerish")) or
#' list(list("checkDouble")).
#' 
#'
#' @param x [any]\cr
#' Object to check.
#' @param lower [\code{numeric(1)}]\cr
#' Lower value all elements of \code{x} must be greater than or equal to.
#' @param upper [\code{numeric(1)}]\cr
#' Upper value all elements of \code{x} must be lower than or equal to.
#' @param finite [\code{logical(1)}]\cr
#' Check for only finite values? Default is \code{FALSE}.
#' @param any.missing [\code{logical(1)}]\cr
#' Are vectors with missing values allowed? Default is \code{TRUE}.
#' @param all.missing [\code{logical(1)}]\cr
#' Are vectors with no non-missing values allowed? Default is \code{TRUE}.
#' Note that empty vectors do not have non-missing values.
#' @param len [\code{integer(1)}]\cr
#' Exact expected length of \code{x}.
#' @param min.len [\code{integer(1)}]\cr
#' Minimal length of \code{x}.
#' @param max.len [\code{integer(1)}]\cr
#' Maximal length of \code{x}.
#' @param unique [\code{logical(1)}]\cr
#' Must all values be unique? Default is \code{FALSE}.
#' @param sorted [\code{logical(1)}]\cr
#' Elements must be sorted in ascending order. Missing values are ignored.
#' @param names [\code{character(1)}]\cr
#' Check for names. See \code{\link{checkNamed}} for possible values.
#' Default is \dQuote{any} which performs no check at all.
#' Note that you can use \code{\link{checkSubset}} to check for a specific set of names.
#' @param typed.missing [\code{logical(1)}]\cr
#' If set to \code{FALSE} (default), all types of missing values (\code{NA}, \code{NA_integer_},
#' \code{NA_real_}, \code{NA_character_} or \code{NA_character_}) as well as empty vectors are allowed
#' while type-checking atomic input.
#' Set to \code{TRUE} to enable strict type checking.
#' @param null.ok [\code{logical(1)}]\cr
#' If set to \code{TRUE}, \code{x} may also be \code{NULL}.
#' In this case only a type check of \code{x} is performed, all additional checks are disabled.
#' @name is_numeric
#' @importFrom checkmate test_numeric
#' @export
is_numeric <- checkmate::test_numeric


#' @title is_string function from checkmate
#'
#' @description Re-export of \code{checkmate::test_string}.
#' See the original package documentation for full details.
#' A string is defined as a scalar character vector.
#' To check for vectors of arbitrary length, see list(list("checkCharacter")).
#' 
#'
#' @param x [any]\cr
#' Object to check.
#' @param na.ok [\code{logical(1)}]\cr
#' Are missing values allowed? Default is \code{FALSE}.
#' @param n.chars [\code{integer(1)}]\cr
#' Exact number of characters for each element of \code{x}.
#' @param min.chars [\code{integer(1)}]\cr
#' Minimum number of characters for each element of \code{x}.
#' @param max.chars [\code{integer(1)}]\cr
#' Maximum number of characters for each element of \code{x}.
#' @param pattern [\code{character(1L)}]\cr
#' Regular expression as used in \code{\link{grepl}}.
#' All non-missing elements of \code{x} must comply to this pattern.
#' @param fixed [\code{character(1)}]\cr
#' Substring to detect in \code{x}. Will be used as \code{pattern} in \code{\link{grepl}}
#' with option \code{fixed} set to \code{TRUE}.
#' All non-missing elements of \code{x} must contain this substring.
#' @param ignore.case [\code{logical(1)}]\cr
#' See \code{\link{grepl}}. Default is \code{FALSE}.
#' @param null.ok [\code{logical(1)}]\cr
#' If set to \code{TRUE}, \code{x} may also be \code{NULL}.
#' In this case only a type check of \code{x} is performed, all additional checks are disabled.
#' @name is_string
#' @importFrom checkmate test_string
#' @export
is_string <- checkmate::test_string


#' @title is_factor function from checkmate
#'
#' @description Re-export of \code{checkmate::test_factor}.
#' See the original package documentation for full details.
#' Check if an argument is a factor
#' 
#'
#' @param x [any]\cr
#' Object to check.
#' @param levels [\code{character}]\cr
#' Vector of allowed factor levels.
#' @param ordered [\code{logical(1)}]\cr
#' Check for an ordered factor? If \code{FALSE} or \code{TRUE}, checks explicitly
#' for an unordered or ordered factor, respectively.
#' Default is \code{NA} which does not perform any additional check.
#' @param empty.levels.ok [\code{logical(1)}]\cr
#' Are empty levels allowed?
#' Default is \code{TRUE}.
#' @param any.missing [\code{logical(1)}]\cr
#' Are vectors with missing values allowed? Default is \code{TRUE}.
#' @param all.missing [\code{logical(1)}]\cr
#' Are vectors with no non-missing values allowed? Default is \code{TRUE}.
#' Note that empty vectors do not have non-missing values.
#' @param len [\code{integer(1)}]\cr
#' Exact expected length of \code{x}.
#' @param min.len [\code{integer(1)}]\cr
#' Minimal length of \code{x}.
#' @param max.len [\code{integer(1)}]\cr
#' Maximal length of \code{x}.
#' @param n.levels [\code{integer(1)}]\cr
#' Exact number of factor levels.
#' Default is \code{NULL} (no check).
#' @param min.levels [\code{integer(1)}]\cr
#' Minimum number of factor levels.
#' Default is \code{NULL} (no check).
#' @param max.levels [\code{integer(1)}]\cr
#' Maximum number of factor levels.
#' Default is \code{NULL} (no check).
#' @param unique [\code{logical(1)}]\cr
#' Must all values be unique? Default is \code{FALSE}.
#' @param names [\code{character(1)}]\cr
#' Check for names. See \code{\link{checkNamed}} for possible values.
#' Default is \dQuote{any} which performs no check at all.
#' Note that you can use \code{\link{checkSubset}} to check for a specific set of names.
#' @param null.ok [\code{logical(1)}]\cr
#' If set to \code{TRUE}, \code{x} may also be \code{NULL}.
#' In this case only a type check of \code{x} is performed, all additional checks are disabled.
#' @name is_factor
#' @importFrom checkmate test_factor
#' @export
is_factor <- checkmate::test_factor


#' @title is_scalar_na function from checkmate
#'
#' @description Re-export of \code{checkmate::test_scalar_na}.
#' See the original package documentation for full details.
#' Check if an argument is a single missing value
#' 
#'
#' @param x [any]\cr
#' Object to check.
#' @param null.ok [\code{logical(1)}]\cr
#' If set to \code{TRUE}, \code{x} may also be \code{NULL}.
#' In this case only a type check of \code{x} is performed, all additional checks are disabled.
#' @name is_scalar_na
#' @importFrom checkmate test_scalar_na
#' @export
is_scalar_na <- checkmate::test_scalar_na


#' @title is_syntactic_literal function from rlang
#'
#' @description Re-export of \code{rlang::is_syntactic_literal}.
#' See the original package documentation for full details.
#' In rlang, an list("expression") is the return type of list(list("parse_expr()")), the
#' set of objects that can be obtained from parsing R code. Under this
#' definition expressions include numbers, strings, list("NULL"), symbols,
#' and function calls. These objects can be classified as:
#' list("\n", list(), " Symbolic objects, i.e. symbols and function calls (for which\n", list("is_symbolic()"), " returns ", list("TRUE"), ")\n", list(), " Syntactic literals, i.e. scalar atomic objects and ", list("NULL"), "\n", "(testable with ", list("is_syntactic_literal()"), ")\n")
#' 
#' list("is_expression()") returns list("TRUE") if the input is either a symbolic
#' object or a syntactic literal. If a call, the elements of the call
#' must all be expressions as well. Unparsable calls are not
#' considered expressions in this narrow definition.
#' 
#' Note that in base R, there exists list(list("expression()")) vectors, a data
#' type similar to a list that supports special attributes created by
#' the parser called source references. This data type is not
#' supported in rlang.
#' 
#'
#' @param x An object to test.
#' @name is_syntactic_literal
#' @importFrom rlang is_syntactic_literal
#' @export
is_syntactic_literal <- rlang::is_syntactic_literal


#' @title is_lang function from rlang
#'
#' @description Re-export of \code{rlang::is_lang}.
#' See the original package documentation for full details.
#' list(list("html"), list(list(list("https://lifecycle.r-lib.org/articles/stages.html#deprecated"), list(list(list("lifecycle-deprecated.svg"), list("options: alt='[Deprecated]'"))))), list(list("[Deprecated]")))
#' These functions are deprecated, please use list(list("is_call()")) and its list("n")
#' argument instead.
#' 
#'
#' @param x An object to test. Formulas and quosures are treated
#' literally.
#' @param name An optional name that the call should match. It is
#' passed to \code{\link{sym()}} before matching. This argument is vectorised
#' and you can supply a vector of names to match. In this case,
#' \code{is_call()} returns \code{TRUE} if at least one name matches.
#' @param n An optional number of arguments that the call should
#' match.
#' @param ns The namespace of the call. If \code{NULL}, the namespace
#' doesn't participate in the pattern-matching. If an empty string
#' \code{""} and \code{x} is a namespaced call, \code{is_call()} returns
#' \code{FALSE}. If any other string, \code{is_call()} checks that \code{x} is
#' namespaced within \code{ns}.
#' 
#' Can be a character vector of namespaces, in which case the call
#' has to match at least one of them, otherwise \code{is_call()} returns
#' \code{FALSE}.
#' @name is_lang
#' @importFrom rlang is_lang
#' @export
is_lang <- rlang::is_lang


#' @title is_call_simple function from rlang
#'
#' @description Re-export of \code{rlang::is_call_simple}.
#' See the original package documentation for full details.
#' list("call_name()") and list("call_ns()") extract the function name or
#' namespace of list("simple") calls as a string. They return list("NULL") for
#' complex calls.
#' list("\n", list(), " Simple calls: ", list("foo()"), ", ", list("bar::foo()"), ".\n", list(), " Complex calls: ", list("foo()()"), ", ", list("bar::foo"), ", ", list("foo$bar()"), ", ", list("(function() NULL)()"), ".\n")
#' 
#' The list("is_call_simple()") predicate helps you determine whether a call
#' is simple. There are two invariants you can count on:
#' list("\n", list(), " If ", list("is_call_simple(x)"), " returns ", list("TRUE"), ", ", list("call_name(x)"), " returns a\n", "string. Otherwise it returns ", list("NULL"), ".\n", list(), " If ", list("is_call_simple(x, ns = TRUE)"), " returns ", list("TRUE"), ", ", list("call_ns()"), "\n", "returns a string. Otherwise it returns ", list("NULL"), ".\n")
#' 
#'
#' @param x An object to test.
#' @param ns Whether call is namespaced. If \code{NULL}, \code{is_call_simple()}
#' is insensitive to namespaces. If \code{TRUE}, \code{is_call_simple()}
#' detects namespaced calls. If \code{FALSE}, it detects unnamespaced
#' calls.
#' @name is_call_simple
#' @importFrom rlang is_call_simple
#' @export
is_call_simple <- rlang::is_call_simple


#' @title is_scalar_integer function from rlang
#'
#' @description Re-export of \code{rlang::is_scalar_integer}.
#' See the original package documentation for full details.
#' These predicates check for a given type and whether the vector is
#' "scalar", that is, of length 1.
#' 
#' In addition to the length check, list("is_string()") and list("is_bool()")
#' return list("FALSE") if their input is missing. This is useful for
#' type-checking arguments, when your function expects a single string
#' or a single list("TRUE") or list("FALSE").
#' 
#'
#' @param x object to be tested.
#' @name is_scalar_integer
#' @importFrom rlang is_scalar_integer
#' @export
is_scalar_integer <- rlang::is_scalar_integer


#' @title is_zap function from rlang
#'
#' @description Re-export of \code{rlang::is_zap}.
#' See the original package documentation for full details.
#' list("zap()") creates a sentinel object that indicates that an object
#' should be removed. For instance, named zaps instruct list(list("env_bind()"))
#' and list(list("call_modify()")) to remove those objects from the environment or
#' the call.
#' 
#' The advantage of zap objects is that they unambiguously signal the
#' intent of removing an object. Sentinels like list("NULL") or
#' list(list("missing_arg()")) are ambiguous because they represent valid R
#' objects.
#' 
#'
#' @param x An object to test.
#' @name is_zap
#' @importFrom rlang is_zap
#' @export
is_zap <- rlang::is_zap


#' @title is_lgl_na function from rlang
#'
#' @description Re-export of \code{rlang::is_lgl_na}.
#' See the original package documentation for full details.
#' list(list("html"), list(list(list("https://lifecycle.r-lib.org/articles/stages.html#questioning"), list(list(list("lifecycle-questioning.svg"), list("options: alt='[Questioning]'"))))), list(list("[Questioning]")))
#' 
#' list("are_na()") checks for missing values in a vector and is equivalent
#' to list(list("base::is.na()")). It is a vectorised predicate, meaning that its
#' output is always the same length as its input. On the other hand,
#' list("is_na()") is a scalar predicate and always returns a scalar
#' boolean, list("TRUE") or list("FALSE"). If its input is not scalar, it returns
#' list("FALSE"). Finally, there are typed versions that check for
#' particular list("missing types").
#' 
#'
#' @param x An object to test
#' @name is_lgl_na
#' @importFrom rlang is_lgl_na
#' @export
is_lgl_na <- rlang::is_lgl_na


#' @title is_bare_vector function from rlang
#'
#' @description Re-export of \code{rlang::is_bare_vector}.
#' See the original package documentation for full details.
#' These predicates check for a given type but only return list("TRUE") for
#' bare R objects. Bare objects have no class attributes. For example,
#' a data frame is a list, but not a bare list.
#' 
#'
#' @param x Object to be tested.
#' @param n Expected length of a vector.
#' @name is_bare_vector
#' @importFrom rlang is_bare_vector
#' @export
is_bare_vector <- rlang::is_bare_vector


#' @title is_missing function from rlang
#'
#' @description Re-export of \code{rlang::is_missing}.
#' See the original package documentation for full details.
#' These functions help using the missing argument as a regular R
#' object.
#' list("\n", list(), " ", list("missing_arg()"), " generates a missing argument.\n", list(), " ", list("is_missing()"), " is like ", list(list("base::missing()")), " but also supports\n", "testing for missing arguments contained in other objects like\n", "lists. It is also more consistent with default arguments which\n", "are never treated as missing (see section below).\n", list(), " ", list("maybe_missing()"), " is useful to pass down an input that might be\n", "missing to another function, potentially substituting by a\n", 
#'     "default value. It avoids triggering an \"argument is missing\" error.\n")
#' 
#'
#' @param x An object that might be the missing argument.
#' @name is_missing
#' @importFrom rlang is_missing
#' @export
is_missing <- rlang::is_missing


#' @title is_scalar_double function from rlang
#'
#' @description Re-export of \code{rlang::is_scalar_double}.
#' See the original package documentation for full details.
#' These predicates check for a given type and whether the vector is
#' "scalar", that is, of length 1.
#' 
#' In addition to the length check, list("is_string()") and list("is_bool()")
#' return list("FALSE") if their input is missing. This is useful for
#' type-checking arguments, when your function expects a single string
#' or a single list("TRUE") or list("FALSE").
#' 
#'
#' @param x object to be tested.
#' @name is_scalar_double
#' @importFrom rlang is_scalar_double
#' @export
is_scalar_double <- rlang::is_scalar_double


#' @title is_call function from rlang
#'
#' @description Re-export of \code{rlang::is_call}.
#' See the original package documentation for full details.
#' This function tests if list("x") is a list("call"). This is a
#' pattern-matching predicate that returns list("FALSE") if list("name") and list("n")
#' are supplied and the call does not match these properties.
#' 
#'
#' @param x An object to test. Formulas and quosures are treated
#' literally.
#' @param name An optional name that the call should match. It is
#' passed to \code{\link{sym()}} before matching. This argument is vectorised
#' and you can supply a vector of names to match. In this case,
#' \code{is_call()} returns \code{TRUE} if at least one name matches.
#' @param n An optional number of arguments that the call should
#' match.
#' @param ns The namespace of the call. If \code{NULL}, the namespace
#' doesn't participate in the pattern-matching. If an empty string
#' \code{""} and \code{x} is a namespaced call, \code{is_call()} returns
#' \code{FALSE}. If any other string, \code{is_call()} checks that \code{x} is
#' namespaced within \code{ns}.
#' 
#' Can be a character vector of namespaces, in which case the call
#' has to match at least one of them, otherwise \code{is_call()} returns
#' \code{FALSE}.
#' @name is_call
#' @importFrom rlang is_call
#' @export
is_call <- rlang::is_call


#' @title is_spliced_bare function from rlang
#'
#' @description Re-export of \code{rlang::is_spliced_bare}.
#' See the original package documentation for full details.
#' The splicing operator list("!!!") operates both in values contexts like
#' list(list("list2()")) and list(list("dots_list()")), and in metaprogramming contexts like
#' list(list("expr()")), list(list("enquos()")), or list(list("inject()")). While the end result looks the
#' same, the implementation is different and much more efficient in
#' the value cases. This difference in implementation may cause
#' performance issues for instance when going from:
#' 
#' list(list("html"), list(list("<div class=\"sourceCode r\">")))list("xs <- list(2, 3)\n", "list2(1, !!!xs, 4)\n")list(list("html"), list(list("</div>")))
#' 
#' to:
#' 
#' list(list("html"), list(list("<div class=\"sourceCode r\">")))list("inject(list2(1, !!!xs, 4))\n")list(list("html"), list(list("</div>")))
#' 
#' In the former case, the performant value-splicing is used. In the
#' latter case, the slow metaprogramming splicing is used.
#' 
#' A common practical case where this may occur is when code is
#' wrapped inside a tidyeval context like list("dplyr::mutate()"). In this
#' case, the metaprogramming operator list("!!!") will take over the
#' value-splicing operator, causing an unexpected slowdown.
#' 
#' To avoid this in performance-critical code, use list("splice()") instead
#' of list("!!!"):
#' 
#' list(list("html"), list(list("<div class=\"sourceCode r\">")))list("# These both use the fast splicing:\n", "list2(1, splice(xs), 4)\n", "inject(list2(1, splice(xs), 4))\n")list(list("html"), list(list("</div>")))
#' 
#'
#' @param x A list or vector to splice non-eagerly.
#' @name is_spliced_bare
#' @importFrom rlang is_spliced_bare
#' @export
is_spliced_bare <- rlang::is_spliced_bare


#' @title is_spliced function from rlang
#'
#' @description Re-export of \code{rlang::is_spliced}.
#' See the original package documentation for full details.
#' The splicing operator list("!!!") operates both in values contexts like
#' list(list("list2()")) and list(list("dots_list()")), and in metaprogramming contexts like
#' list(list("expr()")), list(list("enquos()")), or list(list("inject()")). While the end result looks the
#' same, the implementation is different and much more efficient in
#' the value cases. This difference in implementation may cause
#' performance issues for instance when going from:
#' 
#' list(list("html"), list(list("<div class=\"sourceCode r\">")))list("xs <- list(2, 3)\n", "list2(1, !!!xs, 4)\n")list(list("html"), list(list("</div>")))
#' 
#' to:
#' 
#' list(list("html"), list(list("<div class=\"sourceCode r\">")))list("inject(list2(1, !!!xs, 4))\n")list(list("html"), list(list("</div>")))
#' 
#' In the former case, the performant value-splicing is used. In the
#' latter case, the slow metaprogramming splicing is used.
#' 
#' A common practical case where this may occur is when code is
#' wrapped inside a tidyeval context like list("dplyr::mutate()"). In this
#' case, the metaprogramming operator list("!!!") will take over the
#' value-splicing operator, causing an unexpected slowdown.
#' 
#' To avoid this in performance-critical code, use list("splice()") instead
#' of list("!!!"):
#' 
#' list(list("html"), list(list("<div class=\"sourceCode r\">")))list("# These both use the fast splicing:\n", "list2(1, splice(xs), 4)\n", "inject(list2(1, splice(xs), 4))\n")list(list("html"), list(list("</div>")))
#' 
#'
#' @param x A list or vector to splice non-eagerly.
#' @name is_spliced
#' @importFrom rlang is_spliced
#' @export
is_spliced <- rlang::is_spliced


#' @title is_closure function from rlang
#'
#' @description Re-export of \code{rlang::is_closure}.
#' See the original package documentation for full details.
#' The R language defines two different types of functions: primitive
#' functions, which are low-level, and closures, which are the regular
#' kind of functions.
#' 
#'
#' @param x Object to be tested.
#' @name is_closure
#' @importFrom rlang is_closure
#' @export
is_closure <- rlang::is_closure


#' @title is_empty function from rlang
#'
#' @description Re-export of \code{rlang::is_empty}.
#' See the original package documentation for full details.
#' Is object an empty vector or NULL?
#' 
#'
#' @param x object to test
#' @name is_empty
#' @importFrom rlang is_empty
#' @export
is_empty <- rlang::is_empty


#' @title is_bare_double function from rlang
#'
#' @description Re-export of \code{rlang::is_bare_double}.
#' See the original package documentation for full details.
#' These predicates check for a given type but only return list("TRUE") for
#' bare R objects. Bare objects have no class attributes. For example,
#' a data frame is a list, but not a bare list.
#' 
#'
#' @param x Object to be tested.
#' @param n Expected length of a vector.
#' @name is_bare_double
#' @importFrom rlang is_bare_double
#' @export
is_bare_double <- rlang::is_bare_double


#' @title is_primitive_lazy function from rlang
#'
#' @description Re-export of \code{rlang::is_primitive_lazy}.
#' See the original package documentation for full details.
#' The R language defines two different types of functions: primitive
#' functions, which are low-level, and closures, which are the regular
#' kind of functions.
#' 
#'
#' @param x Object to be tested.
#' @name is_primitive_lazy
#' @importFrom rlang is_primitive_lazy
#' @export
is_primitive_lazy <- rlang::is_primitive_lazy


#' @title is_scalar_character function from rlang
#'
#' @description Re-export of \code{rlang::is_scalar_character}.
#' See the original package documentation for full details.
#' These predicates check for a given type and whether the vector is
#' "scalar", that is, of length 1.
#' 
#' In addition to the length check, list("is_string()") and list("is_bool()")
#' return list("FALSE") if their input is missing. This is useful for
#' type-checking arguments, when your function expects a single string
#' or a single list("TRUE") or list("FALSE").
#' 
#'
#' @param x object to be tested.
#' @name is_scalar_character
#' @importFrom rlang is_scalar_character
#' @export
is_scalar_character <- rlang::is_scalar_character


#' @title is_named2 function from rlang
#'
#' @description Re-export of \code{rlang::is_named2}.
#' See the original package documentation for full details.
#' list("\n", list(), " ", list("is_named()"), " is a scalar predicate that checks that ", list("x"), " has a\n", list("names"), " attribute and that none of the names are missing or empty\n", "(", list("NA"), " or ", list("\"\""), ").\n", list(), " ", list("is_named2()"), " is like ", list("is_named()"), " but always returns ", list("TRUE"), " for\n", "empty vectors, even those that don't have a ", list("names"), " attribute.\n", "In other words, it tests for the property that each element of a\n", 
#'     "vector is named. ", list("is_named2()"), " composes well with ", list(list("names2()")), "\n", "whereas ", list("is_named()"), " composes with ", list("names()"), ".\n", list(), " ", list("have_name()"), " is a vectorised variant.\n")
#' 
#'
#' @param x A vector to test.
#' @name is_named2
#' @importFrom rlang is_named2
#' @export
is_named2 <- rlang::is_named2


#' @title is_scoped function from rlang
#'
#' @description Re-export of \code{rlang::is_scoped}.
#' See the original package documentation for full details.
#' list(list("html"), list(list(list("https://lifecycle.r-lib.org/articles/stages.html#deprecated"), list(list(list("lifecycle-deprecated.svg"), list("options: alt='[Deprecated]'"))))), list(list("[Deprecated]")))
#' 
#' These functions are deprecated as of rlang 0.3.0. Please use
#' list(list("is_attached()")) instead.
#' 
#'
#' @param nm The name of an environment attached to the search
#' path. Call \code{\link{base::search()}} to see what is currently on the path.
#' @name is_scoped
#' @importFrom rlang is_scoped
#' @export
is_scoped <- rlang::is_scoped


#' @title is_bare_string function from rlang
#'
#' @description Re-export of \code{rlang::is_bare_string}.
#' See the original package documentation for full details.
#' These predicates check for a given type but only return list("TRUE") for
#' bare R objects. Bare objects have no class attributes. For example,
#' a data frame is a list, but not a bare list.
#' 
#'
#' @param x Object to be tested.
#' @param n Expected length of a vector.
#' @name is_bare_string
#' @importFrom rlang is_bare_string
#' @export
is_bare_string <- rlang::is_bare_string


#' @title is_namespace function from rlang
#'
#' @description Re-export of \code{rlang::is_namespace}.
#' See the original package documentation for full details.
#' Is an object a namespace environment?
#' 
#'
#' @param x An object to test.
#' @name is_namespace
#' @importFrom rlang is_namespace
#' @export
is_namespace <- rlang::is_namespace


#' @title is_bare_character function from rlang
#'
#' @description Re-export of \code{rlang::is_bare_character}.
#' See the original package documentation for full details.
#' These predicates check for a given type but only return list("TRUE") for
#' bare R objects. Bare objects have no class attributes. For example,
#' a data frame is a list, but not a bare list.
#' 
#'
#' @param x Object to be tested.
#' @param n Expected length of a vector.
#' @name is_bare_character
#' @importFrom rlang is_bare_character
#' @export
is_bare_character <- rlang::is_bare_character


#' @title is_scalar_logical function from rlang
#'
#' @description Re-export of \code{rlang::is_scalar_logical}.
#' See the original package documentation for full details.
#' These predicates check for a given type and whether the vector is
#' "scalar", that is, of length 1.
#' 
#' In addition to the length check, list("is_string()") and list("is_bool()")
#' return list("FALSE") if their input is missing. This is useful for
#' type-checking arguments, when your function expects a single string
#' or a single list("TRUE") or list("FALSE").
#' 
#'
#' @param x object to be tested.
#' @name is_scalar_logical
#' @importFrom rlang is_scalar_logical
#' @export
is_scalar_logical <- rlang::is_scalar_logical


#' @title is_attached function from rlang
#'
#' @description Re-export of \code{rlang::is_attached}.
#' See the original package documentation for full details.
#' The search path is a chain of environments containing exported
#' functions of attached packages.
#' 
#' The API includes:
#' list("\n", list(), " ", list(list("base::search()")), " to get the names of environments attached to the\n", "search path.\n", list(), " ", list("search_envs()"), " returns the environments on the search path as a\n", "list.\n", list(), " ", list("pkg_env_name()"), " takes a bare package name and prefixes it with\n", list("\"package:\""), ". Attached package environments have search names of\n", "the form ", list("package:name"), ".\n", list(), " ", list("pkg_env()"), " takes a bare package name and returns the scoped\n", 
#'     "environment of packages if they are attached to the search path,\n", "and throws an error otherwise. It is a shortcut for\n", list("search_env(pkg_env_name(\"pkgname\"))"), ".\n", list(), " ", list("global_env()"), " and ", list("base_env()"), " (simple aliases for ", list(list("globalenv()")), "\n", "and ", list(list("baseenv()")), "). These are respectively the first and last\n", "environments of the search path.\n", list(), " ", list("is_attached()"), " returns ", list("TRUE"), " when its argument (a search name\n", 
#'     "or a package environment) is attached to the search path.\n")
#' 
#'
#' @param x An environment or a search name.
#' @name is_attached
#' @importFrom rlang is_attached
#' @export
is_attached <- rlang::is_attached


#' @title is_scalar_atomic function from rlang
#'
#' @description Re-export of \code{rlang::is_scalar_atomic}.
#' See the original package documentation for full details.
#' These predicates check for a given type and whether the vector is
#' "scalar", that is, of length 1.
#' 
#' In addition to the length check, list("is_string()") and list("is_bool()")
#' return list("FALSE") if their input is missing. This is useful for
#' type-checking arguments, when your function expects a single string
#' or a single list("TRUE") or list("FALSE").
#' 
#'
#' @param x object to be tested.
#' @name is_scalar_atomic
#' @importFrom rlang is_scalar_atomic
#' @export
is_scalar_atomic <- rlang::is_scalar_atomic


#' @title is_bytes function from rlang
#'
#' @description Re-export of \code{rlang::is_bytes}.
#' See the original package documentation for full details.
#' These type predicates aim to make type testing in R more
#' consistent. They are wrappers around list(list("base::typeof()")), so operate
#' at a level beneath S3/S4 etc.
#' 
#'
#' @param x Object to be tested.
#' @param n Expected length of a vector.
#' @name is_bytes
#' @importFrom rlang is_bytes
#' @export
is_bytes <- rlang::is_bytes


#' @title is_done_box function from rlang
#'
#' @description Re-export of \code{rlang::is_done_box}.
#' See the original package documentation for full details.
#' A value boxed with list("done()") signals to its caller that it
#' should stop iterating. Use it to shortcircuit a loop.
#' 
#'
#' @param x For \code{done()}, a value to box. For \code{is_done_box()}, a
#' value to test.
#' @param empty Whether the box is empty. If \code{NULL}, \code{is_done_box()}
#' returns \code{TRUE} for all done boxes. If \code{TRUE}, it returns \code{TRUE}
#' only for empty boxes. Otherwise it returns \code{TRUE} only for
#' non-empty boxes.
#' @name is_done_box
#' @importFrom rlang is_done_box
#' @export
is_done_box <- rlang::is_done_box


#' @title is_bare_bytes function from rlang
#'
#' @description Re-export of \code{rlang::is_bare_bytes}.
#' See the original package documentation for full details.
#' These predicates check for a given type but only return list("TRUE") for
#' bare R objects. Bare objects have no class attributes. For example,
#' a data frame is a list, but not a bare list.
#' 
#'
#' @param x Object to be tested.
#' @param n Expected length of a vector.
#' @name is_bare_bytes
#' @importFrom rlang is_bare_bytes
#' @export
is_bare_bytes <- rlang::is_bare_bytes


#' @title is_scalar_list function from rlang
#'
#' @description Re-export of \code{rlang::is_scalar_list}.
#' See the original package documentation for full details.
#' These predicates check for a given type and whether the vector is
#' "scalar", that is, of length 1.
#' 
#' In addition to the length check, list("is_string()") and list("is_bool()")
#' return list("FALSE") if their input is missing. This is useful for
#' type-checking arguments, when your function expects a single string
#' or a single list("TRUE") or list("FALSE").
#' 
#'
#' @param x object to be tested.
#' @name is_scalar_list
#' @importFrom rlang is_scalar_list
#' @export
is_scalar_list <- rlang::is_scalar_list


#' @title is_pairlist function from rlang
#'
#' @description Re-export of \code{rlang::is_pairlist}.
#' See the original package documentation for full details.
#' list("\n", list(), " ", list("is_pairlist()"), " checks that ", list("x"), " has type ", list("pairlist"), ".\n", list(), " ", list("is_node()"), " checks that ", list("x"), " has type ", list("pairlist"), " or ", list("language"), ".\n", "It tests whether ", list("x"), " is a node that has a CAR and a CDR,\n", "including callable nodes (language objects).\n", list(), " ", list("is_node_list()"), " checks that ", list("x"), " has type ", list("pairlist"), " or ", list("NULL"), ".\n", list("NULL"), 
#'     " is the empty node list.\n")
#' 
#'
#' @param x Object to test.
#' @name is_pairlist
#' @importFrom rlang is_pairlist
#' @export
is_pairlist <- rlang::is_pairlist


#' @title is_installed function from rlang
#'
#' @description Re-export of \code{rlang::is_installed}.
#' See the original package documentation for full details.
#' These functions check that packages are installed with minimal side
#' effects. If installed, the packages will be loaded but not
#' attached.
#' list("\n", list(), " ", list("is_installed()"), " doesn't interact with the user. It simply\n", "returns ", list("TRUE"), " or ", list("FALSE"), " depending on whether the packages are\n", "installed.\n", list(), " In interactive sessions, ", list("check_installed()"), " asks the user\n", "whether to install missing packages. If the user accepts, the\n", "packages are installed with ", list("pak::pkg_install()"), " if available, or\n", list(list("utils::install.packages()")), " otherwise. If the session is non\n", 
#'     "interactive or if the user chooses not to install the packages,\n", "the current evaluation is aborted.\n")
#' 
#' You can disable the prompt by setting the
#' list("rlib_restart_package_not_found") global option to list("FALSE"). In that
#' case, missing packages always cause an error.
#' 
#'
#' @param pkg The package names. Can include version requirements,
#' e.g. \code{"pkg (>= 1.0.0)"}.
#' @param ... These dots must be empty.
#' @param version Minimum versions for \code{pkg}. If supplied, must be the
#' same length as \code{pkg}. \code{NA} elements stand for any versions.
#' @param compare A character vector of comparison operators to use
#' for \code{version}. If supplied, must be the same length as
#' \code{version}. If \code{NULL}, \code{>=} is used as default for all
#' elements. \code{NA} elements in \code{compare} are also set to \code{>=} by
#' default.
#' @name is_installed
#' @importFrom rlang is_installed
#' @export
is_installed <- rlang::is_installed


#' @title is_dbl_na function from rlang
#'
#' @description Re-export of \code{rlang::is_dbl_na}.
#' See the original package documentation for full details.
#' list(list("html"), list(list(list("https://lifecycle.r-lib.org/articles/stages.html#questioning"), list(list(list("lifecycle-questioning.svg"), list("options: alt='[Questioning]'"))))), list(list("[Questioning]")))
#' 
#' list("are_na()") checks for missing values in a vector and is equivalent
#' to list(list("base::is.na()")). It is a vectorised predicate, meaning that its
#' output is always the same length as its input. On the other hand,
#' list("is_na()") is a scalar predicate and always returns a scalar
#' boolean, list("TRUE") or list("FALSE"). If its input is not scalar, it returns
#' list("FALSE"). Finally, there are typed versions that check for
#' particular list("missing types").
#' 
#'
#' @param x An object to test
#' @name is_dbl_na
#' @importFrom rlang is_dbl_na
#' @export
is_dbl_na <- rlang::is_dbl_na


#' @title is_bare_integer function from rlang
#'
#' @description Re-export of \code{rlang::is_bare_integer}.
#' See the original package documentation for full details.
#' These predicates check for a given type but only return list("TRUE") for
#' bare R objects. Bare objects have no class attributes. For example,
#' a data frame is a list, but not a bare list.
#' 
#'
#' @param x Object to be tested.
#' @param n Expected length of a vector.
#' @name is_bare_integer
#' @importFrom rlang is_bare_integer
#' @export
is_bare_integer <- rlang::is_bare_integer


#' @title is_copyable function from rlang
#'
#' @description Re-export of \code{rlang::is_copyable}.
#' See the original package documentation for full details.
#' When an object is modified, R generally copies it (sometimes
#' lazily) to enforce list(list("https://en.wikipedia.org/wiki/Value_semantics"), list("value semantics")).
#' However, some internal types are uncopyable. If you try to copy
#' them, either with list("<-") or by argument passing, you actually create
#' references to the original object rather than actual
#' copies. Modifying these references can thus have far reaching side
#' effects.
#' 
#'
#' @param x An object to test.
#' @name is_copyable
#' @importFrom rlang is_copyable
#' @export
is_copyable <- rlang::is_copyable


#' @title is_primitive_eager function from rlang
#'
#' @description Re-export of \code{rlang::is_primitive_eager}.
#' See the original package documentation for full details.
#' The R language defines two different types of functions: primitive
#' functions, which are low-level, and closures, which are the regular
#' kind of functions.
#' 
#'
#' @param x Object to be tested.
#' @name is_primitive_eager
#' @importFrom rlang is_primitive_eager
#' @export
is_primitive_eager <- rlang::is_primitive_eager


#' @title is_dictionaryish function from rlang
#'
#' @description Re-export of \code{rlang::is_dictionaryish}.
#' See the original package documentation for full details.
#' Like list(list("is_named()")) but also checks that names are unique.
#' 
#'
#' @param x A vector.
#' @name is_dictionaryish
#' @importFrom rlang is_dictionaryish
#' @export
is_dictionaryish <- rlang::is_dictionaryish


#' @title is_quosure function from rlang
#'
#' @description Re-export of \code{rlang::is_quosure}.
#' See the original package documentation for full details.
#' list("\n", list(), " ", list("new_quosure()"), " wraps any R object (including expressions,\n", "formulas, or other quosures) into a ", list("quosure"), ".\n", list(), " ", list("as_quosure()"), " is similar but it does not rewrap formulas and\n", "quosures.\n")
#' 
#'
#' @param x An object to test.
#' @name is_quosure
#' @importFrom rlang is_quosure
#' @export
is_quosure <- rlang::is_quosure


#' @title is_scalar_complex function from rlang
#'
#' @description Re-export of \code{rlang::is_scalar_complex}.
#' See the original package documentation for full details.
#' These predicates check for a given type and whether the vector is
#' "scalar", that is, of length 1.
#' 
#' In addition to the length check, list("is_string()") and list("is_bool()")
#' return list("FALSE") if their input is missing. This is useful for
#' type-checking arguments, when your function expects a single string
#' or a single list("TRUE") or list("FALSE").
#' 
#'
#' @param x object to be tested.
#' @name is_scalar_complex
#' @importFrom rlang is_scalar_complex
#' @export
is_scalar_complex <- rlang::is_scalar_complex


#' @title is_box function from rlang
#'
#' @description Re-export of \code{rlang::is_box}.
#' See the original package documentation for full details.
#' list("new_box()") is similar to list(list("base::I()")) but it protects a value by
#' wrapping it in a scalar list rather than by adding an attribute.
#' list("unbox()") retrieves the boxed value. list("is_box()") tests whether an
#' object is boxed with optional class. list("as_box()") ensures that a
#' value is wrapped in a box. list("as_box_if()") does the same but only if
#' the value matches a predicate.
#' 
#'
#' @param class For \code{new_box()}, an additional class for the
#' boxed value (in addition to \code{rlang_box}). For \code{is_box()}, a class
#' or vector of classes passed to \code{\link{inherits_all()}}.
#' @name is_box
#' @importFrom rlang is_box
#' @export
is_box <- rlang::is_box


#' @title is_int_na function from rlang
#'
#' @description Re-export of \code{rlang::is_int_na}.
#' See the original package documentation for full details.
#' list(list("html"), list(list(list("https://lifecycle.r-lib.org/articles/stages.html#questioning"), list(list(list("lifecycle-questioning.svg"), list("options: alt='[Questioning]'"))))), list(list("[Questioning]")))
#' 
#' list("are_na()") checks for missing values in a vector and is equivalent
#' to list(list("base::is.na()")). It is a vectorised predicate, meaning that its
#' output is always the same length as its input. On the other hand,
#' list("is_na()") is a scalar predicate and always returns a scalar
#' boolean, list("TRUE") or list("FALSE"). If its input is not scalar, it returns
#' list("FALSE"). Finally, there are typed versions that check for
#' particular list("missing types").
#' 
#'
#' @param x An object to test
#' @name is_int_na
#' @importFrom rlang is_int_na
#' @export
is_int_na <- rlang::is_int_na


#' @title is_reference function from rlang
#'
#' @description Re-export of \code{rlang::is_reference}.
#' See the original package documentation for full details.
#' There are typically two situations where two symbols may refer to
#' the same object.
#' list("\n", list(), " R objects usually have copy-on-write semantics. This is an\n", "optimisation that ensures that objects are only copied if\n", "needed. When you copy a vector, no memory is actually copied\n", "until you modify either the original object or the copy is\n", "modified.\n", "\n", "Note that the copy-on-write optimisation is an implementation\n", "detail that is not guaranteed by the specification of the R\n", "language.\n", list(), " Assigning an ", list("uncopyable"), " object (like an\n", 
#'     "environment) creates a reference. These objects are never copied\n", "even if you modify one of the references.\n")
#' 
#'
#' @name is_reference
#' @importFrom rlang is_reference
#' @export
is_reference <- rlang::is_reference


#' @title is_bare_atomic function from rlang
#'
#' @description Re-export of \code{rlang::is_bare_atomic}.
#' See the original package documentation for full details.
#' These predicates check for a given type but only return list("TRUE") for
#' bare R objects. Bare objects have no class attributes. For example,
#' a data frame is a list, but not a bare list.
#' 
#'
#' @param x Object to be tested.
#' @param n Expected length of a vector.
#' @name is_bare_atomic
#' @importFrom rlang is_bare_atomic
#' @export
is_bare_atomic <- rlang::is_bare_atomic


#' @title is_bare_integerish function from rlang
#'
#' @description Re-export of \code{rlang::is_bare_integerish}.
#' See the original package documentation for full details.
#' These predicates check whether R considers a number vector to be
#' integer-like, according to its own tolerance check (which is in
#' fact delegated to the C library). This function is not adapted to
#' data analysis, see the help for list(list("base::is.integer()")) for examples
#' of how to check for whole numbers.
#' 
#' Things to consider when checking for integer-like doubles:
#' list("\n", list(), " This check can be expensive because the whole double vector has\n", "to be traversed and checked.\n", list(), " Large double values may be integerish but may still not be\n", "coercible to integer. This is because integers in R only support\n", "values up to ", list("2^31 - 1"), " while numbers stored as double can be\n", "much larger.\n")
#' 
#'
#' @param x Object to be tested.
#' @param n Expected length of a vector.
#' @param finite Whether all values of the vector are finite. The
#' non-finite values are \code{NA}, \code{Inf}, \code{-Inf} and \code{NaN}. Setting this
#' to something other than \code{NULL} can be expensive because the whole
#' vector needs to be traversed and checked.
#' @name is_bare_integerish
#' @importFrom rlang is_bare_integerish
#' @export
is_bare_integerish <- rlang::is_bare_integerish


#' @title is_chr_na function from rlang
#'
#' @description Re-export of \code{rlang::is_chr_na}.
#' See the original package documentation for full details.
#' list(list("html"), list(list(list("https://lifecycle.r-lib.org/articles/stages.html#questioning"), list(list(list("lifecycle-questioning.svg"), list("options: alt='[Questioning]'"))))), list(list("[Questioning]")))
#' 
#' list("are_na()") checks for missing values in a vector and is equivalent
#' to list(list("base::is.na()")). It is a vectorised predicate, meaning that its
#' output is always the same length as its input. On the other hand,
#' list("is_na()") is a scalar predicate and always returns a scalar
#' boolean, list("TRUE") or list("FALSE"). If its input is not scalar, it returns
#' list("FALSE"). Finally, there are typed versions that check for
#' particular list("missing types").
#' 
#'
#' @param x An object to test
#' @name is_chr_na
#' @importFrom rlang is_chr_na
#' @export
is_chr_na <- rlang::is_chr_na


#' @title is_na function from rlang
#'
#' @description Re-export of \code{rlang::is_na}.
#' See the original package documentation for full details.
#' list(list("html"), list(list(list("https://lifecycle.r-lib.org/articles/stages.html#questioning"), list(list(list("lifecycle-questioning.svg"), list("options: alt='[Questioning]'"))))), list(list("[Questioning]")))
#' 
#' list("are_na()") checks for missing values in a vector and is equivalent
#' to list(list("base::is.na()")). It is a vectorised predicate, meaning that its
#' output is always the same length as its input. On the other hand,
#' list("is_na()") is a scalar predicate and always returns a scalar
#' boolean, list("TRUE") or list("FALSE"). If its input is not scalar, it returns
#' list("FALSE"). Finally, there are typed versions that check for
#' particular list("missing types").
#' 
#'
#' @param x An object to test
#' @name is_na
#' @importFrom rlang is_na
#' @export
is_na <- rlang::is_na


#' @title is_condition function from rlang
#'
#' @description Re-export of \code{rlang::is_condition}.
#' See the original package documentation for full details.
#' Is object a condition?
#' 
#'
#' @param x An object to test.
#' @name is_condition
#' @importFrom rlang is_condition
#' @export
is_condition <- rlang::is_condition


#' @title is_interactive function from rlang
#'
#' @description Re-export of \code{rlang::is_interactive}.
#' See the original package documentation for full details.
#' Like list(list("base::interactive()")), list("is_interactive()") returns list("TRUE") when
#' the function runs interactively and list("FALSE") when it runs in batch
#' mode. It also checks, in this order:
#' list("\n", list(), " The ", list("rlang_interactive"), " global option. If set to a single ", list("TRUE"), "\n", "or ", list("FALSE"), ", ", list("is_interactive()"), " returns that value immediately. This\n", "escape hatch is useful in unit tests or to manually turn on\n", "interactive features in RMarkdown outputs.\n", list(), " Whether knitr or testthat is in progress, in which case\n", list("is_interactive()"), " returns ", list("FALSE"), ".\n")
#' 
#' list("with_interactive()") and list("local_interactive()") set the global
#' option conveniently.
#' 
#'
#' @name is_interactive
#' @importFrom rlang is_interactive
#' @export
is_interactive <- rlang::is_interactive


#' @title is_quosures function from rlang
#'
#' @description Re-export of \code{rlang::is_quosures}.
#' See the original package documentation for full details.
#' This small S3 class provides methods for list("[") and list("c()") and ensures
#' the following invariants:
#' list("\n", list(), " The list only contains quosures.\n", list(), " It is always named, possibly with a vector of empty strings.\n")
#' 
#' list("new_quosures()") takes a list of quosures and adds the list("quosures")
#' class and a vector of empty names if needed. list("as_quosures()") calls
#' list(list("as_quosure()")) on all elements before creating the list("quosures")
#' object.
#' 
#'
#' @param x A list of quosures or objects to coerce to quosures.
#' @name is_quosures
#' @importFrom rlang is_quosures
#' @export
is_quosures <- rlang::is_quosures


#' @title is_message function from rlang
#'
#' @description Re-export of \code{rlang::is_message}.
#' See the original package documentation for full details.
#' Is object a condition?
#' 
#'
#' @param x An object to test.
#' @name is_message
#' @importFrom rlang is_message
#' @export
is_message <- rlang::is_message


#' @title is_bare_logical function from rlang
#'
#' @description Re-export of \code{rlang::is_bare_logical}.
#' See the original package documentation for full details.
#' These predicates check for a given type but only return list("TRUE") for
#' bare R objects. Bare objects have no class attributes. For example,
#' a data frame is a list, but not a bare list.
#' 
#'
#' @param x Object to be tested.
#' @param n Expected length of a vector.
#' @name is_bare_logical
#' @importFrom rlang is_bare_logical
#' @export
is_bare_logical <- rlang::is_bare_logical


#' @title is_bare_formula function from rlang
#'
#' @description Re-export of \code{rlang::is_bare_formula}.
#' See the original package documentation for full details.
#' list("is_formula()") tests whether list("x") is a call to list("~"). list("is_bare_formula()")
#' tests in addition that list("x") does not inherit from anything else than
#' list("\"formula\"").
#' 
#' list("Note"): When we first implemented list("is_formula()"), we thought it
#' best to treat unevaluated formulas as formulas by default (see
#' section below). Now we think this default introduces too many edge
#' cases in normal code. We recommend always supplying list("scoped = TRUE"). Unevaluated formulas can be handled via a list("is_call(x, \"~\")")
#' branch.
#' 
#'
#' @param x An object to test.
#' @param scoped A boolean indicating whether the quosure is scoped,
#' that is, has a valid environment attribute and inherits from
#' \code{"formula"}. If \code{NULL}, the scope is not inspected.
#' @param lhs A boolean indicating whether the formula has a left-hand
#' side. If \code{NULL}, the LHS is not inspected and \code{is_formula()}
#' returns \code{TRUE} for both one- and two-sided formulas.
#' @name is_bare_formula
#' @importFrom rlang is_bare_formula
#' @export
is_bare_formula <- rlang::is_bare_formula


#' @title is_scalar_vector function from rlang
#'
#' @description Re-export of \code{rlang::is_scalar_vector}.
#' See the original package documentation for full details.
#' These predicates check for a given type and whether the vector is
#' "scalar", that is, of length 1.
#' 
#' In addition to the length check, list("is_string()") and list("is_bool()")
#' return list("FALSE") if their input is missing. This is useful for
#' type-checking arguments, when your function expects a single string
#' or a single list("TRUE") or list("FALSE").
#' 
#'
#' @param x object to be tested.
#' @name is_scalar_vector
#' @importFrom rlang is_scalar_vector
#' @export
is_scalar_vector <- rlang::is_scalar_vector


#' @title is_warning function from rlang
#'
#' @description Re-export of \code{rlang::is_warning}.
#' See the original package documentation for full details.
#' Is object a condition?
#' 
#'
#' @param x An object to test.
#' @name is_warning
#' @importFrom rlang is_warning
#' @export
is_warning <- rlang::is_warning


#' @title is_error function from rlang
#'
#' @description Re-export of \code{rlang::is_error}.
#' See the original package documentation for full details.
#' Is object a condition?
#' 
#'
#' @param x An object to test.
#' @name is_error
#' @importFrom rlang is_error
#' @export
is_error <- rlang::is_error


#' @title is_bare_numeric function from rlang
#'
#' @description Re-export of \code{rlang::is_bare_numeric}.
#' See the original package documentation for full details.
#' These predicates check for a given type but only return list("TRUE") for
#' bare R objects. Bare objects have no class attributes. For example,
#' a data frame is a list, but not a bare list.
#' 
#'
#' @param x Object to be tested.
#' @param n Expected length of a vector.
#' @name is_bare_numeric
#' @importFrom rlang is_bare_numeric
#' @export
is_bare_numeric <- rlang::is_bare_numeric


#' @title is_bare_complex function from rlang
#'
#' @description Re-export of \code{rlang::is_bare_complex}.
#' See the original package documentation for full details.
#' These predicates check for a given type but only return list("TRUE") for
#' bare R objects. Bare objects have no class attributes. For example,
#' a data frame is a list, but not a bare list.
#' 
#'
#' @param x Object to be tested.
#' @param n Expected length of a vector.
#' @name is_bare_complex
#' @importFrom rlang is_bare_complex
#' @export
is_bare_complex <- rlang::is_bare_complex


#' @title is_lambda function from rlang
#'
#' @description Re-export of \code{rlang::is_lambda}.
#' See the original package documentation for full details.
#' list("as_function()") transforms a one-sided formula into a function.
#' This powers the lambda syntax in packages like purrr.
#' 
#'
#' @param x A function or formula.
#' 
#' If a \strong{function}, it is used as is.
#' 
#' If a \strong{formula}, e.g. \code{~ .x + 2}, it is converted to a function
#' with up to two arguments: \code{.x} (single argument) or \code{.x} and \code{.y}
#' (two arguments). The \code{.} placeholder can be used instead of \code{.x}.
#' This allows you to create very compact anonymous functions (lambdas) with up
#' to two inputs. Functions created from formulas have a special
#' class. Use \code{is_lambda()} to test for it.
#' 
#' If a \strong{string}, the function is looked up in \code{env}. Note that
#' this interface is strictly for user convenience because of the
#' scoping issues involved. Package developers should avoid
#' supplying functions by name and instead supply them by value.
#' @name is_lambda
#' @importFrom rlang is_lambda
#' @export
is_lambda <- rlang::is_lambda


#' @title is_bool function from rlang
#'
#' @description Re-export of \code{rlang::is_bool}.
#' See the original package documentation for full details.
#' These predicates check for a given type and whether the vector is
#' "scalar", that is, of length 1.
#' 
#' In addition to the length check, list("is_string()") and list("is_bool()")
#' return list("FALSE") if their input is missing. This is useful for
#' type-checking arguments, when your function expects a single string
#' or a single list("TRUE") or list("FALSE").
#' 
#'
#' @param x object to be tested.
#' @name is_bool
#' @importFrom rlang is_bool
#' @export
is_bool <- rlang::is_bool


#' @title is_cpl_na function from rlang
#'
#' @description Re-export of \code{rlang::is_cpl_na}.
#' See the original package documentation for full details.
#' list(list("html"), list(list(list("https://lifecycle.r-lib.org/articles/stages.html#questioning"), list(list(list("lifecycle-questioning.svg"), list("options: alt='[Questioning]'"))))), list(list("[Questioning]")))
#' 
#' list("are_na()") checks for missing values in a vector and is equivalent
#' to list(list("base::is.na()")). It is a vectorised predicate, meaning that its
#' output is always the same length as its input. On the other hand,
#' list("is_na()") is a scalar predicate and always returns a scalar
#' boolean, list("TRUE") or list("FALSE"). If its input is not scalar, it returns
#' list("FALSE"). Finally, there are typed versions that check for
#' particular list("missing types").
#' 
#'
#' @param x An object to test
#' @name is_cpl_na
#' @importFrom rlang is_cpl_na
#' @export
is_cpl_na <- rlang::is_cpl_na


#' @title is_node_list function from rlang
#'
#' @description Re-export of \code{rlang::is_node_list}.
#' See the original package documentation for full details.
#' list("\n", list(), " ", list("is_pairlist()"), " checks that ", list("x"), " has type ", list("pairlist"), ".\n", list(), " ", list("is_node()"), " checks that ", list("x"), " has type ", list("pairlist"), " or ", list("language"), ".\n", "It tests whether ", list("x"), " is a node that has a CAR and a CDR,\n", "including callable nodes (language objects).\n", list(), " ", list("is_node_list()"), " checks that ", list("x"), " has type ", list("pairlist"), " or ", list("NULL"), ".\n", list("NULL"), 
#'     " is the empty node list.\n")
#' 
#'
#' @param x Object to test.
#' @name is_node_list
#' @importFrom rlang is_node_list
#' @export
is_node_list <- rlang::is_node_list


#' @title is_primitive function from rlang
#'
#' @description Re-export of \code{rlang::is_primitive}.
#' See the original package documentation for full details.
#' The R language defines two different types of functions: primitive
#' functions, which are low-level, and closures, which are the regular
#' kind of functions.
#' 
#'
#' @param x Object to be tested.
#' @name is_primitive
#' @importFrom rlang is_primitive
#' @export
is_primitive <- rlang::is_primitive


#' @title is_callable function from rlang
#'
#' @description Re-export of \code{rlang::is_callable}.
#' See the original package documentation for full details.
#' A callable object is an object that can appear in the function
#' position of a call (as opposed to argument position). This includes
#' list("symbolic objects") that evaluate to a function or
#' literal functions embedded in the call.
#' 
#'
#' @param x An object to test.
#' @name is_callable
#' @importFrom rlang is_callable
#' @export
is_callable <- rlang::is_callable


#' @title is_scalar_integerish function from rlang
#'
#' @description Re-export of \code{rlang::is_scalar_integerish}.
#' See the original package documentation for full details.
#' These predicates check whether R considers a number vector to be
#' integer-like, according to its own tolerance check (which is in
#' fact delegated to the C library). This function is not adapted to
#' data analysis, see the help for list(list("base::is.integer()")) for examples
#' of how to check for whole numbers.
#' 
#' Things to consider when checking for integer-like doubles:
#' list("\n", list(), " This check can be expensive because the whole double vector has\n", "to be traversed and checked.\n", list(), " Large double values may be integerish but may still not be\n", "coercible to integer. This is because integers in R only support\n", "values up to ", list("2^31 - 1"), " while numbers stored as double can be\n", "much larger.\n")
#' 
#'
#' @param x Object to be tested.
#' @param finite Whether all values of the vector are finite. The
#' non-finite values are \code{NA}, \code{Inf}, \code{-Inf} and \code{NaN}. Setting this
#' to something other than \code{NULL} can be expensive because the whole
#' vector needs to be traversed and checked.
#' @name is_scalar_integerish
#' @importFrom rlang is_scalar_integerish
#' @export
is_scalar_integerish <- rlang::is_scalar_integerish


#' @title is_symbolic function from rlang
#'
#' @description Re-export of \code{rlang::is_symbolic}.
#' See the original package documentation for full details.
#' In rlang, an list("expression") is the return type of list(list("parse_expr()")), the
#' set of objects that can be obtained from parsing R code. Under this
#' definition expressions include numbers, strings, list("NULL"), symbols,
#' and function calls. These objects can be classified as:
#' list("\n", list(), " Symbolic objects, i.e. symbols and function calls (for which\n", list("is_symbolic()"), " returns ", list("TRUE"), ")\n", list(), " Syntactic literals, i.e. scalar atomic objects and ", list("NULL"), "\n", "(testable with ", list("is_syntactic_literal()"), ")\n")
#' 
#' list("is_expression()") returns list("TRUE") if the input is either a symbolic
#' object or a syntactic literal. If a call, the elements of the call
#' must all be expressions as well. Unparsable calls are not
#' considered expressions in this narrow definition.
#' 
#' Note that in base R, there exists list(list("expression()")) vectors, a data
#' type similar to a list that supports special attributes created by
#' the parser called source references. This data type is not
#' supported in rlang.
#' 
#'
#' @param x An object to test.
#' @name is_symbolic
#' @importFrom rlang is_symbolic
#' @export
is_symbolic <- rlang::is_symbolic


#' @title is_symbol function from rlang
#'
#' @description Re-export of \code{rlang::is_symbol}.
#' See the original package documentation for full details.
#' Is object a symbol?
#' 
#'
#' @param x An object to test.
#' @param name An optional name or vector of names that the symbol
#' should match.
#' @name is_symbol
#' @importFrom rlang is_symbol
#' @export
is_symbol <- rlang::is_symbol


#' @title is_node function from rlang
#'
#' @description Re-export of \code{rlang::is_node}.
#' See the original package documentation for full details.
#' list("\n", list(), " ", list("is_pairlist()"), " checks that ", list("x"), " has type ", list("pairlist"), ".\n", list(), " ", list("is_node()"), " checks that ", list("x"), " has type ", list("pairlist"), " or ", list("language"), ".\n", "It tests whether ", list("x"), " is a node that has a CAR and a CDR,\n", "including callable nodes (language objects).\n", list(), " ", list("is_node_list()"), " checks that ", list("x"), " has type ", list("pairlist"), " or ", list("NULL"), ".\n", list("NULL"), 
#'     " is the empty node list.\n")
#' 
#'
#' @param x Object to test.
#' @name is_node
#' @importFrom rlang is_node
#' @export
is_node <- rlang::is_node


#' @title is_scalar_raw function from rlang
#'
#' @description Re-export of \code{rlang::is_scalar_raw}.
#' See the original package documentation for full details.
#' These predicates check for a given type and whether the vector is
#' "scalar", that is, of length 1.
#' 
#' In addition to the length check, list("is_string()") and list("is_bool()")
#' return list("FALSE") if their input is missing. This is useful for
#' type-checking arguments, when your function expects a single string
#' or a single list("TRUE") or list("FALSE").
#' 
#'
#' @param x object to be tested.
#' @name is_scalar_raw
#' @importFrom rlang is_scalar_raw
#' @export
is_scalar_raw <- rlang::is_scalar_raw


#' @title is_expression function from rlang
#'
#' @description Re-export of \code{rlang::is_expression}.
#' See the original package documentation for full details.
#' In rlang, an list("expression") is the return type of list(list("parse_expr()")), the
#' set of objects that can be obtained from parsing R code. Under this
#' definition expressions include numbers, strings, list("NULL"), symbols,
#' and function calls. These objects can be classified as:
#' list("\n", list(), " Symbolic objects, i.e. symbols and function calls (for which\n", list("is_symbolic()"), " returns ", list("TRUE"), ")\n", list(), " Syntactic literals, i.e. scalar atomic objects and ", list("NULL"), "\n", "(testable with ", list("is_syntactic_literal()"), ")\n")
#' 
#' list("is_expression()") returns list("TRUE") if the input is either a symbolic
#' object or a syntactic literal. If a call, the elements of the call
#' must all be expressions as well. Unparsable calls are not
#' considered expressions in this narrow definition.
#' 
#' Note that in base R, there exists list(list("expression()")) vectors, a data
#' type similar to a list that supports special attributes created by
#' the parser called source references. This data type is not
#' supported in rlang.
#' 
#'
#' @param x An object to test.
#' @name is_expression
#' @importFrom rlang is_expression
#' @export
is_expression <- rlang::is_expression


#' @title is_bare_raw function from rlang
#'
#' @description Re-export of \code{rlang::is_bare_raw}.
#' See the original package documentation for full details.
#' These predicates check for a given type but only return list("TRUE") for
#' bare R objects. Bare objects have no class attributes. For example,
#' a data frame is a list, but not a bare list.
#' 
#'
#' @param x Object to be tested.
#' @param n Expected length of a vector.
#' @name is_bare_raw
#' @importFrom rlang is_bare_raw
#' @export
is_bare_raw <- rlang::is_bare_raw


#' @title is_scalar_bytes function from rlang
#'
#' @description Re-export of \code{rlang::is_scalar_bytes}.
#' See the original package documentation for full details.
#' These predicates check for a given type and whether the vector is
#' "scalar", that is, of length 1.
#' 
#' In addition to the length check, list("is_string()") and list("is_bool()")
#' return list("FALSE") if their input is missing. This is useful for
#' type-checking arguments, when your function expects a single string
#' or a single list("TRUE") or list("FALSE").
#' 
#'
#' @param x object to be tested.
#' @name is_scalar_bytes
#' @importFrom rlang is_scalar_bytes
#' @export
is_scalar_bytes <- rlang::is_scalar_bytes


#' @title is_bare_list function from rlang
#'
#' @description Re-export of \code{rlang::is_bare_list}.
#' See the original package documentation for full details.
#' These predicates check for a given type but only return list("TRUE") for
#' bare R objects. Bare objects have no class attributes. For example,
#' a data frame is a list, but not a bare list.
#' 
#'
#' @param x Object to be tested.
#' @param n Expected length of a vector.
#' @name is_bare_list
#' @importFrom rlang is_bare_list
#' @export
is_bare_list <- rlang::is_bare_list


#' @title is_bare_environment function from rlang
#'
#' @description Re-export of \code{rlang::is_bare_environment}.
#' See the original package documentation for full details.
#' list("is_bare_environment()") tests whether list("x") is an environment without a s3 or
#' s4 class.
#' 
#'
#' @param x object to test
#' @name is_bare_environment
#' @importFrom rlang is_bare_environment
#' @export
is_bare_environment <- rlang::is_bare_environment


#' @title is_weakref function from rlang
#'
#' @description Re-export of \code{rlang::is_weakref}.
#' See the original package documentation for full details.
#' Is object a weak reference?
#' 
#'
#' @param x An object to test.
#' @name is_weakref
#' @importFrom rlang is_weakref
#' @export
is_weakref <- rlang::is_weakref


#' @title is_file_empty function from fs
#'
#' @description Re-export of \code{fs::is_file_empty}.
#' See the original package documentation for full details.
#' Functions to test for file types
#' 
#'
#' @param path A character vector of one or more paths.
#' @param follow If \code{TRUE}, symbolic links will be followed (recursively) and
#' the results will be that of the final file rather than the link.
#' @name is_file_empty
#' @importFrom fs is_file_empty
#' @export
is_file_empty <- fs::is_file_empty


#' @title is_link function from fs
#'
#' @description Re-export of \code{fs::is_link}.
#' See the original package documentation for full details.
#' Functions to test for file types
#' 
#'
#' @param path A character vector of one or more paths.
#' @name is_link
#' @importFrom fs is_link
#' @export
is_link <- fs::is_link


#' @title is_dir function from fs
#'
#' @description Re-export of \code{fs::is_dir}.
#' See the original package documentation for full details.
#' Functions to test for file types
#' 
#'
#' @param path A character vector of one or more paths.
#' @param follow If \code{TRUE}, symbolic links will be followed (recursively) and
#' the results will be that of the final file rather than the link.
#' @name is_dir
#' @importFrom fs is_dir
#' @export
is_dir <- fs::is_dir


#' @title is_file function from fs
#'
#' @description Re-export of \code{fs::is_file}.
#' See the original package documentation for full details.
#' Functions to test for file types
#' 
#'
#' @param path A character vector of one or more paths.
#' @param follow If \code{TRUE}, symbolic links will be followed (recursively) and
#' the results will be that of the final file rather than the link.
#' @name is_file
#' @importFrom fs is_file
#' @export
is_file <- fs::is_file


#' @title is_absolute_path function from fs
#'
#' @description Re-export of \code{fs::is_absolute_path}.
#' See the original package documentation for full details.
#' Test if a path is an absolute path
#' 
#'
#' @param path A character vector of one or more paths.
#' @name is_absolute_path
#' @importFrom fs is_absolute_path
#' @export
is_absolute_path <- fs::is_absolute_path


#' @title is_existing_file function from fs
#'
#' @description Re-export of \code{fs::file_exists}.
#' See the original package documentation for full details.
#' list("file_exists(path)") is a shortcut for list("file_access(x, \"exists\")");
#' list("dir_exists(path)") and list("link_exists(path)") are similar but also check that
#' the path is a directory or link, respectively. (list("file_exists(path)") returns
#' list("TRUE") if list("path") exists and it is a directory.)
#' 
#'
#' @param path A character vector of one or more paths.
#' @name is_existing_file
#' @importFrom fs file_exists
#' @export
is_existing_file <- fs::file_exists


#' @title is_existing_dir function from fs
#'
#' @description Re-export of \code{fs::dir_exists}.
#' See the original package documentation for full details.
#' list("file_exists(path)") is a shortcut for list("file_access(x, \"exists\")");
#' list("dir_exists(path)") and list("link_exists(path)") are similar but also check that
#' the path is a directory or link, respectively. (list("file_exists(path)") returns
#' list("TRUE") if list("path") exists and it is a directory.)
#' 
#'
#' @param path A character vector of one or more paths.
#' @name is_existing_dir
#' @importFrom fs dir_exists
#' @export
is_existing_dir <- fs::dir_exists


#' @title is_existing_link function from fs
#'
#' @description Re-export of \code{fs::link_exists}.
#' See the original package documentation for full details.
#' list("file_exists(path)") is a shortcut for list("file_access(x, \"exists\")");
#' list("dir_exists(path)") and list("link_exists(path)") are similar but also check that
#' the path is a directory or link, respectively. (list("file_exists(path)") returns
#' list("TRUE") if list("path") exists and it is a directory.)
#' 
#'
#' @param path A character vector of one or more paths.
#' @name is_existing_link
#' @importFrom fs link_exists
#' @export
is_existing_link <- fs::link_exists


#' @title is_POSIXlt function from lubridate
#'
#' @description Re-export of \code{lubridate::is.POSIXlt}.
#' See the original package documentation for full details.
#' list(list("POSIXct()")) mirrors primitive contructors in base R (list(list("double()")),
#' list(list("character()")) etc.)
#' 
#'
#' @param x an R object
#' @name is_POSIXlt
#' @importFrom lubridate is.POSIXlt
#' @export
is_POSIXlt <- lubridate::is.POSIXlt


#' @title is_POSIXt function from lubridate
#'
#' @description Re-export of \code{lubridate::is.POSIXt}.
#' See the original package documentation for full details.
#' list(list("POSIXct()")) mirrors primitive contructors in base R (list(list("double()")),
#' list(list("character()")) etc.)
#' 
#'
#' @param x an R object
#' @name is_POSIXt
#' @importFrom lubridate is.POSIXt
#' @export
is_POSIXt <- lubridate::is.POSIXt


#' @title is_timepoint function from lubridate
#'
#' @description Re-export of \code{lubridate::is.timepoint}.
#' See the original package documentation for full details.
#' An instant is a specific moment in time. Most common date-time
#' objects (e.g, POSIXct, POSIXlt, and Date objects) are instants.
#' 
#'
#' @param x an R object
#' @name is_timepoint
#' @importFrom lubridate is.timepoint
#' @export
is_timepoint <- lubridate::is.timepoint


#' @title is_timespan function from lubridate
#'
#' @description Re-export of \code{lubridate::is.timespan}.
#' See the original package documentation for full details.
#' Is x a length of time?
#' 
#'
#' @param x an R object
#' @name is_timespan
#' @importFrom lubridate is.timespan
#' @export
is_timespan <- lubridate::is.timespan


#' @title is_difftime function from lubridate
#'
#' @description Re-export of \code{lubridate::is.difftime}.
#' See the original package documentation for full details.
#' Is x a difftime object?
#' 
#'
#' @param x an R object
#' @name is_difftime
#' @importFrom lubridate is.difftime
#' @export
is_difftime <- lubridate::is.difftime


#' @title is_duration function from lubridate
#'
#' @description Re-export of \code{lubridate::is.duration}.
#' See the original package documentation for full details.
#' list("duration()") creates a duration object with the specified values. Entries
#' for different units are cumulative. durations display as the number of
#' seconds in a time span. When this number is large, durations also display an
#' estimate in larger units, however, the underlying object is always recorded
#' as a fixed number of seconds. For display and creation purposes, units are
#' converted to seconds using their most common lengths in seconds. Minutes = 60
#' seconds, hours = 3600 seconds, days = 86400 seconds, weeks = 604800. Units
#' larger than weeks are not used due to their variability.
#' 
#'
#' @param x numeric value of the number of units to be contained in the
#' duration.
#' @name is_duration
#' @importFrom lubridate is.duration
#' @export
is_duration <- lubridate::is.duration


#' @title is_period function from lubridate
#'
#' @description Re-export of \code{lubridate::is.period}.
#' See the original package documentation for full details.
#' list("period()") creates or parses a period object with the specified values.
#' 
#'
#' @param x Any R object for \code{is.periods} and a numeric value of the number of
#' units for elementary constructors. With the exception of seconds(), x must
#' be an integer.
#' @name is_period
#' @importFrom lubridate is.period
#' @export
is_period <- lubridate::is.period


#' @title is_instant function from lubridate
#'
#' @description Re-export of \code{lubridate::is.instant}.
#' See the original package documentation for full details.
#' An instant is a specific moment in time. Most common date-time
#' objects (e.g, POSIXct, POSIXlt, and Date objects) are instants.
#' 
#'
#' @param x an R object
#' @name is_instant
#' @importFrom lubridate is.instant
#' @export
is_instant <- lubridate::is.instant


#' @title is_interval function from lubridate
#'
#' @description Re-export of \code{lubridate::is.interval}.
#' See the original package documentation for full details.
#' list("interval()") creates an list("Interval") object with the specified start and
#' end dates. If the start date occurs before the end date, the interval will be
#' positive. Otherwise, it will be negative. Character vectors in ISO 8601
#' format are supported from v1.7.2.
#' 
#' list("int_start()")/list("int_end()") and list("int_start<-()")/list("int_end<-()") are
#' "accessors" and "setters" respectively of the start/end date of an
#' interval.
#' 
#' list("int_flip()") reverses the order of the start date and end date in an
#' interval. The new interval takes place during the same timespan as the
#' original interval, but has the opposite direction.
#' 
#' list("int_shift()") shifts the start and end dates of an interval up or down the
#' timeline by a specified amount. Note that this may change the exact length of
#' the interval if the interval is shifted by a Period object. Intervals shifted
#' by a Duration or difftime object will retain their exact length in seconds.
#' 
#' list("int_overlaps()") tests if two intervals overlap.
#' 
#' list("int_standardize()") ensures all intervals in an interval object are
#' positive. If an interval is not positive, flip it so that it retains its
#' endpoints but becomes positive.
#' 
#' list("int_aligns()") tests if two intervals share an endpoint. The direction of
#' each interval is ignored. int_align tests whether the earliest or latest
#' moments of each interval occur at the same time.
#' 
#' list("int_diff()") returns the intervals that occur between the elements of a
#' vector of date-times. list("int_diff()") is similar to the POSIXt and Date
#' methods of list(list("diff()")), but returns an list("Interval") object instead
#' of a difftime object.
#' 
#'
#' @param x an R object
#' @name is_interval
#' @importFrom lubridate is.interval
#' @export
is_interval <- lubridate::is.interval

