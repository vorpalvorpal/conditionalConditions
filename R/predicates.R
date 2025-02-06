#' Predicate Functions
#'
#' These functions are re-exports of predicate functions from various packages,
#' standardised to use is_ prefix naming convention. All functionality is
#' preserved from the original functions.
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

#' @noRd
ensure_atomic_boolean <- function(fun, package) {
      args <- formals(get(fun, envir = asNamespace(package)))
      args_str <- paste(purrr::map2_chr(args,
        names(args),
        \(arg, arg_name) {
          if (rlang::is_missing(arg)) return(arg_name)
          else if (purrr::is_null(arg)) return(paste0(arg_name, ' = NULL', collapse = ''))
          else if (rlang::is_call(arg)) return(paste0(arg_name, ' = ', rlang::expr_text(arg), collapse = ''))
          #else if (!is.na(arg) && arg == '') return(paste0(arg_name, ' = ""', collapse = ''))
          else if (rlang::is_character(arg)) return(paste0(arg_name, ' = "', arg, '"', collapse = ''))
          else paste0(arg_name, ' = ', as.character(arg), collapse = '')
        }), collapse = ', ')
      args_pass <- paste(sprintf('%s = %s', names(args), names(args)), collapse = ', ')

      eval(parse(text = sprintf(
        'function(%s) {
          result <- get("%s", envir = asNamespace("%s"))(%s)
          if (length(result) != 1) return(FALSE)
          result
        }', args_str, fun, package, args_pass)))
    }

#' @aliases are_path_for_output, is_not_path_for_output, are_not_path_for_output

#' @title Check if a path is suited for creating an output file
#'
#' @description This is a re-export of \code{\link[checkmate:checkPathForOutput]{checkmate::test_path_for_output()}}, modified to have standardised naming and standardised vector handling.
#' Documentation is atuomatically generated from the original package documentation. See the \code{\link[checkmate:checkPathForOutput]{original}} for full details.
#' 
#' Check if a file path can be used safely to create a file and write to it.
#' 
#' This is checked:
#' \itemize{
#'  \item{Does list("dirname(x)") exist?
#'  }\item{Does no file under path list("x") exist?
#'  }\item{Is list("dirname(x)") writable?
#' }}
#' Paths are relative to the current working directory.
#' 
#' 
#' @returns 
#' - Calls to \code{is_path_for_output} are guaranteed to return a scalar boolean (ie. a single \code{TRUE} or \code{FALSE} value). If an argument of length > 1 is given, \code{FALSE} is returned.
#' - \code{are_path_for_output} is a wrapper around \code{\link[purrr]{map_lgl}(vec, \(i) is_path_for_output(i, ...))}. A boolean vector of the same length as the input is guaranteed.
#' - Calls to \code{is_not_path_for_output}/\code{are_not_path_for_output} negate the output of \code{is_path_for_output}/\code{are_path_for_output}.
#' @md
#'
#' @param x any\cr
#' Object to check.
#' @param overwrite \code{logical(1)}\cr
#' If \code{TRUE}, an existing file in place is allowed if it
#' it is both readable and writable.
#' Default is \code{FALSE}.
#' @param extension \code{character(1)}\cr
#' Extension of the file, e.g. \dQuote{txt} or \dQuote{tar.gz}.
#' @name is_path_for_output
#' @importFrom checkmate test_path_for_output
#' @importFrom purrr map_lgl
#' @export
is_path_for_output <- ensure_atomic_boolean('test_path_for_output', 'checkmate')
#' 
#' @rdname is_path_for_output
#' @export
is_not_path_for_output <- function(x, overwrite, extension) !is_path_for_output(x, overwrite, extension)
#' 
#' @rdname is_path_for_output
#' @export
are_path_for_output <- function(x, overwrite, extension) {
      purrr::map_lgl(x, \(x) is_path_for_output(x = x, overwrite = overwrite, extension = extension))
    }
#' 
#' @rdname is_path_for_output
#' @export
are_not_path_for_output <- function(x, overwrite, extension) !are_path_for_output(x = x, overwrite = overwrite, extension = extension)


#' @aliases are_posixct, is_not_posixct, are_not_posixct

#' @title Check that an argument is a date/time object in POSIXct format
#'
#' @description This is a re-export of \code{\link[checkmate:checkPOSIXct]{checkmate::test_posixct()}}, modified to have standardised naming and standardised vector handling.
#' Documentation is atuomatically generated from the original package documentation. See the \code{\link[checkmate:checkPOSIXct]{original}} for full details.
#' 
#' Checks that an object is of class \code{\link{POSIXct}}.
#' 
#' 
#' @returns 
#' - Calls to \code{is_posixct} are guaranteed to return a scalar boolean (ie. a single \code{TRUE} or \code{FALSE} value). If an argument of length > 1 is given, \code{FALSE} is returned.
#' - \code{are_posixct} is a wrapper around \code{\link[purrr]{map_lgl}(vec, \(i) is_posixct(i, ...))}. A boolean vector of the same length as the input is guaranteed.
#' - Calls to \code{is_not_posixct}/\code{are_not_posixct} negate the output of \code{is_posixct}/\code{are_posixct}.
#' @md
#'
#' @param x any\cr
#' Object to check.
#' @param lower \code{\link{Date}}\cr
#' All non-missing dates in \code{x} must be >= this POSIXct time. Must be provided in the same timezone as \code{x}.
#' @param upper \code{\link{Date}}\cr
#' All non-missing dates in \code{x} must be <= this POSIXct time. Must be provided in the same timezone as \code{x}.
#' @param any.missing \code{logical(1)}\cr
#' Are vectors with missing values allowed? Default is \code{TRUE}.
#' @param all.missing \code{logical(1)}\cr
#' Are vectors with no non-missing values allowed? Default is \code{TRUE}.
#' Note that empty vectors do not have non-missing values.
#' @param len \code{integer(1)}\cr
#' Exact expected length of \code{x}.
#' @param min.len \code{integer(1)}\cr
#' Minimal length of \code{x}.
#' @param max.len \code{integer(1)}\cr
#' Maximal length of \code{x}.
#' @param unique \code{logical(1)}\cr
#' Must all values be unique? Default is \code{FALSE}.
#' @param sorted \code{logical(1)}\cr
#' Elements must be sorted in ascending order. Missing values are ignored.
#' @param null.ok \code{logical(1)}\cr
#' If set to \code{TRUE}, \code{x} may also be \code{NULL}.
#' In this case only a type check of \code{x} is performed, all additional checks are disabled.
#' @name is_posixct
#' @importFrom checkmate test_posixct
#' @importFrom purrr map_lgl
#' @export
is_posixct <- ensure_atomic_boolean('test_posixct', 'checkmate')
#' 
#' @rdname is_posixct
#' @export
is_not_posixct <- function(x, lower, upper, any.missing, all.missing, len, min.len, max.len, unique, sorted, null.ok) !is_posixct(x, lower, upper, any.missing, all.missing, len, min.len, max.len, unique, sorted, null.ok)
#' 
#' @rdname is_posixct
#' @export
are_posixct <- function(x, lower, upper, any.missing, all.missing, len, min.len, max.len, unique, sorted, null.ok) {
      purrr::map_lgl(x, \(x) is_posixct(x = x, lower = lower, upper = upper, any.missing = any.missing, all.missing = all.missing, len = len, min.len = min.len, max.len = max.len, unique = unique, sorted = sorted, null.ok = null.ok))
    }
#' 
#' @rdname is_posixct
#' @export
are_not_posixct <- function(x, lower, upper, any.missing, all.missing, len, min.len, max.len, unique, sorted, null.ok) !are_posixct(x = x, lower = lower, upper = upper, any.missing = any.missing, all.missing = all.missing, len = len, min.len = min.len, max.len = max.len, unique = unique, sorted = sorted, null.ok = null.ok)


#' @aliases are_class, is_not_class, are_not_class

#' @title Check the class membership of an argument
#'
#' @description This is a re-export of \code{\link[checkmate:checkClass]{checkmate::test_class()}}, modified to have standardised naming and standardised vector handling.
#' Documentation is atuomatically generated from the original package documentation. See the \code{\link[checkmate:checkClass]{original}} for full details.
#' 
#' Check the class membership of an argument.
#' 
#' 
#' @returns 
#' - Calls to \code{is_class} are guaranteed to return a scalar boolean (ie. a single \code{TRUE} or \code{FALSE} value). If an argument of length > 1 is given, \code{FALSE} is returned.
#' - \code{are_class} is a wrapper around \code{\link[purrr]{map_lgl}(vec, \(i) is_class(i, ...))}. A boolean vector of the same length as the input is guaranteed.
#' - Calls to \code{is_not_class}/\code{are_not_class} negate the output of \code{is_class}/\code{are_class}.
#' @md
#'
#' @param x any\cr
#' Object to check.
#' @param classes \code{character}\cr
#' Class names to check for inheritance with \code{\link{inherits}}.
#' \code{x} must inherit from all specified classes.
#' @param ordered \code{logical(1)}\cr
#' Expect \code{x} to be specialized in provided order.
#' Default is \code{FALSE}.
#' @param null.ok \code{logical(1)}\cr
#' If set to \code{TRUE}, \code{x} may also be \code{NULL}.
#' In this case only a type check of \code{x} is performed, all additional checks are disabled.
#' @name is_class
#' @importFrom checkmate test_class
#' @importFrom purrr map_lgl
#' @export
is_class <- ensure_atomic_boolean('test_class', 'checkmate')
#' 
#' @rdname is_class
#' @export
is_not_class <- function(x, classes, ordered, null.ok) !is_class(x, classes, ordered, null.ok)
#' 
#' @rdname is_class
#' @export
are_class <- function(x, classes, ordered, null.ok) {
      purrr::map_lgl(x, \(x) is_class(x = x, classes = classes, ordered = ordered, null.ok = null.ok))
    }
#' 
#' @rdname is_class
#' @export
are_not_class <- function(x, classes, ordered, null.ok) !are_class(x = x, classes = classes, ordered = ordered, null.ok = null.ok)


#' @aliases are_flag, is_not_flag, are_not_flag

#' @title Check if an argument is a flag
#'
#' @description This is a re-export of \code{\link[checkmate:checkFlag]{checkmate::test_flag()}}, modified to have standardised naming and standardised vector handling.
#' Documentation is atuomatically generated from the original package documentation. See the \code{\link[checkmate:checkFlag]{original}} for full details.
#' 
#' A flag is defined as single logical value.
#' 
#' 
#' @returns 
#' - Calls to \code{is_flag} are guaranteed to return a scalar boolean (ie. a single \code{TRUE} or \code{FALSE} value). If an argument of length > 1 is given, \code{FALSE} is returned.
#' - \code{are_flag} is a wrapper around \code{\link[purrr]{map_lgl}(vec, \(i) is_flag(i, ...))}. A boolean vector of the same length as the input is guaranteed.
#' - Calls to \code{is_not_flag}/\code{are_not_flag} negate the output of \code{is_flag}/\code{are_flag}.
#' @md
#'
#' @param x any\cr
#' Object to check.
#' @param na.ok \code{logical(1)}\cr
#' Are missing values allowed? Default is \code{FALSE}.
#' @param null.ok \code{logical(1)}\cr
#' If set to \code{TRUE}, \code{x} may also be \code{NULL}.
#' In this case only a type check of \code{x} is performed, all additional checks are disabled.
#' @name is_flag
#' @importFrom checkmate test_flag
#' @importFrom purrr map_lgl
#' @export
is_flag <- ensure_atomic_boolean('test_flag', 'checkmate')
#' 
#' @rdname is_flag
#' @export
is_not_flag <- function(x, na.ok, null.ok) !is_flag(x, na.ok, null.ok)
#' 
#' @rdname is_flag
#' @export
are_flag <- function(x, na.ok, null.ok) {
      purrr::map_lgl(x, \(x) is_flag(x = x, na.ok = na.ok, null.ok = null.ok))
    }
#' 
#' @rdname is_flag
#' @export
are_not_flag <- function(x, na.ok, null.ok) !are_flag(x = x, na.ok = na.ok, null.ok = null.ok)


#' @aliases are_os, is_not_os, are_not_os

#' @title Check the operating system
#'
#' @description This is a re-export of \code{\link[checkmate:checkOS]{checkmate::test_os()}}, modified to have standardised naming and standardised vector handling.
#' Documentation is atuomatically generated from the original package documentation. See the \code{\link[checkmate:checkOS]{original}} for full details.
#' 
#' Check the operating system.
#' 
#' 
#' @returns 
#' - Calls to \code{is_os} are guaranteed to return a scalar boolean (ie. a single \code{TRUE} or \code{FALSE} value). If an argument of length > 1 is given, \code{FALSE} is returned.
#' - \code{are_os} is a wrapper around \code{\link[purrr]{map_lgl}(vec, \(i) is_os(i, ...))}. A boolean vector of the same length as the input is guaranteed.
#' - Calls to \code{is_not_os}/\code{are_not_os} negate the output of \code{is_os}/\code{are_os}.
#' @md
#'
#' @param os \code{character(1)}\cr
#' Check the operating system to be in a set with possible elements \dQuote{windows},
#' \dQuote{mac}, \dQuote{linux} and \dQuote{solaris}.
#' @name is_os
#' @importFrom checkmate test_os
#' @importFrom purrr map_lgl
#' @export
is_os <- ensure_atomic_boolean('test_os', 'checkmate')
#' 
#' @rdname is_os
#' @export
is_not_os <- function(os) !is_os(os)
#' 
#' @rdname is_os
#' @export
are_os <- function(os) {
      purrr::map_lgl(os, \(os) is_os(os = os))
    }
#' 
#' @rdname is_os
#' @export
are_not_os <- function(os) !are_os(os = os)


#' @aliases are_existing_test_file, is_not_existing_test_file, are_not_existing_test_file

#' @title Check existence and access rights of files
#'
#' @description This is a re-export of \code{\link[checkmate:checkFileExists]{checkmate::test_file_exists()}}, modified to have standardised naming and standardised vector handling.
#' Documentation is atuomatically generated from the original package documentation. See the \code{\link[checkmate:checkFileExists]{original}} for full details.
#' 
#' Check existence and access rights of files.
#' 
#' 
#' @returns 
#' - Calls to \code{is_existing_test_file} are guaranteed to return a scalar boolean (ie. a single \code{TRUE} or \code{FALSE} value). If an argument of length > 1 is given, \code{FALSE} is returned.
#' - \code{are_existing_test_file} is a wrapper around \code{\link[purrr]{map_lgl}(vec, \(i) is_existing_test_file(i, ...))}. A boolean vector of the same length as the input is guaranteed.
#' - Calls to \code{is_not_existing_test_file}/\code{are_not_existing_test_file} negate the output of \code{is_existing_test_file}/\code{are_existing_test_file}.
#' @md
#'
#' @param x any\cr
#' Object to check.
#' @param access \code{character(1)}\cr
#' Single string containing possible characters \sQuote{r}, \sQuote{w} and \sQuote{x} to
#' force a check for read, write or execute access rights, respectively.
#' Write and executable rights are not checked on Windows.
#' @param extension \code{character}\cr
#' Vector of allowed file extensions, matched case insensitive.
#' @name is_existing_test_file
#' @importFrom checkmate test_file_exists
#' @importFrom purrr map_lgl
#' @export
is_existing_test_file <- ensure_atomic_boolean('test_file_exists', 'checkmate')
#' 
#' @rdname is_existing_test_file
#' @export
is_not_existing_test_file <- function(x, access, extension) !is_existing_test_file(x, access, extension)
#' 
#' @rdname is_existing_test_file
#' @export
are_existing_test_file <- function(x, access, extension) {
      purrr::map_lgl(x, \(x) is_existing_test_file(x = x, access = access, extension = extension))
    }
#' 
#' @rdname is_existing_test_file
#' @export
are_not_existing_test_file <- function(x, access, extension) !are_existing_test_file(x = x, access = access, extension = extension)


#' @aliases are_permutation, is_not_permutation, are_not_permutation

#' @title Check if the arguments are permutations of each other.
#'
#' @description This is a re-export of \code{\link[checkmate:checkPermutation]{checkmate::test_permutation()}}, modified to have standardised naming and standardised vector handling.
#' Documentation is atuomatically generated from the original package documentation. See the \code{\link[checkmate:checkPermutation]{original}} for full details.
#' 
#' In contrast to \code{\link{checkSetEqual}}, the function tests for a true
#' permutation of the two vectors and also considers duplicated values.
#' Missing values are being treated as actual values by default.
#' Does not work on raw values.
#' 
#' 
#' @returns 
#' - Calls to \code{is_permutation} are guaranteed to return a scalar boolean (ie. a single \code{TRUE} or \code{FALSE} value). If an argument of length > 1 is given, \code{FALSE} is returned.
#' - \code{are_permutation} is a wrapper around \code{\link[purrr]{map_lgl}(vec, \(i) is_permutation(i, ...))}. A boolean vector of the same length as the input is guaranteed.
#' - Calls to \code{is_not_permutation}/\code{are_not_permutation} negate the output of \code{is_permutation}/\code{are_permutation}.
#' @md
#'
#' @param x any\cr
#' Object to check.
#' @param y \code{atomic}\cr
#' Vector to compare with. Atomic vector of type other than raw.
#' @param na.ok \code{logical(1)}\cr
#' Are missing values allowed? Default is \code{TRUE}.
#' @name is_permutation
#' @importFrom checkmate test_permutation
#' @importFrom purrr map_lgl
#' @export
is_permutation <- ensure_atomic_boolean('test_permutation', 'checkmate')
#' 
#' @rdname is_permutation
#' @export
is_not_permutation <- function(x, y, na.ok) !is_permutation(x, y, na.ok)
#' 
#' @rdname is_permutation
#' @export
are_permutation <- function(x, y, na.ok) {
      purrr::map_lgl(x, \(x) is_permutation(x = x, y = y, na.ok = na.ok))
    }
#' 
#' @rdname is_permutation
#' @export
are_not_permutation <- function(x, y, na.ok) !are_permutation(x = x, y = y, na.ok = na.ok)


#' @aliases are_r6, is_not_r6, are_not_r6

#' @title Check if an argument is an R6 class
#'
#' @description This is a re-export of \code{\link[checkmate:checkR6]{checkmate::test_r6()}}, modified to have standardised naming and standardised vector handling.
#' Documentation is atuomatically generated from the original package documentation. See the \code{\link[checkmate:checkR6]{original}} for full details.
#' 
#' Check if an argument is an R6 class.
#' 
#' 
#' @returns 
#' - Calls to \code{is_r6} are guaranteed to return a scalar boolean (ie. a single \code{TRUE} or \code{FALSE} value). If an argument of length > 1 is given, \code{FALSE} is returned.
#' - \code{are_r6} is a wrapper around \code{\link[purrr]{map_lgl}(vec, \(i) is_r6(i, ...))}. A boolean vector of the same length as the input is guaranteed.
#' - Calls to \code{is_not_r6}/\code{are_not_r6} negate the output of \code{is_r6}/\code{are_r6}.
#' @md
#'
#' @param x any\cr
#' Object to check.
#' @param classes \code{character}\cr
#' Class names to check for inheritance with \code{\link{inherits}}.
#' \code{x} must inherit from all specified classes.
#' @param ordered \code{logical(1)}\cr
#' Expect \code{x} to be specialized in provided order.
#' Default is \code{FALSE}.
#' @param cloneable \code{logical(1)}\cr
#' If \code{TRUE}, check that \code{x} has a \code{clone} method. If \code{FALSE}, ensure that
#' \code{x} is not cloneable.
#' @param public \code{character}\cr
#' Names of expected public slots. This includes active bindings.
#' @param private \code{character}\cr
#' Names of expected private slots.
#' @param null.ok \code{logical(1)}\cr
#' If set to \code{TRUE}, \code{x} may also be \code{NULL}.
#' In this case only a type check of \code{x} is performed, all additional checks are disabled.
#' @name is_r6
#' @importFrom checkmate test_r6
#' @importFrom purrr map_lgl
#' @export
is_r6 <- ensure_atomic_boolean('test_r6', 'checkmate')
#' 
#' @rdname is_r6
#' @export
is_not_r6 <- function(x, classes, ordered, cloneable, public, private, null.ok) !is_r6(x, classes, ordered, cloneable, public, private, null.ok)
#' 
#' @rdname is_r6
#' @export
are_r6 <- function(x, classes, ordered, cloneable, public, private, null.ok) {
      purrr::map_lgl(x, \(x) is_r6(x = x, classes = classes, ordered = ordered, cloneable = cloneable, public = public, private = private, null.ok = null.ok))
    }
#' 
#' @rdname is_r6
#' @export
are_not_r6 <- function(x, classes, ordered, cloneable, public, private, null.ok) !are_r6(x = x, classes = classes, ordered = ordered, cloneable = cloneable, public = public, private = private, null.ok = null.ok)


#' @aliases are_data_frame, is_not_data_frame, are_not_data_frame

#' @title Check if an argument is a data frame
#'
#' @description This is a re-export of \code{\link[checkmate:checkDataFrame]{checkmate::test_data_frame()}}, modified to have standardised naming and standardised vector handling.
#' Documentation is atuomatically generated from the original package documentation. See the \code{\link[checkmate:checkDataFrame]{original}} for full details.
#' 
#' Check if an argument is a data frame.
#' 
#' 
#' @returns 
#' - Calls to \code{is_data_frame} are guaranteed to return a scalar boolean (ie. a single \code{TRUE} or \code{FALSE} value). If an argument of length > 1 is given, \code{FALSE} is returned.
#' - \code{are_data_frame} is a wrapper around \code{\link[purrr]{map_lgl}(vec, \(i) is_data_frame(i, ...))}. A boolean vector of the same length as the input is guaranteed.
#' - Calls to \code{is_not_data_frame}/\code{are_not_data_frame} negate the output of \code{is_data_frame}/\code{are_data_frame}.
#' @md
#'
#' @param x any\cr
#' Object to check.
#' @param types \code{character}\cr
#' Character vector of class names. Each list element must inherit
#' from at least one of the provided types.
#' The types \dQuote{logical}, \dQuote{integer}, \dQuote{integerish}, \dQuote{double},
#' \dQuote{numeric}, \dQuote{complex}, \dQuote{character}, \dQuote{factor}, \dQuote{atomic}, \dQuote{vector}
#' \dQuote{atomicvector}, \dQuote{array}, \dQuote{matrix}, \dQuote{list}, \dQuote{function},
#' \dQuote{environment} and \dQuote{null} are supported.
#' For other types \code{\link{inherits}} is used as a fallback to check \code{x}'s inheritance.
#' Defaults to \code{character(0)} (no check).
#' @param any.missing \code{logical(1)}\cr
#' Are missing values allowed? Default is \code{TRUE}.
#' @param all.missing \code{logical(1)}\cr
#' Are columns with only missing values allowed? Default is \code{TRUE}.
#' @param min.rows \code{integer(1)}\cr
#' Minimum number of rows.
#' @param max.rows \code{integer(1)}\cr
#' Maximum number of rows.
#' @param min.cols \code{integer(1)}\cr
#' Minimum number of columns.
#' @param max.cols \code{integer(1)}\cr
#' Maximum number of columns.
#' @param nrows \code{integer(1)}\cr
#' Exact number of rows.
#' @param ncols \code{integer(1)}\cr
#' Exact number of columns.
#' @param row.names \code{character(1)}\cr
#' Check for row names. Default is \dQuote{NULL} (no check).
#' See \code{\link{checkNamed}} for possible values.
#' Note that you can use \code{\link{checkSubset}} to check for a specific set of names.
#' @param col.names \code{character(1)}\cr
#' Check for column names. Default is \dQuote{NULL} (no check).
#' See \code{\link{checkNamed}} for possible values.
#' Note that you can use \code{\link{checkSubset}} to test for a specific set of names.
#' @param null.ok \code{logical(1)}\cr
#' If set to \code{TRUE}, \code{x} may also be \code{NULL}.
#' In this case only a type check of \code{x} is performed, all additional checks are disabled.
#' @name is_data_frame
#' @importFrom checkmate test_data_frame
#' @importFrom purrr map_lgl
#' @export
is_data_frame <- ensure_atomic_boolean('test_data_frame', 'checkmate')
#' 
#' @rdname is_data_frame
#' @export
is_not_data_frame <- function(x, types, any.missing, all.missing, min.rows, max.rows, min.cols, max.cols, nrows, ncols, row.names, col.names, null.ok) !is_data_frame(x, types, any.missing, all.missing, min.rows, max.rows, min.cols, max.cols, nrows, ncols, row.names, col.names, null.ok)
#' 
#' @rdname is_data_frame
#' @export
are_data_frame <- function(x, types, any.missing, all.missing, min.rows, max.rows, min.cols, max.cols, nrows, ncols, row.names, col.names, null.ok) {
      purrr::map_lgl(x, \(x) is_data_frame(x = x, types = types, any.missing = any.missing, all.missing = all.missing, min.rows = min.rows, max.rows = max.rows, min.cols = min.cols, max.cols = max.cols, nrows = nrows, ncols = ncols, row.names = row.names, col.names = col.names, null.ok = null.ok))
    }
#' 
#' @rdname is_data_frame
#' @export
are_not_data_frame <- function(x, types, any.missing, all.missing, min.rows, max.rows, min.cols, max.cols, nrows, ncols, row.names, col.names, null.ok) !are_data_frame(x = x, types = types, any.missing = any.missing, all.missing = all.missing, min.rows = min.rows, max.rows = max.rows, min.cols = min.cols, max.cols = max.cols, nrows = nrows, ncols = ncols, row.names = row.names, col.names = col.names, null.ok = null.ok)


#' @aliases are_function, is_not_function, are_not_function

#' @title Check if an argument is a function
#'
#' @description This is a re-export of \code{\link[checkmate:checkFunction]{checkmate::test_function()}}, modified to have standardised naming and standardised vector handling.
#' Documentation is atuomatically generated from the original package documentation. See the \code{\link[checkmate:checkFunction]{original}} for full details.
#' 
#' Check if an argument is a function.
#' 
#' 
#' @returns 
#' - Calls to \code{is_function} are guaranteed to return a scalar boolean (ie. a single \code{TRUE} or \code{FALSE} value). If an argument of length > 1 is given, \code{FALSE} is returned.
#' - \code{are_function} is a wrapper around \code{\link[purrr]{map_lgl}(vec, \(i) is_function(i, ...))}. A boolean vector of the same length as the input is guaranteed.
#' - Calls to \code{is_not_function}/\code{are_not_function} negate the output of \code{is_function}/\code{are_function}.
#' @md
#'
#' @param x any\cr
#' Object to check.
#' @param args \code{character}\cr
#' Expected formal arguments. Checks that a function has no arguments if
#' set to \code{character(0)}.
#' Default is \code{NULL} (no check).
#' @param ordered \code{logical(1)}\cr
#' Flag whether the arguments provided in \code{args} must be the first
#' \code{length(args)} arguments of the function in the specified order.
#' Default is \code{FALSE}.
#' @param nargs \code{integer(1)}\cr
#' Required number of arguments, without \code{...}.
#' Default is \code{NULL} (no check).
#' @param null.ok \code{logical(1)}\cr
#' If set to \code{TRUE}, \code{x} may also be \code{NULL}.
#' In this case only a type check of \code{x} is performed, all additional checks are disabled.
#' @name is_function
#' @importFrom checkmate test_function
#' @importFrom purrr map_lgl
#' @export
is_function <- ensure_atomic_boolean('test_function', 'checkmate')
#' 
#' @rdname is_function
#' @export
is_not_function <- function(x, args, ordered, nargs, null.ok) !is_function(x, args, ordered, nargs, null.ok)
#' 
#' @rdname is_function
#' @export
are_function <- function(x, args, ordered, nargs, null.ok) {
      purrr::map_lgl(x, \(x) is_function(x = x, args = args, ordered = ordered, nargs = nargs, null.ok = null.ok))
    }
#' 
#' @rdname is_function
#' @export
are_not_function <- function(x, args, ordered, nargs, null.ok) !are_function(x = x, args = args, ordered = ordered, nargs = nargs, null.ok = null.ok)


#' @aliases are_environment, is_not_environment, are_not_environment

#' @title Check if an argument is an environment
#'
#' @description This is a re-export of \code{\link[checkmate:checkEnvironment]{checkmate::test_environment()}}, modified to have standardised naming and standardised vector handling.
#' Documentation is atuomatically generated from the original package documentation. See the \code{\link[checkmate:checkEnvironment]{original}} for full details.
#' 
#' Check if an argument is an environment.
#' 
#' 
#' @returns 
#' - Calls to \code{is_environment} are guaranteed to return a scalar boolean (ie. a single \code{TRUE} or \code{FALSE} value). If an argument of length > 1 is given, \code{FALSE} is returned.
#' - \code{are_environment} is a wrapper around \code{\link[purrr]{map_lgl}(vec, \(i) is_environment(i, ...))}. A boolean vector of the same length as the input is guaranteed.
#' - Calls to \code{is_not_environment}/\code{are_not_environment} negate the output of \code{is_environment}/\code{are_environment}.
#' @md
#'
#' @param x any\cr
#' Object to check.
#' @param contains \code{character}\cr
#' Vector of object names expected in the environment.
#' Defaults to \code{character(0)}.
#' @param null.ok \code{logical(1)}\cr
#' If set to \code{TRUE}, \code{x} may also be \code{NULL}.
#' In this case only a type check of \code{x} is performed, all additional checks are disabled.
#' @name is_environment
#' @importFrom checkmate test_environment
#' @importFrom purrr map_lgl
#' @export
is_environment <- ensure_atomic_boolean('test_environment', 'checkmate')
#' 
#' @rdname is_environment
#' @export
is_not_environment <- function(x, contains, null.ok) !is_environment(x, contains, null.ok)
#' 
#' @rdname is_environment
#' @export
are_environment <- function(x, contains, null.ok) {
      purrr::map_lgl(x, \(x) is_environment(x = x, contains = contains, null.ok = null.ok))
    }
#' 
#' @rdname is_environment
#' @export
are_not_environment <- function(x, contains, null.ok) !are_environment(x = x, contains = contains, null.ok = null.ok)


#' @aliases are_integerish, is_not_integerish, are_not_integerish

#' @title Check if an object is an integerish vector
#'
#' @description This is a re-export of \code{\link[checkmate:checkIntegerish]{checkmate::test_integerish()}}, modified to have standardised naming and standardised vector handling.
#' Documentation is atuomatically generated from the original package documentation. See the \code{\link[checkmate:checkIntegerish]{original}} for full details.
#' 
#' An integerish value is defined as value safely convertible to integer.
#' This includes integers and numeric values which sufficiently close to an
#' integer w.r.t. a numeric tolerance `tol`.
#' 
#' 
#' @returns 
#' - Calls to \code{is_integerish} are guaranteed to return a scalar boolean (ie. a single \code{TRUE} or \code{FALSE} value). If an argument of length > 1 is given, \code{FALSE} is returned.
#' - \code{are_integerish} is a wrapper around \code{\link[purrr]{map_lgl}(vec, \(i) is_integerish(i, ...))}. A boolean vector of the same length as the input is guaranteed.
#' - Calls to \code{is_not_integerish}/\code{are_not_integerish} negate the output of \code{is_integerish}/\code{are_integerish}.
#' @md
#'
#' @param x any\cr
#' Object to check.
#' @param tol \code{double(1)}\cr
#' Numerical tolerance used to check whether a double or complex can be converted.
#' Default is \code{sqrt(.Machine$double.eps)}.
#' @param lower \code{numeric(1)}\cr
#' Lower value all elements of \code{x} must be greater than or equal to.
#' @param upper \code{numeric(1)}\cr
#' Upper value all elements of \code{x} must be lower than or equal to.
#' @param any.missing \code{logical(1)}\cr
#' Are vectors with missing values allowed? Default is \code{TRUE}.
#' @param all.missing \code{logical(1)}\cr
#' Are vectors with no non-missing values allowed? Default is \code{TRUE}.
#' Note that empty vectors do not have non-missing values.
#' @param len \code{integer(1)}\cr
#' Exact expected length of \code{x}.
#' @param min.len \code{integer(1)}\cr
#' Minimal length of \code{x}.
#' @param max.len \code{integer(1)}\cr
#' Maximal length of \code{x}.
#' @param unique \code{logical(1)}\cr
#' Must all values be unique? Default is \code{FALSE}.
#' @param sorted \code{logical(1)}\cr
#' Elements must be sorted in ascending order. Missing values are ignored.
#' @param names \code{character(1)}\cr
#' Check for names. See \code{\link{checkNamed}} for possible values.
#' Default is \dQuote{any} which performs no check at all.
#' Note that you can use \code{\link{checkSubset}} to check for a specific set of names.
#' @param typed.missing \code{logical(1)}\cr
#' If set to \code{FALSE} (default), all types of missing values (\code{NA}, \code{NA_integer_},
#' \code{NA_real_}, \code{NA_character_} or \code{NA_character_}) as well as empty vectors are allowed
#' while type-checking atomic input.
#' Set to \code{TRUE} to enable strict type checking.
#' @param null.ok \code{logical(1)}\cr
#' If set to \code{TRUE}, \code{x} may also be \code{NULL}.
#' In this case only a type check of \code{x} is performed, all additional checks are disabled.
#' @name is_integerish
#' @importFrom checkmate test_integerish
#' @importFrom purrr map_lgl
#' @export
is_integerish <- ensure_atomic_boolean('test_integerish', 'checkmate')
#' 
#' @rdname is_integerish
#' @export
is_not_integerish <- function(x, tol, lower, upper, any.missing, all.missing, len, min.len, max.len, unique, sorted, names, typed.missing, null.ok) !is_integerish(x, tol, lower, upper, any.missing, all.missing, len, min.len, max.len, unique, sorted, names, typed.missing, null.ok)
#' 
#' @rdname is_integerish
#' @export
are_integerish <- function(x, tol, lower, upper, any.missing, all.missing, len, min.len, max.len, unique, sorted, names, typed.missing, null.ok) {
      purrr::map_lgl(x, \(x) is_integerish(x = x, tol = tol, lower = lower, upper = upper, any.missing = any.missing, all.missing = all.missing, len = len, min.len = min.len, max.len = max.len, unique = unique, sorted = sorted, names = names, typed.missing = typed.missing, null.ok = null.ok))
    }
#' 
#' @rdname is_integerish
#' @export
are_not_integerish <- function(x, tol, lower, upper, any.missing, all.missing, len, min.len, max.len, unique, sorted, names, typed.missing, null.ok) !are_integerish(x = x, tol = tol, lower = lower, upper = upper, any.missing = any.missing, all.missing = all.missing, len = len, min.len = min.len, max.len = max.len, unique = unique, sorted = sorted, names = names, typed.missing = typed.missing, null.ok = null.ok)


#' @aliases are_atomic_vector, is_not_atomic_vector, are_not_atomic_vector

#' @title Check that an argument is an atomic vector
#'
#' @description This is a re-export of \code{\link[checkmate:checkAtomicVector]{checkmate::test_atomic_vector()}}, modified to have standardised naming and standardised vector handling.
#' Documentation is atuomatically generated from the original package documentation. See the \code{\link[checkmate:checkAtomicVector]{original}} for full details.
#' 
#' An atomic vector is defined slightly different from specifications in
#' \code{\link{is.atomic}} and \code{\link{is.vector}}:
#' An atomic vector is either \code{logical}, \code{integer}, \code{numeric},
#' \code{complex}, \code{character} or \code{raw} and can have any attributes except a
#' dimension attribute (like matrices).
#' I.e., a \code{factor} is an atomic vector, but a matrix or \code{NULL} are not.
#' In short, this is basically equivalent to \code{is.atomic(x) && !is.null(x) && is.null(dim(x))}.
#' 
#' 
#' @returns 
#' - Calls to \code{is_atomic_vector} are guaranteed to return a scalar boolean (ie. a single \code{TRUE} or \code{FALSE} value). If an argument of length > 1 is given, \code{FALSE} is returned.
#' - \code{are_atomic_vector} is a wrapper around \code{\link[purrr]{map_lgl}(vec, \(i) is_atomic_vector(i, ...))}. A boolean vector of the same length as the input is guaranteed.
#' - Calls to \code{is_not_atomic_vector}/\code{are_not_atomic_vector} negate the output of \code{is_atomic_vector}/\code{are_atomic_vector}.
#' @md
#'
#' @param x any\cr
#' Object to check.
#' @param any.missing \code{logical(1)}\cr
#' Are vectors with missing values allowed? Default is \code{TRUE}.
#' @param all.missing \code{logical(1)}\cr
#' Are vectors with only missing values allowed? Default is \code{TRUE}.
#' @param len \code{integer(1)}\cr
#' Exact expected length of \code{x}.
#' @param min.len \code{integer(1)}\cr
#' Minimal length of \code{x}.
#' @param max.len \code{integer(1)}\cr
#' Maximal length of \code{x}.
#' @param unique \code{logical(1)}\cr
#' Must all values be unique? Default is \code{FALSE}.
#' @param names \code{character(1)}\cr
#' Check for names. See \code{\link{checkNamed}} for possible values.
#' Default is \dQuote{any} which performs no check at all.
#' @name is_atomic_vector
#' @importFrom checkmate test_atomic_vector
#' @importFrom purrr map_lgl
#' @export
is_atomic_vector <- ensure_atomic_boolean('test_atomic_vector', 'checkmate')
#' 
#' @rdname is_atomic_vector
#' @export
is_not_atomic_vector <- function(x, any.missing, all.missing, len, min.len, max.len, unique, names) !is_atomic_vector(x, any.missing, all.missing, len, min.len, max.len, unique, names)
#' 
#' @rdname is_atomic_vector
#' @export
are_atomic_vector <- function(x, any.missing, all.missing, len, min.len, max.len, unique, names) {
      purrr::map_lgl(x, \(x) is_atomic_vector(x = x, any.missing = any.missing, all.missing = all.missing, len = len, min.len = min.len, max.len = max.len, unique = unique, names = names))
    }
#' 
#' @rdname is_atomic_vector
#' @export
are_not_atomic_vector <- function(x, any.missing, all.missing, len, min.len, max.len, unique, names) !are_atomic_vector(x = x, any.missing = any.missing, all.missing = all.missing, len = len, min.len = min.len, max.len = max.len, unique = unique, names = names)


#' @aliases are_number, is_not_number, are_not_number

#' @title Check if an argument is a single numeric value
#'
#' @description This is a re-export of \code{\link[checkmate:checkNumber]{checkmate::test_number()}}, modified to have standardised naming and standardised vector handling.
#' Documentation is atuomatically generated from the original package documentation. See the \code{\link[checkmate:checkNumber]{original}} for full details.
#' 
#' Check if an argument is a single numeric value.
#' 
#' 
#' @returns 
#' - Calls to \code{is_number} are guaranteed to return a scalar boolean (ie. a single \code{TRUE} or \code{FALSE} value). If an argument of length > 1 is given, \code{FALSE} is returned.
#' - \code{are_number} is a wrapper around \code{\link[purrr]{map_lgl}(vec, \(i) is_number(i, ...))}. A boolean vector of the same length as the input is guaranteed.
#' - Calls to \code{is_not_number}/\code{are_not_number} negate the output of \code{is_number}/\code{are_number}.
#' @md
#'
#' @param x any\cr
#' Object to check.
#' @param na.ok \code{logical(1)}\cr
#' Are missing values allowed? Default is \code{FALSE}.
#' @param lower \code{numeric(1)}\cr
#' Lower value all elements of \code{x} must be greater than or equal to.
#' @param upper \code{numeric(1)}\cr
#' Upper value all elements of \code{x} must be lower than or equal to.
#' @param finite \code{logical(1)}\cr
#' Check for only finite values? Default is \code{FALSE}.
#' @param null.ok \code{logical(1)}\cr
#' If set to \code{TRUE}, \code{x} may also be \code{NULL}.
#' In this case only a type check of \code{x} is performed, all additional checks are disabled.
#' @name is_number
#' @importFrom checkmate test_number
#' @importFrom purrr map_lgl
#' @export
is_number <- ensure_atomic_boolean('test_number', 'checkmate')
#' 
#' @rdname is_number
#' @export
is_not_number <- function(x, na.ok, lower, upper, finite, null.ok) !is_number(x, na.ok, lower, upper, finite, null.ok)
#' 
#' @rdname is_number
#' @export
are_number <- function(x, na.ok, lower, upper, finite, null.ok) {
      purrr::map_lgl(x, \(x) is_number(x = x, na.ok = na.ok, lower = lower, upper = upper, finite = finite, null.ok = null.ok))
    }
#' 
#' @rdname is_number
#' @export
are_not_number <- function(x, na.ok, lower, upper, finite, null.ok) !are_number(x = x, na.ok = na.ok, lower = lower, upper = upper, finite = finite, null.ok = null.ok)


#' @aliases are_logical, is_not_logical, are_not_logical

#' @title Check if an argument is a vector of type logical
#'
#' @description This is a re-export of \code{\link[checkmate:checkLogical]{checkmate::test_logical()}}, modified to have standardised naming and standardised vector handling.
#' Documentation is atuomatically generated from the original package documentation. See the \code{\link[checkmate:checkLogical]{original}} for full details.
#' 
#' Check if an argument is a vector of type logical.
#' 
#' 
#' @returns 
#' - Calls to \code{is_logical} are guaranteed to return a scalar boolean (ie. a single \code{TRUE} or \code{FALSE} value). If an argument of length > 1 is given, \code{FALSE} is returned.
#' - \code{are_logical} is a wrapper around \code{\link[purrr]{map_lgl}(vec, \(i) is_logical(i, ...))}. A boolean vector of the same length as the input is guaranteed.
#' - Calls to \code{is_not_logical}/\code{are_not_logical} negate the output of \code{is_logical}/\code{are_logical}.
#' @md
#'
#' @param x any\cr
#' Object to check.
#' @param any.missing \code{logical(1)}\cr
#' Are vectors with missing values allowed? Default is \code{TRUE}.
#' @param all.missing \code{logical(1)}\cr
#' Are vectors with no non-missing values allowed? Default is \code{TRUE}.
#' Note that empty vectors do not have non-missing values.
#' @param len \code{integer(1)}\cr
#' Exact expected length of \code{x}.
#' @param min.len \code{integer(1)}\cr
#' Minimal length of \code{x}.
#' @param max.len \code{integer(1)}\cr
#' Maximal length of \code{x}.
#' @param unique \code{logical(1)}\cr
#' Must all values be unique? Default is \code{FALSE}.
#' @param names \code{character(1)}\cr
#' Check for names. See \code{\link{checkNamed}} for possible values.
#' Default is \dQuote{any} which performs no check at all.
#' Note that you can use \code{\link{checkSubset}} to check for a specific set of names.
#' @param typed.missing \code{logical(1)}\cr
#' If set to \code{FALSE} (default), all types of missing values (\code{NA}, \code{NA_integer_},
#' \code{NA_real_}, \code{NA_character_} or \code{NA_character_}) as well as empty vectors are allowed
#' while type-checking atomic input.
#' Set to \code{TRUE} to enable strict type checking.
#' @param null.ok \code{logical(1)}\cr
#' If set to \code{TRUE}, \code{x} may also be \code{NULL}.
#' In this case only a type check of \code{x} is performed, all additional checks are disabled.
#' @name is_logical
#' @importFrom checkmate test_logical
#' @importFrom purrr map_lgl
#' @export
is_logical <- ensure_atomic_boolean('test_logical', 'checkmate')
#' 
#' @rdname is_logical
#' @export
is_not_logical <- function(x, any.missing, all.missing, len, min.len, max.len, unique, names, typed.missing, null.ok) !is_logical(x, any.missing, all.missing, len, min.len, max.len, unique, names, typed.missing, null.ok)
#' 
#' @rdname is_logical
#' @export
are_logical <- function(x, any.missing, all.missing, len, min.len, max.len, unique, names, typed.missing, null.ok) {
      purrr::map_lgl(x, \(x) is_logical(x = x, any.missing = any.missing, all.missing = all.missing, len = len, min.len = min.len, max.len = max.len, unique = unique, names = names, typed.missing = typed.missing, null.ok = null.ok))
    }
#' 
#' @rdname is_logical
#' @export
are_not_logical <- function(x, any.missing, all.missing, len, min.len, max.len, unique, names, typed.missing, null.ok) !are_logical(x = x, any.missing = any.missing, all.missing = all.missing, len = len, min.len = min.len, max.len = max.len, unique = unique, names = names, typed.missing = typed.missing, null.ok = null.ok)


#' @aliases are_named, is_not_named, are_not_named

#' @title Check if an argument is named
#'
#' @description This is a re-export of \code{\link[checkmate:checkNamed]{checkmate::test_named()}}, modified to have standardised naming and standardised vector handling.
#' Documentation is atuomatically generated from the original package documentation. See the \code{\link[checkmate:checkNamed]{original}} for full details.
#' 
#' Check if an argument is named.
#' 
#' 
#' @returns 
#' - Calls to \code{is_named} are guaranteed to return a scalar boolean (ie. a single \code{TRUE} or \code{FALSE} value). If an argument of length > 1 is given, \code{FALSE} is returned.
#' - \code{are_named} is a wrapper around \code{\link[purrr]{map_lgl}(vec, \(i) is_named(i, ...))}. A boolean vector of the same length as the input is guaranteed.
#' - Calls to \code{is_not_named}/\code{are_not_named} negate the output of \code{is_named}/\code{are_named}.
#' @md
#'
#' @param x any\cr
#' Object to check.
#' @param type character(1)\cr
#' Select the check(s) to perform.
#' \dQuote{unnamed} checks \code{x} to be unnamed.
#' \dQuote{named} (default) checks \code{x} to be named which excludes names to be \code{NA} or empty (\code{""}).
#' \dQuote{unique} additionally tests for non-duplicated names.
#' \dQuote{strict} checks for unique names which comply to R's variable name restrictions.
#' Note that for zero-length \code{x} every name check evaluates to \code{TRUE}.
#' @name is_named
#' @importFrom checkmate test_named
#' @importFrom purrr map_lgl
#' @export
is_named <- ensure_atomic_boolean('test_named', 'checkmate')
#' 
#' @rdname is_named
#' @export
is_not_named <- function(x, type) !is_named(x, type)
#' 
#' @rdname is_named
#' @export
are_named <- function(x, type) {
      purrr::map_lgl(x, \(x) is_named(x = x, type = type))
    }
#' 
#' @rdname is_named
#' @export
are_not_named <- function(x, type) !are_named(x = x, type = type)


#' @aliases are_tibble, is_not_tibble, are_not_tibble

#' @title Check if an argument is a tibble
#'
#' @description This is a re-export of \code{\link[checkmate:checkTibble]{checkmate::test_tibble()}}, modified to have standardised naming and standardised vector handling.
#' Documentation is atuomatically generated from the original package documentation. See the \code{\link[checkmate:checkTibble]{original}} for full details.
#' 
#' Check if an argument is a tibble.
#' 
#' 
#' @returns 
#' - Calls to \code{is_tibble} are guaranteed to return a scalar boolean (ie. a single \code{TRUE} or \code{FALSE} value). If an argument of length > 1 is given, \code{FALSE} is returned.
#' - \code{are_tibble} is a wrapper around \code{\link[purrr]{map_lgl}(vec, \(i) is_tibble(i, ...))}. A boolean vector of the same length as the input is guaranteed.
#' - Calls to \code{is_not_tibble}/\code{are_not_tibble} negate the output of \code{is_tibble}/\code{are_tibble}.
#' @md
#'
#' @param x any\cr
#' Object to check.
#' @param types \code{character}\cr
#' Character vector of class names. Each list element must inherit
#' from at least one of the provided types.
#' The types \dQuote{logical}, \dQuote{integer}, \dQuote{integerish}, \dQuote{double},
#' \dQuote{numeric}, \dQuote{complex}, \dQuote{character}, \dQuote{factor}, \dQuote{atomic}, \dQuote{vector}
#' \dQuote{atomicvector}, \dQuote{array}, \dQuote{matrix}, \dQuote{list}, \dQuote{function},
#' \dQuote{environment} and \dQuote{null} are supported.
#' For other types \code{\link{inherits}} is used as a fallback to check \code{x}'s inheritance.
#' Defaults to \code{character(0)} (no check).
#' @param any.missing \code{logical(1)}\cr
#' Are missing values allowed? Default is \code{TRUE}.
#' @param all.missing \code{logical(1)}\cr
#' Are matrices with only missing values allowed? Default is \code{TRUE}.
#' @param min.rows \code{integer(1)}\cr
#' Minimum number of rows.
#' @param max.rows \code{integer(1)}\cr
#' Maximum number of rows.
#' @param min.cols \code{integer(1)}\cr
#' Minimum number of columns.
#' @param max.cols \code{integer(1)}\cr
#' Maximum number of columns.
#' @param nrows \code{integer(1)}\cr
#' Exact number of rows.
#' @param ncols \code{integer(1)}\cr
#' Exact number of columns.
#' @param row.names \code{character(1)}\cr
#' Check for row names. Default is \dQuote{NULL} (no check).
#' See \code{\link{checkNamed}} for possible values.
#' Note that you can use \code{\link{checkSubset}} to check for a specific set of names.
#' @param col.names \code{character(1)}\cr
#' Check for column names. Default is \dQuote{NULL} (no check).
#' See \code{\link{checkNamed}} for possible values.
#' Note that you can use \code{\link{checkSubset}} to test for a specific set of names.
#' @param null.ok \code{logical(1)}\cr
#' If set to \code{TRUE}, \code{x} may also be \code{NULL}.
#' In this case only a type check of \code{x} is performed, all additional checks are disabled.
#' @name is_tibble
#' @importFrom checkmate test_tibble
#' @importFrom purrr map_lgl
#' @export
is_tibble <- ensure_atomic_boolean('test_tibble', 'checkmate')
#' 
#' @rdname is_tibble
#' @export
is_not_tibble <- function(x, types, any.missing, all.missing, min.rows, max.rows, min.cols, max.cols, nrows, ncols, row.names, col.names, null.ok) !is_tibble(x, types, any.missing, all.missing, min.rows, max.rows, min.cols, max.cols, nrows, ncols, row.names, col.names, null.ok)
#' 
#' @rdname is_tibble
#' @export
are_tibble <- function(x, types, any.missing, all.missing, min.rows, max.rows, min.cols, max.cols, nrows, ncols, row.names, col.names, null.ok) {
      purrr::map_lgl(x, \(x) is_tibble(x = x, types = types, any.missing = any.missing, all.missing = all.missing, min.rows = min.rows, max.rows = max.rows, min.cols = min.cols, max.cols = max.cols, nrows = nrows, ncols = ncols, row.names = row.names, col.names = col.names, null.ok = null.ok))
    }
#' 
#' @rdname is_tibble
#' @export
are_not_tibble <- function(x, types, any.missing, all.missing, min.rows, max.rows, min.cols, max.cols, nrows, ncols, row.names, col.names, null.ok) !are_tibble(x = x, types = types, any.missing = any.missing, all.missing = all.missing, min.rows = min.rows, max.rows = max.rows, min.cols = min.cols, max.cols = max.cols, nrows = nrows, ncols = ncols, row.names = row.names, col.names = col.names, null.ok = null.ok)


#' @aliases are_directory, is_not_directory, are_not_directory

#' @title Check for existence and access rights of directories
#'
#' @description This is a re-export of \code{\link[checkmate:checkDirectoryExists]{checkmate::test_directory()}}, modified to have standardised naming and standardised vector handling.
#' Documentation is atuomatically generated from the original package documentation. See the \code{\link[checkmate:checkDirectoryExists]{original}} for full details.
#' 
#' Check for existence and access rights of directories.
#' 
#' 
#' @returns 
#' - Calls to \code{is_directory} are guaranteed to return a scalar boolean (ie. a single \code{TRUE} or \code{FALSE} value). If an argument of length > 1 is given, \code{FALSE} is returned.
#' - \code{are_directory} is a wrapper around \code{\link[purrr]{map_lgl}(vec, \(i) is_directory(i, ...))}. A boolean vector of the same length as the input is guaranteed.
#' - Calls to \code{is_not_directory}/\code{are_not_directory} negate the output of \code{is_directory}/\code{are_directory}.
#' @md
#'
#' @param x any\cr
#' Object to check.
#' @param access \code{character(1)}\cr
#' Single string containing possible characters \sQuote{r}, \sQuote{w} and \sQuote{x} to
#' force a check for read, write or execute access rights, respectively.
#' Write and executable rights are not checked on Windows.
#' @name is_directory
#' @importFrom checkmate test_directory
#' @importFrom purrr map_lgl
#' @export
is_directory <- ensure_atomic_boolean('test_directory', 'checkmate')
#' 
#' @rdname is_directory
#' @export
is_not_directory <- function(x, access) !is_directory(x, access)
#' 
#' @rdname is_directory
#' @export
are_directory <- function(x, access) {
      purrr::map_lgl(x, \(x) is_directory(x = x, access = access))
    }
#' 
#' @rdname is_directory
#' @export
are_not_directory <- function(x, access) !are_directory(x = x, access = access)


#' @aliases are_int, is_not_int, are_not_int

#' @title Check if an argument is a single integerish value
#'
#' @description This is a re-export of \code{\link[checkmate:checkInt]{checkmate::test_int()}}, modified to have standardised naming and standardised vector handling.
#' Documentation is atuomatically generated from the original package documentation. See the \code{\link[checkmate:checkInt]{original}} for full details.
#' 
#' Check if an argument is a single integerish value.
#' 
#' 
#' @returns 
#' - Calls to \code{is_int} are guaranteed to return a scalar boolean (ie. a single \code{TRUE} or \code{FALSE} value). If an argument of length > 1 is given, \code{FALSE} is returned.
#' - \code{are_int} is a wrapper around \code{\link[purrr]{map_lgl}(vec, \(i) is_int(i, ...))}. A boolean vector of the same length as the input is guaranteed.
#' - Calls to \code{is_not_int}/\code{are_not_int} negate the output of \code{is_int}/\code{are_int}.
#' @md
#'
#' @param x any\cr
#' Object to check.
#' @param na.ok \code{logical(1)}\cr
#' Are missing values allowed? Default is \code{FALSE}.
#' @param lower \code{numeric(1)}\cr
#' Lower value all elements of \code{x} must be greater than or equal to.
#' @param upper \code{numeric(1)}\cr
#' Upper value all elements of \code{x} must be lower than or equal to.
#' @param tol \code{double(1)}\cr
#' Numerical tolerance used to check whether a double or complex can be converted.
#' Default is \code{sqrt(.Machine$double.eps)}.
#' @param null.ok \code{logical(1)}\cr
#' If set to \code{TRUE}, \code{x} may also be \code{NULL}.
#' In this case only a type check of \code{x} is performed, all additional checks are disabled.
#' @name is_int
#' @importFrom checkmate test_int
#' @importFrom purrr map_lgl
#' @export
is_int <- ensure_atomic_boolean('test_int', 'checkmate')
#' 
#' @rdname is_int
#' @export
is_not_int <- function(x, na.ok, lower, upper, tol, null.ok) !is_int(x, na.ok, lower, upper, tol, null.ok)
#' 
#' @rdname is_int
#' @export
are_int <- function(x, na.ok, lower, upper, tol, null.ok) {
      purrr::map_lgl(x, \(x) is_int(x = x, na.ok = na.ok, lower = lower, upper = upper, tol = tol, null.ok = null.ok))
    }
#' 
#' @rdname is_int
#' @export
are_not_int <- function(x, na.ok, lower, upper, tol, null.ok) !are_int(x = x, na.ok = na.ok, lower = lower, upper = upper, tol = tol, null.ok = null.ok)


#' @aliases are_complex, is_not_complex, are_not_complex

#' @title Check if an argument is a vector of type complex
#'
#' @description This is a re-export of \code{\link[checkmate:checkComplex]{checkmate::test_complex()}}, modified to have standardised naming and standardised vector handling.
#' Documentation is atuomatically generated from the original package documentation. See the \code{\link[checkmate:checkComplex]{original}} for full details.
#' 
#' Check if an argument is a vector of type complex.
#' 
#' 
#' @returns 
#' - Calls to \code{is_complex} are guaranteed to return a scalar boolean (ie. a single \code{TRUE} or \code{FALSE} value). If an argument of length > 1 is given, \code{FALSE} is returned.
#' - \code{are_complex} is a wrapper around \code{\link[purrr]{map_lgl}(vec, \(i) is_complex(i, ...))}. A boolean vector of the same length as the input is guaranteed.
#' - Calls to \code{is_not_complex}/\code{are_not_complex} negate the output of \code{is_complex}/\code{are_complex}.
#' @md
#'
#' @param x any\cr
#' Object to check.
#' @param any.missing \code{logical(1)}\cr
#' Are vectors with missing values allowed? Default is \code{TRUE}.
#' @param all.missing \code{logical(1)}\cr
#' Are vectors with no non-missing values allowed? Default is \code{TRUE}.
#' Note that empty vectors do not have non-missing values.
#' @param len \code{integer(1)}\cr
#' Exact expected length of \code{x}.
#' @param min.len \code{integer(1)}\cr
#' Minimal length of \code{x}.
#' @param max.len \code{integer(1)}\cr
#' Maximal length of \code{x}.
#' @param unique \code{logical(1)}\cr
#' Must all values be unique? Default is \code{FALSE}.
#' @param names \code{character(1)}\cr
#' Check for names. See \code{\link{checkNamed}} for possible values.
#' Default is \dQuote{any} which performs no check at all.
#' Note that you can use \code{\link{checkSubset}} to check for a specific set of names.
#' @param typed.missing \code{logical(1)}\cr
#' If set to \code{FALSE} (default), all types of missing values (\code{NA}, \code{NA_integer_},
#' \code{NA_real_}, \code{NA_character_} or \code{NA_character_}) as well as empty vectors are allowed
#' while type-checking atomic input.
#' Set to \code{TRUE} to enable strict type checking.
#' @param null.ok \code{logical(1)}\cr
#' If set to \code{TRUE}, \code{x} may also be \code{NULL}.
#' In this case only a type check of \code{x} is performed, all additional checks are disabled.
#' @name is_complex
#' @importFrom checkmate test_complex
#' @importFrom purrr map_lgl
#' @export
is_complex <- ensure_atomic_boolean('test_complex', 'checkmate')
#' 
#' @rdname is_complex
#' @export
is_not_complex <- function(x, any.missing, all.missing, len, min.len, max.len, unique, names, typed.missing, null.ok) !is_complex(x, any.missing, all.missing, len, min.len, max.len, unique, names, typed.missing, null.ok)
#' 
#' @rdname is_complex
#' @export
are_complex <- function(x, any.missing, all.missing, len, min.len, max.len, unique, names, typed.missing, null.ok) {
      purrr::map_lgl(x, \(x) is_complex(x = x, any.missing = any.missing, all.missing = all.missing, len = len, min.len = min.len, max.len = max.len, unique = unique, names = names, typed.missing = typed.missing, null.ok = null.ok))
    }
#' 
#' @rdname is_complex
#' @export
are_not_complex <- function(x, any.missing, all.missing, len, min.len, max.len, unique, names, typed.missing, null.ok) !are_complex(x = x, any.missing = any.missing, all.missing = all.missing, len = len, min.len = min.len, max.len = max.len, unique = unique, names = names, typed.missing = typed.missing, null.ok = null.ok)


#' @aliases are_vector, is_not_vector, are_not_vector

#' @title Check if an argument is a vector
#'
#' @description This is a re-export of \code{\link[checkmate:checkVector]{checkmate::test_vector()}}, modified to have standardised naming and standardised vector handling.
#' Documentation is atuomatically generated from the original package documentation. See the \code{\link[checkmate:checkVector]{original}} for full details.
#' 
#' Check if an argument is a vector.
#' 
#' 
#' @returns 
#' - Calls to \code{is_vector} are guaranteed to return a scalar boolean (ie. a single \code{TRUE} or \code{FALSE} value). If an argument of length > 1 is given, \code{FALSE} is returned.
#' - \code{are_vector} is a wrapper around \code{\link[purrr]{map_lgl}(vec, \(i) is_vector(i, ...))}. A boolean vector of the same length as the input is guaranteed.
#' - Calls to \code{is_not_vector}/\code{are_not_vector} negate the output of \code{is_vector}/\code{are_vector}.
#' @md
#'
#' @param x any\cr
#' Object to check.
#' @param strict \code{logical(1)}\cr
#' May the vector have additional attributes? If \code{TRUE}, mimics the behavior of
#' \code{\link{is.vector}}.
#' Default is \code{FALSE} which allows e.g. \code{factor}s or \code{data.frame}s
#' to be recognized as vectors.
#' @param any.missing \code{logical(1)}\cr
#' Are vectors with missing values allowed? Default is \code{TRUE}.
#' @param all.missing \code{logical(1)}\cr
#' Are vectors with no non-missing values allowed? Default is \code{TRUE}.
#' Note that empty vectors do not have non-missing values.
#' @param len \code{integer(1)}\cr
#' Exact expected length of \code{x}.
#' @param min.len \code{integer(1)}\cr
#' Minimal length of \code{x}.
#' @param max.len \code{integer(1)}\cr
#' Maximal length of \code{x}.
#' @param unique \code{logical(1)}\cr
#' Must all values be unique? Default is \code{FALSE}.
#' @param names \code{character(1)}\cr
#' Check for names. See \code{\link{checkNamed}} for possible values.
#' Default is \dQuote{any} which performs no check at all.
#' Note that you can use \code{\link{checkSubset}} to check for a specific set of names.
#' @param null.ok \code{logical(1)}\cr
#' If set to \code{TRUE}, \code{x} may also be \code{NULL}.
#' In this case only a type check of \code{x} is performed, all additional checks are disabled.
#' @name is_vector
#' @importFrom checkmate test_vector
#' @importFrom purrr map_lgl
#' @export
is_vector <- ensure_atomic_boolean('test_vector', 'checkmate')
#' 
#' @rdname is_vector
#' @export
is_not_vector <- function(x, strict, any.missing, all.missing, len, min.len, max.len, unique, names, null.ok) !is_vector(x, strict, any.missing, all.missing, len, min.len, max.len, unique, names, null.ok)
#' 
#' @rdname is_vector
#' @export
are_vector <- function(x, strict, any.missing, all.missing, len, min.len, max.len, unique, names, null.ok) {
      purrr::map_lgl(x, \(x) is_vector(x = x, strict = strict, any.missing = any.missing, all.missing = all.missing, len = len, min.len = min.len, max.len = max.len, unique = unique, names = names, null.ok = null.ok))
    }
#' 
#' @rdname is_vector
#' @export
are_not_vector <- function(x, strict, any.missing, all.missing, len, min.len, max.len, unique, names, null.ok) !are_vector(x = x, strict = strict, any.missing = any.missing, all.missing = all.missing, len = len, min.len = min.len, max.len = max.len, unique = unique, names = names, null.ok = null.ok)


#' @aliases are_array, is_not_array, are_not_array

#' @title Check if an argument is an array
#'
#' @description This is a re-export of \code{\link[checkmate:checkArray]{checkmate::test_array()}}, modified to have standardised naming and standardised vector handling.
#' Documentation is atuomatically generated from the original package documentation. See the \code{\link[checkmate:checkArray]{original}} for full details.
#' 
#' Check if an argument is an array.
#' 
#' 
#' @returns 
#' - Calls to \code{is_array} are guaranteed to return a scalar boolean (ie. a single \code{TRUE} or \code{FALSE} value). If an argument of length > 1 is given, \code{FALSE} is returned.
#' - \code{are_array} is a wrapper around \code{\link[purrr]{map_lgl}(vec, \(i) is_array(i, ...))}. A boolean vector of the same length as the input is guaranteed.
#' - Calls to \code{is_not_array}/\code{are_not_array} negate the output of \code{is_array}/\code{are_array}.
#' @md
#'
#' @param x any\cr
#' Object to check.
#' @param mode \code{character(1)}\cr
#' Storage mode of the array. Arrays can hold vectors, i.e. \dQuote{logical},
#' \dQuote{integer}, \dQuote{integerish}, \dQuote{double}, \dQuote{numeric}, \dQuote{complex},
#' \dQuote{character} and \dQuote{list}. You can also specify \dQuote{atomic}
#' here to explicitly prohibit lists. Default is \code{NULL} (no check).
#' If all values of \code{x} are missing, this check is skipped.
#' @param any.missing \code{logical(1)}\cr
#' Are missing values allowed? Default is \code{TRUE}.
#' @param d \code{integer(1)}\cr
#' Exact number of dimensions of array \code{x}.
#' Default is \code{NULL} (no check).
#' @param min.d \code{integer(1)}\cr
#' Minimum number of dimensions of array \code{x}.
#' Default is \code{NULL} (no check).
#' @param max.d \code{integer(1)}\cr
#' Maximum number of dimensions of array \code{x}.
#' Default is \code{NULL} (no check).
#' @param null.ok \code{logical(1)}\cr
#' If set to \code{TRUE}, \code{x} may also be \code{NULL}.
#' In this case only a type check of \code{x} is performed, all additional checks are disabled.
#' @name is_array
#' @importFrom checkmate test_array
#' @importFrom purrr map_lgl
#' @export
is_array <- ensure_atomic_boolean('test_array', 'checkmate')
#' 
#' @rdname is_array
#' @export
is_not_array <- function(x, mode, any.missing, d, min.d, max.d, null.ok) !is_array(x, mode, any.missing, d, min.d, max.d, null.ok)
#' 
#' @rdname is_array
#' @export
are_array <- function(x, mode, any.missing, d, min.d, max.d, null.ok) {
      purrr::map_lgl(x, \(x) is_array(x = x, mode = mode, any.missing = any.missing, d = d, min.d = min.d, max.d = max.d, null.ok = null.ok))
    }
#' 
#' @rdname is_array
#' @export
are_not_array <- function(x, mode, any.missing, d, min.d, max.d, null.ok) !are_array(x = x, mode = mode, any.missing = any.missing, d = d, min.d = min.d, max.d = max.d, null.ok = null.ok)


#' @aliases are_date, is_not_date, are_not_date

#' @title Check that an argument is a Date
#'
#' @description This is a re-export of \code{\link[checkmate:checkDate]{checkmate::test_date()}}, modified to have standardised naming and standardised vector handling.
#' Documentation is atuomatically generated from the original package documentation. See the \code{\link[checkmate:checkDate]{original}} for full details.
#' 
#' Checks that an object is of class \code{\link{Date}}.
#' 
#' 
#' @returns 
#' - Calls to \code{is_date} are guaranteed to return a scalar boolean (ie. a single \code{TRUE} or \code{FALSE} value). If an argument of length > 1 is given, \code{FALSE} is returned.
#' - \code{are_date} is a wrapper around \code{\link[purrr]{map_lgl}(vec, \(i) is_date(i, ...))}. A boolean vector of the same length as the input is guaranteed.
#' - Calls to \code{is_not_date}/\code{are_not_date} negate the output of \code{is_date}/\code{are_date}.
#' @md
#'
#' @param x any\cr
#' Object to check.
#' @param lower \code{\link{Date}}\cr
#' All non-missing dates in \code{x} must be >= this date. Comparison is done via \code{\link{Ops.Date}}.
#' @param upper \code{\link{Date}}\cr
#' All non-missing dates in \code{x} must be before <= this date. Comparison is done via \code{\link{Ops.Date}}.
#' @param any.missing \code{logical(1)}\cr
#' Are vectors with missing values allowed? Default is \code{TRUE}.
#' @param all.missing \code{logical(1)}\cr
#' Are vectors with no non-missing values allowed? Default is \code{TRUE}.
#' Note that empty vectors do not have non-missing values.
#' @param len \code{integer(1)}\cr
#' Exact expected length of \code{x}.
#' @param min.len \code{integer(1)}\cr
#' Minimal length of \code{x}.
#' @param max.len \code{integer(1)}\cr
#' Maximal length of \code{x}.
#' @param unique \code{logical(1)}\cr
#' Must all values be unique? Default is \code{FALSE}.
#' @param null.ok \code{logical(1)}\cr
#' If set to \code{TRUE}, \code{x} may also be \code{NULL}.
#' In this case only a type check of \code{x} is performed, all additional checks are disabled.
#' @name is_date
#' @importFrom checkmate test_date
#' @importFrom purrr map_lgl
#' @export
is_date <- ensure_atomic_boolean('test_date', 'checkmate')
#' 
#' @rdname is_date
#' @export
is_not_date <- function(x, lower, upper, any.missing, all.missing, len, min.len, max.len, unique, null.ok) !is_date(x, lower, upper, any.missing, all.missing, len, min.len, max.len, unique, null.ok)
#' 
#' @rdname is_date
#' @export
are_date <- function(x, lower, upper, any.missing, all.missing, len, min.len, max.len, unique, null.ok) {
      purrr::map_lgl(x, \(x) is_date(x = x, lower = lower, upper = upper, any.missing = any.missing, all.missing = all.missing, len = len, min.len = min.len, max.len = max.len, unique = unique, null.ok = null.ok))
    }
#' 
#' @rdname is_date
#' @export
are_not_date <- function(x, lower, upper, any.missing, all.missing, len, min.len, max.len, unique, null.ok) !are_date(x = x, lower = lower, upper = upper, any.missing = any.missing, all.missing = all.missing, len = len, min.len = min.len, max.len = max.len, unique = unique, null.ok = null.ok)


#' @aliases are_raw, is_not_raw, are_not_raw

#' @title Check if an argument is a raw vector
#'
#' @description This is a re-export of \code{\link[checkmate:checkRaw]{checkmate::test_raw()}}, modified to have standardised naming and standardised vector handling.
#' Documentation is atuomatically generated from the original package documentation. See the \code{\link[checkmate:checkRaw]{original}} for full details.
#' 
#' Check if an argument is a raw vector.
#' 
#' 
#' @returns 
#' - Calls to \code{is_raw} are guaranteed to return a scalar boolean (ie. a single \code{TRUE} or \code{FALSE} value). If an argument of length > 1 is given, \code{FALSE} is returned.
#' - \code{are_raw} is a wrapper around \code{\link[purrr]{map_lgl}(vec, \(i) is_raw(i, ...))}. A boolean vector of the same length as the input is guaranteed.
#' - Calls to \code{is_not_raw}/\code{are_not_raw} negate the output of \code{is_raw}/\code{are_raw}.
#' @md
#'
#' @param x any\cr
#' Object to check.
#' @param len \code{integer(1)}\cr
#' Exact expected length of \code{x}.
#' @param min.len \code{integer(1)}\cr
#' Minimal length of \code{x}.
#' @param max.len \code{integer(1)}\cr
#' Maximal length of \code{x}.
#' @param names \code{character(1)}\cr
#' Check for names. See \code{\link{checkNamed}} for possible values.
#' Default is \dQuote{any} which performs no check at all.
#' Note that you can use \code{\link{checkSubset}} to check for a specific set of names.
#' @param null.ok \code{logical(1)}\cr
#' If set to \code{TRUE}, \code{x} may also be \code{NULL}.
#' In this case only a type check of \code{x} is performed, all additional checks are disabled.
#' @name is_raw
#' @importFrom checkmate test_raw
#' @importFrom purrr map_lgl
#' @export
is_raw <- ensure_atomic_boolean('test_raw', 'checkmate')
#' 
#' @rdname is_raw
#' @export
is_not_raw <- function(x, len, min.len, max.len, names, null.ok) !is_raw(x, len, min.len, max.len, names, null.ok)
#' 
#' @rdname is_raw
#' @export
are_raw <- function(x, len, min.len, max.len, names, null.ok) {
      purrr::map_lgl(x, \(x) is_raw(x = x, len = len, min.len = min.len, max.len = max.len, names = names, null.ok = null.ok))
    }
#' 
#' @rdname is_raw
#' @export
are_not_raw <- function(x, len, min.len, max.len, names, null.ok) !are_raw(x = x, len = len, min.len = min.len, max.len = max.len, names = names, null.ok = null.ok)


#' @aliases are_count, is_not_count, are_not_count

#' @title Check if an argument is a count
#'
#' @description This is a re-export of \code{\link[checkmate:checkCount]{checkmate::test_count()}}, modified to have standardised naming and standardised vector handling.
#' Documentation is atuomatically generated from the original package documentation. See the \code{\link[checkmate:checkCount]{original}} for full details.
#' 
#' A count is defined as non-negative integerish value.
#' 
#' 
#' @returns 
#' - Calls to \code{is_count} are guaranteed to return a scalar boolean (ie. a single \code{TRUE} or \code{FALSE} value). If an argument of length > 1 is given, \code{FALSE} is returned.
#' - \code{are_count} is a wrapper around \code{\link[purrr]{map_lgl}(vec, \(i) is_count(i, ...))}. A boolean vector of the same length as the input is guaranteed.
#' - Calls to \code{is_not_count}/\code{are_not_count} negate the output of \code{is_count}/\code{are_count}.
#' @md
#'
#' @param x any\cr
#' Object to check.
#' @param na.ok \code{logical(1)}\cr
#' Are missing values allowed? Default is \code{FALSE}.
#' @param positive \code{logical(1)}\cr
#' Must \code{x} be positive (>= 1)?
#' Default is \code{FALSE}, allowing 0.
#' @param tol \code{double(1)}\cr
#' Numerical tolerance used to check whether a double or complex can be converted.
#' Default is \code{sqrt(.Machine$double.eps)}.
#' @param null.ok \code{logical(1)}\cr
#' If set to \code{TRUE}, \code{x} may also be \code{NULL}.
#' In this case only a type check of \code{x} is performed, all additional checks are disabled.
#' @name is_count
#' @importFrom checkmate test_count
#' @importFrom purrr map_lgl
#' @export
is_count <- ensure_atomic_boolean('test_count', 'checkmate')
#' 
#' @rdname is_count
#' @export
is_not_count <- function(x, na.ok, positive, tol, null.ok) !is_count(x, na.ok, positive, tol, null.ok)
#' 
#' @rdname is_count
#' @export
are_count <- function(x, na.ok, positive, tol, null.ok) {
      purrr::map_lgl(x, \(x) is_count(x = x, na.ok = na.ok, positive = positive, tol = tol, null.ok = null.ok))
    }
#' 
#' @rdname is_count
#' @export
are_not_count <- function(x, na.ok, positive, tol, null.ok) !are_count(x = x, na.ok = na.ok, positive = positive, tol = tol, null.ok = null.ok)


#' @aliases are_integer, is_not_integer, are_not_integer

#' @title Check if an argument is vector of type integer
#'
#' @description This is a re-export of \code{\link[checkmate:checkInteger]{checkmate::test_integer()}}, modified to have standardised naming and standardised vector handling.
#' Documentation is atuomatically generated from the original package documentation. See the \code{\link[checkmate:checkInteger]{original}} for full details.
#' 
#' Check if an argument is vector of type integer.
#' 
#' 
#' @returns 
#' - Calls to \code{is_integer} are guaranteed to return a scalar boolean (ie. a single \code{TRUE} or \code{FALSE} value). If an argument of length > 1 is given, \code{FALSE} is returned.
#' - \code{are_integer} is a wrapper around \code{\link[purrr]{map_lgl}(vec, \(i) is_integer(i, ...))}. A boolean vector of the same length as the input is guaranteed.
#' - Calls to \code{is_not_integer}/\code{are_not_integer} negate the output of \code{is_integer}/\code{are_integer}.
#' @md
#'
#' @param x any\cr
#' Object to check.
#' @param lower \code{numeric(1)}\cr
#' Lower value all elements of \code{x} must be greater than or equal to.
#' @param upper \code{numeric(1)}\cr
#' Upper value all elements of \code{x} must be lower than or equal to.
#' @param any.missing \code{logical(1)}\cr
#' Are vectors with missing values allowed? Default is \code{TRUE}.
#' @param all.missing \code{logical(1)}\cr
#' Are vectors with no non-missing values allowed? Default is \code{TRUE}.
#' Note that empty vectors do not have non-missing values.
#' @param len \code{integer(1)}\cr
#' Exact expected length of \code{x}.
#' @param min.len \code{integer(1)}\cr
#' Minimal length of \code{x}.
#' @param max.len \code{integer(1)}\cr
#' Maximal length of \code{x}.
#' @param unique \code{logical(1)}\cr
#' Must all values be unique? Default is \code{FALSE}.
#' @param sorted \code{logical(1)}\cr
#' Elements must be sorted in ascending order. Missing values are ignored.
#' @param names \code{character(1)}\cr
#' Check for names. See \code{\link{checkNamed}} for possible values.
#' Default is \dQuote{any} which performs no check at all.
#' Note that you can use \code{\link{checkSubset}} to check for a specific set of names.
#' @param typed.missing \code{logical(1)}\cr
#' If set to \code{FALSE} (default), all types of missing values (\code{NA}, \code{NA_integer_},
#' \code{NA_real_}, \code{NA_character_} or \code{NA_character_}) as well as empty vectors are allowed
#' while type-checking atomic input.
#' Set to \code{TRUE} to enable strict type checking.
#' @param null.ok \code{logical(1)}\cr
#' If set to \code{TRUE}, \code{x} may also be \code{NULL}.
#' In this case only a type check of \code{x} is performed, all additional checks are disabled.
#' @name is_integer
#' @importFrom checkmate test_integer
#' @importFrom purrr map_lgl
#' @export
is_integer <- ensure_atomic_boolean('test_integer', 'checkmate')
#' 
#' @rdname is_integer
#' @export
is_not_integer <- function(x, lower, upper, any.missing, all.missing, len, min.len, max.len, unique, sorted, names, typed.missing, null.ok) !is_integer(x, lower, upper, any.missing, all.missing, len, min.len, max.len, unique, sorted, names, typed.missing, null.ok)
#' 
#' @rdname is_integer
#' @export
are_integer <- function(x, lower, upper, any.missing, all.missing, len, min.len, max.len, unique, sorted, names, typed.missing, null.ok) {
      purrr::map_lgl(x, \(x) is_integer(x = x, lower = lower, upper = upper, any.missing = any.missing, all.missing = all.missing, len = len, min.len = min.len, max.len = max.len, unique = unique, sorted = sorted, names = names, typed.missing = typed.missing, null.ok = null.ok))
    }
#' 
#' @rdname is_integer
#' @export
are_not_integer <- function(x, lower, upper, any.missing, all.missing, len, min.len, max.len, unique, sorted, names, typed.missing, null.ok) !are_integer(x = x, lower = lower, upper = upper, any.missing = any.missing, all.missing = all.missing, len = len, min.len = min.len, max.len = max.len, unique = unique, sorted = sorted, names = names, typed.missing = typed.missing, null.ok = null.ok)


#' @aliases are_data_table, is_not_data_table, are_not_data_table

#' @title Check if an argument is a data table
#'
#' @description This is a re-export of \code{\link[checkmate:checkDataTable]{checkmate::test_data_table()}}, modified to have standardised naming and standardised vector handling.
#' Documentation is atuomatically generated from the original package documentation. See the \code{\link[checkmate:checkDataTable]{original}} for full details.
#' 
#' Check if an argument is a data table.
#' 
#' 
#' @returns 
#' - Calls to \code{is_data_table} are guaranteed to return a scalar boolean (ie. a single \code{TRUE} or \code{FALSE} value). If an argument of length > 1 is given, \code{FALSE} is returned.
#' - \code{are_data_table} is a wrapper around \code{\link[purrr]{map_lgl}(vec, \(i) is_data_table(i, ...))}. A boolean vector of the same length as the input is guaranteed.
#' - Calls to \code{is_not_data_table}/\code{are_not_data_table} negate the output of \code{is_data_table}/\code{are_data_table}.
#' @md
#'
#' @param x any\cr
#' Object to check.
#' @param key \code{character}\cr
#' Expected primary key(s) of the data table.
#' @param index \code{character}\cr
#' Expected secondary key(s) of the data table.
#' @param types \code{character}\cr
#' Character vector of class names. Each list element must inherit
#' from at least one of the provided types.
#' The types \dQuote{logical}, \dQuote{integer}, \dQuote{integerish}, \dQuote{double},
#' \dQuote{numeric}, \dQuote{complex}, \dQuote{character}, \dQuote{factor}, \dQuote{atomic}, \dQuote{vector}
#' \dQuote{atomicvector}, \dQuote{array}, \dQuote{matrix}, \dQuote{list}, \dQuote{function},
#' \dQuote{environment} and \dQuote{null} are supported.
#' For other types \code{\link{inherits}} is used as a fallback to check \code{x}'s inheritance.
#' Defaults to \code{character(0)} (no check).
#' @param any.missing \code{logical(1)}\cr
#' Are missing values allowed? Default is \code{TRUE}.
#' @param all.missing \code{logical(1)}\cr
#' Are matrices with only missing values allowed? Default is \code{TRUE}.
#' @param min.rows \code{integer(1)}\cr
#' Minimum number of rows.
#' @param max.rows \code{integer(1)}\cr
#' Maximum number of rows.
#' @param min.cols \code{integer(1)}\cr
#' Minimum number of columns.
#' @param max.cols \code{integer(1)}\cr
#' Maximum number of columns.
#' @param nrows \code{integer(1)}\cr
#' Exact number of rows.
#' @param ncols \code{integer(1)}\cr
#' Exact number of columns.
#' @param row.names \code{character(1)}\cr
#' Check for row names. Default is \dQuote{NULL} (no check).
#' See \code{\link{checkNamed}} for possible values.
#' Note that you can use \code{\link{checkSubset}} to check for a specific set of names.
#' @param col.names \code{character(1)}\cr
#' Check for column names. Default is \dQuote{NULL} (no check).
#' See \code{\link{checkNamed}} for possible values.
#' Note that you can use \code{\link{checkSubset}} to test for a specific set of names.
#' @param null.ok \code{logical(1)}\cr
#' If set to \code{TRUE}, \code{x} may also be \code{NULL}.
#' In this case only a type check of \code{x} is performed, all additional checks are disabled.
#' @name is_data_table
#' @importFrom checkmate test_data_table
#' @importFrom purrr map_lgl
#' @export
is_data_table <- ensure_atomic_boolean('test_data_table', 'checkmate')
#' 
#' @rdname is_data_table
#' @export
is_not_data_table <- function(x, key, index, types, any.missing, all.missing, min.rows, max.rows, min.cols, max.cols, nrows, ncols, row.names, col.names, null.ok) !is_data_table(x, key, index, types, any.missing, all.missing, min.rows, max.rows, min.cols, max.cols, nrows, ncols, row.names, col.names, null.ok)
#' 
#' @rdname is_data_table
#' @export
are_data_table <- function(x, key, index, types, any.missing, all.missing, min.rows, max.rows, min.cols, max.cols, nrows, ncols, row.names, col.names, null.ok) {
      purrr::map_lgl(x, \(x) is_data_table(x = x, key = key, index = index, types = types, any.missing = any.missing, all.missing = all.missing, min.rows = min.rows, max.rows = max.rows, min.cols = min.cols, max.cols = max.cols, nrows = nrows, ncols = ncols, row.names = row.names, col.names = col.names, null.ok = null.ok))
    }
#' 
#' @rdname is_data_table
#' @export
are_not_data_table <- function(x, key, index, types, any.missing, all.missing, min.rows, max.rows, min.cols, max.cols, nrows, ncols, row.names, col.names, null.ok) !are_data_table(x = x, key = key, index = index, types = types, any.missing = any.missing, all.missing = all.missing, min.rows = min.rows, max.rows = max.rows, min.cols = min.cols, max.cols = max.cols, nrows = nrows, ncols = ncols, row.names = row.names, col.names = col.names, null.ok = null.ok)


#' @aliases are_subset, is_not_subset, are_not_subset

#' @title Check if an argument is a subset of a given set
#'
#' @description This is a re-export of \code{\link[checkmate:checkSubset]{checkmate::test_subset()}}, modified to have standardised naming and standardised vector handling.
#' Documentation is atuomatically generated from the original package documentation. See the \code{\link[checkmate:checkSubset]{original}} for full details.
#' 
#' Check if an argument is a subset of a given set.
#' 
#' 
#' @returns 
#' - Calls to \code{is_subset} are guaranteed to return a scalar boolean (ie. a single \code{TRUE} or \code{FALSE} value). If an argument of length > 1 is given, \code{FALSE} is returned.
#' - \code{are_subset} is a wrapper around \code{\link[purrr]{map_lgl}(vec, \(i) is_subset(i, ...))}. A boolean vector of the same length as the input is guaranteed.
#' - Calls to \code{is_not_subset}/\code{are_not_subset} negate the output of \code{is_subset}/\code{are_subset}.
#' @md
#'
#' @param x any\cr
#' Object to check.
#' @param choices \code{atomic}\cr
#' Set of possible values. May be empty.
#' @param empty.ok \code{logical(1)}\cr
#' Treat zero-length \code{x} as subset of any set \code{choices} (this includes \code{NULL})?
#' Default is \code{TRUE}.
#' @param fmatch \code{logical(1)}\cr
#' Use the set operations implemented in \code{\link{fmatch}} in package \pkg{fastmatch}.
#' If \pkg{fastmatch} is not installed, this silently falls back to \code{\link{match}}.
#' \code{\link{fmatch}} modifies \code{y} by reference:
#' A hash table is added as attribute which is used in subsequent calls.
#' @name is_subset
#' @importFrom checkmate test_subset
#' @importFrom purrr map_lgl
#' @export
is_subset <- ensure_atomic_boolean('test_subset', 'checkmate')
#' 
#' @rdname is_subset
#' @export
is_not_subset <- function(x, choices, empty.ok, fmatch) !is_subset(x, choices, empty.ok, fmatch)
#' 
#' @rdname is_subset
#' @export
are_subset <- function(x, choices, empty.ok, fmatch) {
      purrr::map_lgl(x, \(x) is_subset(x = x, choices = choices, empty.ok = empty.ok, fmatch = fmatch))
    }
#' 
#' @rdname is_subset
#' @export
are_not_subset <- function(x, choices, empty.ok, fmatch) !are_subset(x = x, choices = choices, empty.ok = empty.ok, fmatch = fmatch)


#' @aliases are_names, is_not_names, are_not_names

#' @title Check names to comply to specific rules
#'
#' @description This is a re-export of \code{\link[checkmate:checkNames]{checkmate::test_names()}}, modified to have standardised naming and standardised vector handling.
#' Documentation is atuomatically generated from the original package documentation. See the \code{\link[checkmate:checkNames]{original}} for full details.
#' 
#' Performs various checks on character vectors, usually names.
#' 
#' 
#' @returns 
#' - Calls to \code{is_names} are guaranteed to return a scalar boolean (ie. a single \code{TRUE} or \code{FALSE} value). If an argument of length > 1 is given, \code{FALSE} is returned.
#' - \code{are_names} is a wrapper around \code{\link[purrr]{map_lgl}(vec, \(i) is_names(i, ...))}. A boolean vector of the same length as the input is guaranteed.
#' - Calls to \code{is_not_names}/\code{are_not_names} negate the output of \code{is_names}/\code{are_names}.
#' @md
#'
#' @param x \code{character} || \code{NULL}\cr
#' Names to check using rules defined via \code{type}.
#' @param type character(1)\cr
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
#' @param subset.of \code{character}\cr
#' Names provided in \code{x} must be subset of the set \code{subset.of}.
#' @param must.include \code{character}\cr
#' Names provided in \code{x} must be a superset of the set \code{must.include}.
#' @param permutation.of \code{character}\cr
#' Names provided in \code{x} must be a permutation of the set \code{permutation.of}.
#' Duplicated names in \code{permutation.of} are stripped out and duplicated names in \code{x}
#' thus lead to a failed check.
#' Use this argument instead of \code{identical.to} if the order of the names is not relevant.
#' @param identical.to \code{character}\cr
#' Names provided in \code{x} must be identical to the vector \code{identical.to}.
#' Use this argument instead of \code{permutation.of} if the order of the names is relevant.
#' @param disjunct.from \code{character}\cr
#' Names provided in \code{x} must may not be present in the vector \code{disjunct.from}.
#' @param what \code{character(1)}\cr
#' Type of name vector to check, e.g. \dQuote{names} (default), \dQuote{colnames} or \dQuote{rownames}.
#' @name is_names
#' @importFrom checkmate test_names
#' @importFrom purrr map_lgl
#' @export
is_names <- ensure_atomic_boolean('test_names', 'checkmate')
#' 
#' @rdname is_names
#' @export
is_not_names <- function(x, type, subset.of, must.include, permutation.of, identical.to, disjunct.from, what) !is_names(x, type, subset.of, must.include, permutation.of, identical.to, disjunct.from, what)
#' 
#' @rdname is_names
#' @export
are_names <- function(x, type, subset.of, must.include, permutation.of, identical.to, disjunct.from, what) {
      purrr::map_lgl(x, \(x) is_names(x = x, type = type, subset.of = subset.of, must.include = must.include, permutation.of = permutation.of, identical.to = identical.to, disjunct.from = disjunct.from, what = what))
    }
#' 
#' @rdname is_names
#' @export
are_not_names <- function(x, type, subset.of, must.include, permutation.of, identical.to, disjunct.from, what) !are_names(x = x, type = type, subset.of = subset.of, must.include = must.include, permutation.of = permutation.of, identical.to = identical.to, disjunct.from = disjunct.from, what = what)


#' @aliases are_set_equal, is_not_set_equal, are_not_set_equal

#' @title Check if an argument is equal to a given set
#'
#' @description This is a re-export of \code{\link[checkmate:checkSetEqual]{checkmate::test_set_equal()}}, modified to have standardised naming and standardised vector handling.
#' Documentation is atuomatically generated from the original package documentation. See the \code{\link[checkmate:checkSetEqual]{original}} for full details.
#' 
#' Check if an argument is equal to a given set.
#' 
#' 
#' @returns 
#' - Calls to \code{is_set_equal} are guaranteed to return a scalar boolean (ie. a single \code{TRUE} or \code{FALSE} value). If an argument of length > 1 is given, \code{FALSE} is returned.
#' - \code{are_set_equal} is a wrapper around \code{\link[purrr]{map_lgl}(vec, \(i) is_set_equal(i, ...))}. A boolean vector of the same length as the input is guaranteed.
#' - Calls to \code{is_not_set_equal}/\code{are_not_set_equal} negate the output of \code{is_set_equal}/\code{are_set_equal}.
#' @md
#'
#' @param x any\cr
#' Object to check.
#' @param y \code{atomic}\cr
#' Set to compare with.
#' @param ordered \code{logical(1)}\cr
#' Check \code{x} to have the same length and order as \code{y}, i.e.
#' check using \dQuote{==} while handling \code{NA}s nicely.
#' Default is \code{FALSE}.
#' @param fmatch \code{logical(1)}\cr
#' Use the set operations implemented in \code{\link{fmatch}} in package \pkg{fastmatch}.
#' If \pkg{fastmatch} is not installed, this silently falls back to \code{\link{match}}.
#' \code{\link{fmatch}} modifies \code{y} by reference:
#' A hash table is added as attribute which is used in subsequent calls.
#' @name is_set_equal
#' @importFrom checkmate test_set_equal
#' @importFrom purrr map_lgl
#' @export
is_set_equal <- ensure_atomic_boolean('test_set_equal', 'checkmate')
#' 
#' @rdname is_set_equal
#' @export
is_not_set_equal <- function(x, y, ordered, fmatch) !is_set_equal(x, y, ordered, fmatch)
#' 
#' @rdname is_set_equal
#' @export
are_set_equal <- function(x, y, ordered, fmatch) {
      purrr::map_lgl(x, \(x) is_set_equal(x = x, y = y, ordered = ordered, fmatch = fmatch))
    }
#' 
#' @rdname is_set_equal
#' @export
are_not_set_equal <- function(x, y, ordered, fmatch) !are_set_equal(x = x, y = y, ordered = ordered, fmatch = fmatch)


#' @aliases are_double, is_not_double, are_not_double

#' @title Check that an argument is a vector of type double
#'
#' @description This is a re-export of \code{\link[checkmate:checkDouble]{checkmate::test_double()}}, modified to have standardised naming and standardised vector handling.
#' Documentation is atuomatically generated from the original package documentation. See the \code{\link[checkmate:checkDouble]{original}} for full details.
#' 
#' Check that an argument is a vector of type double.
#' 
#' 
#' @returns 
#' - Calls to \code{is_double} are guaranteed to return a scalar boolean (ie. a single \code{TRUE} or \code{FALSE} value). If an argument of length > 1 is given, \code{FALSE} is returned.
#' - \code{are_double} is a wrapper around \code{\link[purrr]{map_lgl}(vec, \(i) is_double(i, ...))}. A boolean vector of the same length as the input is guaranteed.
#' - Calls to \code{is_not_double}/\code{are_not_double} negate the output of \code{is_double}/\code{are_double}.
#' @md
#'
#' @param x any\cr
#' Object to check.
#' @param lower \code{numeric(1)}\cr
#' Lower value all elements of \code{x} must be greater than or equal to.
#' @param upper \code{numeric(1)}\cr
#' Upper value all elements of \code{x} must be lower than or equal to.
#' @param finite \code{logical(1)}\cr
#' Check for only finite values? Default is \code{FALSE}.
#' @param any.missing \code{logical(1)}\cr
#' Are vectors with missing values allowed? Default is \code{TRUE}.
#' @param all.missing \code{logical(1)}\cr
#' Are vectors with no non-missing values allowed? Default is \code{TRUE}.
#' Note that empty vectors do not have non-missing values.
#' @param len \code{integer(1)}\cr
#' Exact expected length of \code{x}.
#' @param min.len \code{integer(1)}\cr
#' Minimal length of \code{x}.
#' @param max.len \code{integer(1)}\cr
#' Maximal length of \code{x}.
#' @param unique \code{logical(1)}\cr
#' Must all values be unique? Default is \code{FALSE}.
#' @param sorted \code{logical(1)}\cr
#' Elements must be sorted in ascending order. Missing values are ignored.
#' @param names \code{character(1)}\cr
#' Check for names. See \code{\link{checkNamed}} for possible values.
#' Default is \dQuote{any} which performs no check at all.
#' Note that you can use \code{\link{checkSubset}} to check for a specific set of names.
#' @param typed.missing \code{logical(1)}\cr
#' If set to \code{FALSE} (default), all types of missing values (\code{NA}, \code{NA_integer_},
#' \code{NA_real_}, \code{NA_character_} or \code{NA_character_}) as well as empty vectors are allowed
#' while type-checking atomic input.
#' Set to \code{TRUE} to enable strict type checking.
#' @param null.ok \code{logical(1)}\cr
#' If set to \code{TRUE}, \code{x} may also be \code{NULL}.
#' In this case only a type check of \code{x} is performed, all additional checks are disabled.
#' @name is_double
#' @importFrom checkmate test_double
#' @importFrom purrr map_lgl
#' @export
is_double <- ensure_atomic_boolean('test_double', 'checkmate')
#' 
#' @rdname is_double
#' @export
is_not_double <- function(x, lower, upper, finite, any.missing, all.missing, len, min.len, max.len, unique, sorted, names, typed.missing, null.ok) !is_double(x, lower, upper, finite, any.missing, all.missing, len, min.len, max.len, unique, sorted, names, typed.missing, null.ok)
#' 
#' @rdname is_double
#' @export
are_double <- function(x, lower, upper, finite, any.missing, all.missing, len, min.len, max.len, unique, sorted, names, typed.missing, null.ok) {
      purrr::map_lgl(x, \(x) is_double(x = x, lower = lower, upper = upper, finite = finite, any.missing = any.missing, all.missing = all.missing, len = len, min.len = min.len, max.len = max.len, unique = unique, sorted = sorted, names = names, typed.missing = typed.missing, null.ok = null.ok))
    }
#' 
#' @rdname is_double
#' @export
are_not_double <- function(x, lower, upper, finite, any.missing, all.missing, len, min.len, max.len, unique, sorted, names, typed.missing, null.ok) !are_double(x = x, lower = lower, upper = upper, finite = finite, any.missing = any.missing, all.missing = all.missing, len = len, min.len = min.len, max.len = max.len, unique = unique, sorted = sorted, names = names, typed.missing = typed.missing, null.ok = null.ok)


#' @aliases are_true, is_not_true, are_not_true

#' @title Check if an argument is TRUE
#'
#' @description This is a re-export of \code{\link[checkmate:checkTRUE]{checkmate::test_true()}}, modified to have standardised naming and standardised vector handling.
#' Documentation is atuomatically generated from the original package documentation. See the \code{\link[checkmate:checkTRUE]{original}} for full details.
#' 
#' Simply checks if an argument is \code{TRUE}.
#' 
#' 
#' @returns 
#' - Calls to \code{is_true} are guaranteed to return a scalar boolean (ie. a single \code{TRUE} or \code{FALSE} value). If an argument of length > 1 is given, \code{FALSE} is returned.
#' - \code{are_true} is a wrapper around \code{\link[purrr]{map_lgl}(vec, \(i) is_true(i, ...))}. A boolean vector of the same length as the input is guaranteed.
#' - Calls to \code{is_not_true}/\code{are_not_true} negate the output of \code{is_true}/\code{are_true}.
#' @md
#'
#' @param x any\cr
#' Object to check.
#' @param na.ok \code{logical(1)}\cr
#' Are missing values allowed? Default is \code{FALSE}.
#' @name is_true
#' @importFrom checkmate test_true
#' @importFrom purrr map_lgl
#' @export
is_true <- ensure_atomic_boolean('test_true', 'checkmate')
#' 
#' @rdname is_true
#' @export
is_not_true <- function(x, na.ok) !is_true(x, na.ok)
#' 
#' @rdname is_true
#' @export
are_true <- function(x, na.ok) {
      purrr::map_lgl(x, \(x) is_true(x = x, na.ok = na.ok))
    }
#' 
#' @rdname is_true
#' @export
are_not_true <- function(x, na.ok) !are_true(x = x, na.ok = na.ok)


#' @aliases are_multi_class, is_not_multi_class, are_not_multi_class

#' @title Check the class membership of an argument
#'
#' @description This is a re-export of \code{\link[checkmate:checkMultiClass]{checkmate::test_multi_class()}}, modified to have standardised naming and standardised vector handling.
#' Documentation is atuomatically generated from the original package documentation. See the \code{\link[checkmate:checkMultiClass]{original}} for full details.
#' 
#' Check the class membership of an argument.
#' 
#' 
#' @returns 
#' - Calls to \code{is_multi_class} are guaranteed to return a scalar boolean (ie. a single \code{TRUE} or \code{FALSE} value). If an argument of length > 1 is given, \code{FALSE} is returned.
#' - \code{are_multi_class} is a wrapper around \code{\link[purrr]{map_lgl}(vec, \(i) is_multi_class(i, ...))}. A boolean vector of the same length as the input is guaranteed.
#' - Calls to \code{is_not_multi_class}/\code{are_not_multi_class} negate the output of \code{is_multi_class}/\code{are_multi_class}.
#' @md
#'
#' @param x any\cr
#' Object to check.
#' @param classes \code{character}\cr
#' Class names to check for inheritance with \code{\link{inherits}}.
#' \code{x} must inherit from any of the specified classes.
#' @param null.ok \code{logical(1)}\cr
#' If set to \code{TRUE}, \code{x} may also be \code{NULL}.
#' In this case only a type check of \code{x} is performed, all additional checks are disabled.
#' @name is_multi_class
#' @importFrom checkmate test_multi_class
#' @importFrom purrr map_lgl
#' @export
is_multi_class <- ensure_atomic_boolean('test_multi_class', 'checkmate')
#' 
#' @rdname is_multi_class
#' @export
is_not_multi_class <- function(x, classes, null.ok) !is_multi_class(x, classes, null.ok)
#' 
#' @rdname is_multi_class
#' @export
are_multi_class <- function(x, classes, null.ok) {
      purrr::map_lgl(x, \(x) is_multi_class(x = x, classes = classes, null.ok = null.ok))
    }
#' 
#' @rdname is_multi_class
#' @export
are_not_multi_class <- function(x, classes, null.ok) !are_multi_class(x = x, classes = classes, null.ok = null.ok)


#' @aliases are_null, is_not_null, are_not_null

#' @title Check if an argument is NULL
#'
#' @description This is a re-export of \code{\link[checkmate:checkNull]{checkmate::test_null()}}, modified to have standardised naming and standardised vector handling.
#' Documentation is atuomatically generated from the original package documentation. See the \code{\link[checkmate:checkNull]{original}} for full details.
#' 
#' Check if an argument is NULL.
#' 
#' 
#' @returns 
#' - Calls to \code{is_null} are guaranteed to return a scalar boolean (ie. a single \code{TRUE} or \code{FALSE} value). If an argument of length > 1 is given, \code{FALSE} is returned.
#' - \code{are_null} is a wrapper around \code{\link[purrr]{map_lgl}(vec, \(i) is_null(i, ...))}. A boolean vector of the same length as the input is guaranteed.
#' - Calls to \code{is_not_null}/\code{are_not_null} negate the output of \code{is_null}/\code{are_null}.
#' @md
#'
#' @param x any\cr
#' Object to check.
#' @name is_null
#' @importFrom checkmate test_null
#' @importFrom purrr map_lgl
#' @export
is_null <- ensure_atomic_boolean('test_null', 'checkmate')
#' 
#' @rdname is_null
#' @export
is_not_null <- function(x) !is_null(x)
#' 
#' @rdname is_null
#' @export
are_null <- function(x) {
      purrr::map_lgl(x, \(x) is_null(x = x))
    }
#' 
#' @rdname is_null
#' @export
are_not_null <- function(x) !are_null(x = x)


#' @aliases are_scalar, is_not_scalar, are_not_scalar

#' @title Check if an argument is a single atomic value
#'
#' @description This is a re-export of \code{\link[checkmate:checkScalar]{checkmate::test_scalar()}}, modified to have standardised naming and standardised vector handling.
#' Documentation is atuomatically generated from the original package documentation. See the \code{\link[checkmate:checkScalar]{original}} for full details.
#' 
#' Check if an argument is a single atomic value.
#' 
#' 
#' @returns 
#' - Calls to \code{is_scalar} are guaranteed to return a scalar boolean (ie. a single \code{TRUE} or \code{FALSE} value). If an argument of length > 1 is given, \code{FALSE} is returned.
#' - \code{are_scalar} is a wrapper around \code{\link[purrr]{map_lgl}(vec, \(i) is_scalar(i, ...))}. A boolean vector of the same length as the input is guaranteed.
#' - Calls to \code{is_not_scalar}/\code{are_not_scalar} negate the output of \code{is_scalar}/\code{are_scalar}.
#' @md
#'
#' @param x any\cr
#' Object to check.
#' @param na.ok \code{logical(1)}\cr
#' Are missing values allowed? Default is \code{FALSE}.
#' @param null.ok \code{logical(1)}\cr
#' If set to \code{TRUE}, \code{x} may also be \code{NULL}.
#' In this case only a type check of \code{x} is performed, all additional checks are disabled.
#' @name is_scalar
#' @importFrom checkmate test_scalar
#' @importFrom purrr map_lgl
#' @export
is_scalar <- ensure_atomic_boolean('test_scalar', 'checkmate')
#' 
#' @rdname is_scalar
#' @export
is_not_scalar <- function(x, na.ok, null.ok) !is_scalar(x, na.ok, null.ok)
#' 
#' @rdname is_scalar
#' @export
are_scalar <- function(x, na.ok, null.ok) {
      purrr::map_lgl(x, \(x) is_scalar(x = x, na.ok = na.ok, null.ok = null.ok))
    }
#' 
#' @rdname is_scalar
#' @export
are_not_scalar <- function(x, na.ok, null.ok) !are_scalar(x = x, na.ok = na.ok, null.ok = null.ok)


#' @aliases are_false, is_not_false, are_not_false

#' @title Check if an argument is FALSE
#'
#' @description This is a re-export of \code{\link[checkmate:checkFALSE]{checkmate::test_false()}}, modified to have standardised naming and standardised vector handling.
#' Documentation is atuomatically generated from the original package documentation. See the \code{\link[checkmate:checkFALSE]{original}} for full details.
#' 
#' Simply checks if an argument is \code{FALSE}.
#' 
#' 
#' @returns 
#' - Calls to \code{is_false} are guaranteed to return a scalar boolean (ie. a single \code{TRUE} or \code{FALSE} value). If an argument of length > 1 is given, \code{FALSE} is returned.
#' - \code{are_false} is a wrapper around \code{\link[purrr]{map_lgl}(vec, \(i) is_false(i, ...))}. A boolean vector of the same length as the input is guaranteed.
#' - Calls to \code{is_not_false}/\code{are_not_false} negate the output of \code{is_false}/\code{are_false}.
#' @md
#'
#' @param x any\cr
#' Object to check.
#' @param na.ok \code{logical(1)}\cr
#' Are missing values allowed? Default is \code{FALSE}.
#' @name is_false
#' @importFrom checkmate test_false
#' @importFrom purrr map_lgl
#' @export
is_false <- ensure_atomic_boolean('test_false', 'checkmate')
#' 
#' @rdname is_false
#' @export
is_not_false <- function(x, na.ok) !is_false(x, na.ok)
#' 
#' @rdname is_false
#' @export
are_false <- function(x, na.ok) {
      purrr::map_lgl(x, \(x) is_false(x = x, na.ok = na.ok))
    }
#' 
#' @rdname is_false
#' @export
are_not_false <- function(x, na.ok) !are_false(x = x, na.ok = na.ok)


#' @aliases are_formula, is_not_formula, are_not_formula

#' @title Check if an argument is a formula
#'
#' @description This is a re-export of \code{\link[checkmate:checkFormula]{checkmate::test_formula()}}, modified to have standardised naming and standardised vector handling.
#' Documentation is atuomatically generated from the original package documentation. See the \code{\link[checkmate:checkFormula]{original}} for full details.
#' 
#' Check if an argument is a formula.
#' 
#' 
#' @returns 
#' - Calls to \code{is_formula} are guaranteed to return a scalar boolean (ie. a single \code{TRUE} or \code{FALSE} value). If an argument of length > 1 is given, \code{FALSE} is returned.
#' - \code{are_formula} is a wrapper around \code{\link[purrr]{map_lgl}(vec, \(i) is_formula(i, ...))}. A boolean vector of the same length as the input is guaranteed.
#' - Calls to \code{is_not_formula}/\code{are_not_formula} negate the output of \code{is_formula}/\code{are_formula}.
#' @md
#'
#' @param x any\cr
#' Object to check.
#' @param null.ok \code{logical(1)}\cr
#' If set to \code{TRUE}, \code{x} may also be \code{NULL}.
#' In this case only a type check of \code{x} is performed, all additional checks are disabled.
#' @name is_formula
#' @importFrom checkmate test_formula
#' @importFrom purrr map_lgl
#' @export
is_formula <- ensure_atomic_boolean('test_formula', 'checkmate')
#' 
#' @rdname is_formula
#' @export
is_not_formula <- function(x, null.ok) !is_formula(x, null.ok)
#' 
#' @rdname is_formula
#' @export
are_formula <- function(x, null.ok) {
      purrr::map_lgl(x, \(x) is_formula(x = x, null.ok = null.ok))
    }
#' 
#' @rdname is_formula
#' @export
are_not_formula <- function(x, null.ok) !are_formula(x = x, null.ok = null.ok)


#' @aliases are_disjunct, is_not_disjunct, are_not_disjunct

#' @title Check if an argument is disjunct from a given set
#'
#' @description This is a re-export of \code{\link[checkmate:checkDisjunct]{checkmate::test_disjunct()}}, modified to have standardised naming and standardised vector handling.
#' Documentation is atuomatically generated from the original package documentation. See the \code{\link[checkmate:checkDisjunct]{original}} for full details.
#' 
#' Check if an argument is disjunct from a given set.
#' 
#' 
#' @returns 
#' - Calls to \code{is_disjunct} are guaranteed to return a scalar boolean (ie. a single \code{TRUE} or \code{FALSE} value). If an argument of length > 1 is given, \code{FALSE} is returned.
#' - \code{are_disjunct} is a wrapper around \code{\link[purrr]{map_lgl}(vec, \(i) is_disjunct(i, ...))}. A boolean vector of the same length as the input is guaranteed.
#' - Calls to \code{is_not_disjunct}/\code{are_not_disjunct} negate the output of \code{is_disjunct}/\code{are_disjunct}.
#' @md
#'
#' @param x any\cr
#' Object to check.
#' @param y \code{atomic}\cr
#' Other Set.
#' @param fmatch \code{logical(1)}\cr
#' Use the set operations implemented in \code{\link{fmatch}} in package \pkg{fastmatch}.
#' If \pkg{fastmatch} is not installed, this silently falls back to \code{\link{match}}.
#' \code{\link{fmatch}} modifies \code{y} by reference:
#' A hash table is added as attribute which is used in subsequent calls.
#' @name is_disjunct
#' @importFrom checkmate test_disjunct
#' @importFrom purrr map_lgl
#' @export
is_disjunct <- ensure_atomic_boolean('test_disjunct', 'checkmate')
#' 
#' @rdname is_disjunct
#' @export
is_not_disjunct <- function(x, y, fmatch) !is_disjunct(x, y, fmatch)
#' 
#' @rdname is_disjunct
#' @export
are_disjunct <- function(x, y, fmatch) {
      purrr::map_lgl(x, \(x) is_disjunct(x = x, y = y, fmatch = fmatch))
    }
#' 
#' @rdname is_disjunct
#' @export
are_not_disjunct <- function(x, y, fmatch) !are_disjunct(x = x, y = y, fmatch = fmatch)


#' @aliases are_existing_test_directory, is_not_existing_test_directory, are_not_existing_test_directory

#' @title Check for existence and access rights of directories
#'
#' @description This is a re-export of \code{\link[checkmate:checkDirectoryExists]{checkmate::test_directory_exists()}}, modified to have standardised naming and standardised vector handling.
#' Documentation is atuomatically generated from the original package documentation. See the \code{\link[checkmate:checkDirectoryExists]{original}} for full details.
#' 
#' Check for existence and access rights of directories.
#' 
#' 
#' @returns 
#' - Calls to \code{is_existing_test_directory} are guaranteed to return a scalar boolean (ie. a single \code{TRUE} or \code{FALSE} value). If an argument of length > 1 is given, \code{FALSE} is returned.
#' - \code{are_existing_test_directory} is a wrapper around \code{\link[purrr]{map_lgl}(vec, \(i) is_existing_test_directory(i, ...))}. A boolean vector of the same length as the input is guaranteed.
#' - Calls to \code{is_not_existing_test_directory}/\code{are_not_existing_test_directory} negate the output of \code{is_existing_test_directory}/\code{are_existing_test_directory}.
#' @md
#'
#' @param x any\cr
#' Object to check.
#' @param access \code{character(1)}\cr
#' Single string containing possible characters \sQuote{r}, \sQuote{w} and \sQuote{x} to
#' force a check for read, write or execute access rights, respectively.
#' Write and executable rights are not checked on Windows.
#' @name is_existing_test_directory
#' @importFrom checkmate test_directory_exists
#' @importFrom purrr map_lgl
#' @export
is_existing_test_directory <- ensure_atomic_boolean('test_directory_exists', 'checkmate')
#' 
#' @rdname is_existing_test_directory
#' @export
is_not_existing_test_directory <- function(x, access) !is_existing_test_directory(x, access)
#' 
#' @rdname is_existing_test_directory
#' @export
are_existing_test_directory <- function(x, access) {
      purrr::map_lgl(x, \(x) is_existing_test_directory(x = x, access = access))
    }
#' 
#' @rdname is_existing_test_directory
#' @export
are_not_existing_test_directory <- function(x, access) !are_existing_test_directory(x = x, access = access)


#' @aliases are_atomic, is_not_atomic, are_not_atomic

#' @title Check that an argument is an atomic vector
#'
#' @description This is a re-export of \code{\link[checkmate:checkAtomic]{checkmate::test_atomic()}}, modified to have standardised naming and standardised vector handling.
#' Documentation is atuomatically generated from the original package documentation. See the \code{\link[checkmate:checkAtomic]{original}} for full details.
#' 
#' For the definition of \dQuote{atomic}, see \code{\link{is.atomic}}.
#' 
#' Note that `NULL` is recognized as a valid atomic value, as in R versions up to version 4.3.x.
#' For details, see \url{https://stat.ethz.ch/pipermail/r-devel/2023-September/082892.html}.
#' 
#' 
#' @returns 
#' - Calls to \code{is_atomic} are guaranteed to return a scalar boolean (ie. a single \code{TRUE} or \code{FALSE} value). If an argument of length > 1 is given, \code{FALSE} is returned.
#' - \code{are_atomic} is a wrapper around \code{\link[purrr]{map_lgl}(vec, \(i) is_atomic(i, ...))}. A boolean vector of the same length as the input is guaranteed.
#' - Calls to \code{is_not_atomic}/\code{are_not_atomic} negate the output of \code{is_atomic}/\code{are_atomic}.
#' @md
#'
#' @param x any\cr
#' Object to check.
#' @param any.missing \code{logical(1)}\cr
#' Are vectors with missing values allowed? Default is \code{TRUE}.
#' @param all.missing \code{logical(1)}\cr
#' Are vectors with no non-missing values allowed? Default is \code{TRUE}.
#' Note that empty vectors do not have non-missing values.
#' @param len \code{integer(1)}\cr
#' Exact expected length of \code{x}.
#' @param min.len \code{integer(1)}\cr
#' Minimal length of \code{x}.
#' @param max.len \code{integer(1)}\cr
#' Maximal length of \code{x}.
#' @param unique \code{logical(1)}\cr
#' Must all values be unique? Default is \code{FALSE}.
#' @param names \code{character(1)}\cr
#' Check for names. See \code{\link{checkNamed}} for possible values.
#' Default is \dQuote{any} which performs no check at all.
#' Note that you can use \code{\link{checkSubset}} to check for a specific set of names.
#' @name is_atomic
#' @importFrom checkmate test_atomic
#' @importFrom purrr map_lgl
#' @export
is_atomic <- ensure_atomic_boolean('test_atomic', 'checkmate')
#' 
#' @rdname is_atomic
#' @export
is_not_atomic <- function(x, any.missing, all.missing, len, min.len, max.len, unique, names) !is_atomic(x, any.missing, all.missing, len, min.len, max.len, unique, names)
#' 
#' @rdname is_atomic
#' @export
are_atomic <- function(x, any.missing, all.missing, len, min.len, max.len, unique, names) {
      purrr::map_lgl(x, \(x) is_atomic(x = x, any.missing = any.missing, all.missing = all.missing, len = len, min.len = min.len, max.len = max.len, unique = unique, names = names))
    }
#' 
#' @rdname is_atomic
#' @export
are_not_atomic <- function(x, any.missing, all.missing, len, min.len, max.len, unique, names) !are_atomic(x = x, any.missing = any.missing, all.missing = all.missing, len = len, min.len = min.len, max.len = max.len, unique = unique, names = names)


#' @aliases are_choice, is_not_choice, are_not_choice

#' @title Check if an object is an element of a given set
#'
#' @description This is a re-export of \code{\link[checkmate:checkChoice]{checkmate::test_choice()}}, modified to have standardised naming and standardised vector handling.
#' Documentation is atuomatically generated from the original package documentation. See the \code{\link[checkmate:checkChoice]{original}} for full details.
#' 
#' Check if an object is an element of a given set.
#' 
#' 
#' @returns 
#' - Calls to \code{is_choice} are guaranteed to return a scalar boolean (ie. a single \code{TRUE} or \code{FALSE} value). If an argument of length > 1 is given, \code{FALSE} is returned.
#' - \code{are_choice} is a wrapper around \code{\link[purrr]{map_lgl}(vec, \(i) is_choice(i, ...))}. A boolean vector of the same length as the input is guaranteed.
#' - Calls to \code{is_not_choice}/\code{are_not_choice} negate the output of \code{is_choice}/\code{are_choice}.
#' @md
#'
#' @param x any\cr
#' Object to check.
#' @param choices \code{atomic}\cr
#' Set of possible values.
#' @param null.ok \code{logical(1)}\cr
#' If set to \code{TRUE}, \code{x} may also be \code{NULL}.
#' In this case only a type check of \code{x} is performed, all additional checks are disabled.
#' @param fmatch \code{logical(1)}\cr
#' Use the set operations implemented in \code{\link{fmatch}} in package \pkg{fastmatch}.
#' If \pkg{fastmatch} is not installed, this silently falls back to \code{\link{match}}.
#' \code{\link{fmatch}} modifies \code{y} by reference:
#' A hash table is added as attribute which is used in subsequent calls.
#' @name is_choice
#' @importFrom checkmate test_choice
#' @importFrom purrr map_lgl
#' @export
is_choice <- ensure_atomic_boolean('test_choice', 'checkmate')
#' 
#' @rdname is_choice
#' @export
is_not_choice <- function(x, choices, null.ok, fmatch) !is_choice(x, choices, null.ok, fmatch)
#' 
#' @rdname is_choice
#' @export
are_choice <- function(x, choices, null.ok, fmatch) {
      purrr::map_lgl(x, \(x) is_choice(x = x, choices = choices, null.ok = null.ok, fmatch = fmatch))
    }
#' 
#' @rdname is_choice
#' @export
are_not_choice <- function(x, choices, null.ok, fmatch) !are_choice(x = x, choices = choices, null.ok = null.ok, fmatch = fmatch)


#' @aliases are_access, is_not_access, are_not_access

#' @title Check file system access rights
#'
#' @description This is a re-export of \code{\link[checkmate:checkAccess]{checkmate::test_access()}}, modified to have standardised naming and standardised vector handling.
#' Documentation is atuomatically generated from the original package documentation. See the \code{\link[checkmate:checkAccess]{original}} for full details.
#' 
#' Check file system access rights.
#' 
#' 
#' @returns 
#' - Calls to \code{is_access} are guaranteed to return a scalar boolean (ie. a single \code{TRUE} or \code{FALSE} value). If an argument of length > 1 is given, \code{FALSE} is returned.
#' - \code{are_access} is a wrapper around \code{\link[purrr]{map_lgl}(vec, \(i) is_access(i, ...))}. A boolean vector of the same length as the input is guaranteed.
#' - Calls to \code{is_not_access}/\code{are_not_access} negate the output of \code{is_access}/\code{are_access}.
#' @md
#'
#' @param x any\cr
#' Object to check.
#' @param access \code{character(1)}\cr
#' Single string containing possible characters \sQuote{r}, \sQuote{w} and \sQuote{x} to
#' force a check for read, write or execute access rights, respectively.
#' Write and executable rights are not checked on Windows.
#' @name is_access
#' @importFrom checkmate test_access
#' @importFrom purrr map_lgl
#' @export
is_access <- ensure_atomic_boolean('test_access', 'checkmate')
#' 
#' @rdname is_access
#' @export
is_not_access <- function(x, access) !is_access(x, access)
#' 
#' @rdname is_access
#' @export
are_access <- function(x, access) {
      purrr::map_lgl(x, \(x) is_access(x = x, access = access))
    }
#' 
#' @rdname is_access
#' @export
are_not_access <- function(x, access) !are_access(x = x, access = access)


#' @aliases are_list, is_not_list, are_not_list

#' @title Check if an argument is a list
#'
#' @description This is a re-export of \code{\link[checkmate:checkList]{checkmate::test_list()}}, modified to have standardised naming and standardised vector handling.
#' Documentation is atuomatically generated from the original package documentation. See the \code{\link[checkmate:checkList]{original}} for full details.
#' 
#' Check if an argument is a list.
#' 
#' 
#' @returns 
#' - Calls to \code{is_list} are guaranteed to return a scalar boolean (ie. a single \code{TRUE} or \code{FALSE} value). If an argument of length > 1 is given, \code{FALSE} is returned.
#' - \code{are_list} is a wrapper around \code{\link[purrr]{map_lgl}(vec, \(i) is_list(i, ...))}. A boolean vector of the same length as the input is guaranteed.
#' - Calls to \code{is_not_list}/\code{are_not_list} negate the output of \code{is_list}/\code{are_list}.
#' @md
#'
#' @param x any\cr
#' Object to check.
#' @param types \code{character}\cr
#' Character vector of class names. Each list element must inherit
#' from at least one of the provided types.
#' The types \dQuote{logical}, \dQuote{integer}, \dQuote{integerish}, \dQuote{double},
#' \dQuote{numeric}, \dQuote{complex}, \dQuote{character}, \dQuote{factor}, \dQuote{atomic}, \dQuote{vector}
#' \dQuote{atomicvector}, \dQuote{array}, \dQuote{matrix}, \dQuote{list}, \dQuote{function},
#' \dQuote{environment} and \dQuote{null} are supported.
#' For other types \code{\link{inherits}} is used as a fallback to check \code{x}'s inheritance.
#' Defaults to \code{character(0)} (no check).
#' @param any.missing \code{logical(1)}\cr
#' Are vectors with missing values allowed? Default is \code{TRUE}.
#' @param all.missing \code{logical(1)}\cr
#' Are vectors with no non-missing values allowed? Default is \code{TRUE}.
#' Note that empty vectors do not have non-missing values.
#' @param len \code{integer(1)}\cr
#' Exact expected length of \code{x}.
#' @param min.len \code{integer(1)}\cr
#' Minimal length of \code{x}.
#' @param max.len \code{integer(1)}\cr
#' Maximal length of \code{x}.
#' @param unique \code{logical(1)}\cr
#' Must all values be unique? Default is \code{FALSE}.
#' @param names \code{character(1)}\cr
#' Check for names. See \code{\link{checkNamed}} for possible values.
#' Default is \dQuote{any} which performs no check at all.
#' Note that you can use \code{\link{checkSubset}} to check for a specific set of names.
#' @param null.ok \code{logical(1)}\cr
#' If set to \code{TRUE}, \code{x} may also be \code{NULL}.
#' In this case only a type check of \code{x} is performed, all additional checks are disabled.
#' @name is_list
#' @importFrom checkmate test_list
#' @importFrom purrr map_lgl
#' @export
is_list <- ensure_atomic_boolean('test_list', 'checkmate')
#' 
#' @rdname is_list
#' @export
is_not_list <- function(x, types, any.missing, all.missing, len, min.len, max.len, unique, names, null.ok) !is_list(x, types, any.missing, all.missing, len, min.len, max.len, unique, names, null.ok)
#' 
#' @rdname is_list
#' @export
are_list <- function(x, types, any.missing, all.missing, len, min.len, max.len, unique, names, null.ok) {
      purrr::map_lgl(x, \(x) is_list(x = x, types = types, any.missing = any.missing, all.missing = all.missing, len = len, min.len = min.len, max.len = max.len, unique = unique, names = names, null.ok = null.ok))
    }
#' 
#' @rdname is_list
#' @export
are_not_list <- function(x, types, any.missing, all.missing, len, min.len, max.len, unique, names, null.ok) !are_list(x = x, types = types, any.missing = any.missing, all.missing = all.missing, len = len, min.len = min.len, max.len = max.len, unique = unique, names = names, null.ok = null.ok)


#' @aliases are_character, is_not_character, are_not_character

#' @title Check if an argument is a vector of type character
#'
#' @description This is a re-export of \code{\link[checkmate:checkCharacter]{checkmate::test_character()}}, modified to have standardised naming and standardised vector handling.
#' Documentation is atuomatically generated from the original package documentation. See the \code{\link[checkmate:checkCharacter]{original}} for full details.
#' 
#' To check for scalar strings, see \code{\link{checkString}}.
#' 
#' 
#' @returns 
#' - Calls to \code{is_character} are guaranteed to return a scalar boolean (ie. a single \code{TRUE} or \code{FALSE} value). If an argument of length > 1 is given, \code{FALSE} is returned.
#' - \code{are_character} is a wrapper around \code{\link[purrr]{map_lgl}(vec, \(i) is_character(i, ...))}. A boolean vector of the same length as the input is guaranteed.
#' - Calls to \code{is_not_character}/\code{are_not_character} negate the output of \code{is_character}/\code{are_character}.
#' @md
#'
#' @param x any\cr
#' Object to check.
#' @param n.chars \code{integer(1)}\cr
#' Exact number of characters for each element of \code{x}.
#' @param min.chars \code{integer(1)}\cr
#' Minimum number of characters for each element of \code{x}.
#' @param max.chars \code{integer(1)}\cr
#' Maximum number of characters for each element of \code{x}.
#' @param pattern \code{character(1L)}\cr
#' Regular expression as used in \code{\link{grepl}}.
#' All non-missing elements of \code{x} must comply to this pattern.
#' @param fixed \code{character(1)}\cr
#' Substring to detect in \code{x}. Will be used as \code{pattern} in \code{\link{grepl}}
#' with option \code{fixed} set to \code{TRUE}.
#' All non-missing elements of \code{x} must contain this substring.
#' @param ignore.case \code{logical(1)}\cr
#' See \code{\link{grepl}}. Default is \code{FALSE}.
#' @param any.missing \code{logical(1)}\cr
#' Are vectors with missing values allowed? Default is \code{TRUE}.
#' @param all.missing \code{logical(1)}\cr
#' Are vectors with no non-missing values allowed? Default is \code{TRUE}.
#' Note that empty vectors do not have non-missing values.
#' @param len \code{integer(1)}\cr
#' Exact expected length of \code{x}.
#' @param min.len \code{integer(1)}\cr
#' Minimal length of \code{x}.
#' @param max.len \code{integer(1)}\cr
#' Maximal length of \code{x}.
#' @param unique \code{logical(1)}\cr
#' Must all values be unique? Default is \code{FALSE}.
#' @param sorted \code{logical(1)}\cr
#' Elements must be sorted in ascending order. Missing values are ignored.
#' @param names \code{character(1)}\cr
#' Check for names. See \code{\link{checkNamed}} for possible values.
#' Default is \dQuote{any} which performs no check at all.
#' Note that you can use \code{\link{checkSubset}} to check for a specific set of names.
#' @param typed.missing \code{logical(1)}\cr
#' If set to \code{FALSE} (default), all types of missing values (\code{NA}, \code{NA_integer_},
#' \code{NA_real_}, \code{NA_character_} or \code{NA_character_}) as well as empty vectors are allowed
#' while type-checking atomic input.
#' Set to \code{TRUE} to enable strict type checking.
#' @param null.ok \code{logical(1)}\cr
#' If set to \code{TRUE}, \code{x} may also be \code{NULL}.
#' In this case only a type check of \code{x} is performed, all additional checks are disabled.
#' @name is_character
#' @importFrom checkmate test_character
#' @importFrom purrr map_lgl
#' @export
is_character <- ensure_atomic_boolean('test_character', 'checkmate')
#' 
#' @rdname is_character
#' @export
is_not_character <- function(x, n.chars, min.chars, max.chars, pattern, fixed, ignore.case, any.missing, all.missing, len, min.len, max.len, unique, sorted, names, typed.missing, null.ok) !is_character(x, n.chars, min.chars, max.chars, pattern, fixed, ignore.case, any.missing, all.missing, len, min.len, max.len, unique, sorted, names, typed.missing, null.ok)
#' 
#' @rdname is_character
#' @export
are_character <- function(x, n.chars, min.chars, max.chars, pattern, fixed, ignore.case, any.missing, all.missing, len, min.len, max.len, unique, sorted, names, typed.missing, null.ok) {
      purrr::map_lgl(x, \(x) is_character(x = x, n.chars = n.chars, min.chars = min.chars, max.chars = max.chars, pattern = pattern, fixed = fixed, ignore.case = ignore.case, any.missing = any.missing, all.missing = all.missing, len = len, min.len = min.len, max.len = max.len, unique = unique, sorted = sorted, names = names, typed.missing = typed.missing, null.ok = null.ok))
    }
#' 
#' @rdname is_character
#' @export
are_not_character <- function(x, n.chars, min.chars, max.chars, pattern, fixed, ignore.case, any.missing, all.missing, len, min.len, max.len, unique, sorted, names, typed.missing, null.ok) !are_character(x = x, n.chars = n.chars, min.chars = min.chars, max.chars = max.chars, pattern = pattern, fixed = fixed, ignore.case = ignore.case, any.missing = any.missing, all.missing = all.missing, len = len, min.len = min.len, max.len = max.len, unique = unique, sorted = sorted, names = names, typed.missing = typed.missing, null.ok = null.ok)


#' @aliases are_matrix, is_not_matrix, are_not_matrix

#' @title Check if an argument is a matrix
#'
#' @description This is a re-export of \code{\link[checkmate:checkMatrix]{checkmate::test_matrix()}}, modified to have standardised naming and standardised vector handling.
#' Documentation is atuomatically generated from the original package documentation. See the \code{\link[checkmate:checkMatrix]{original}} for full details.
#' 
#' Check if an argument is a matrix.
#' 
#' 
#' @returns 
#' - Calls to \code{is_matrix} are guaranteed to return a scalar boolean (ie. a single \code{TRUE} or \code{FALSE} value). If an argument of length > 1 is given, \code{FALSE} is returned.
#' - \code{are_matrix} is a wrapper around \code{\link[purrr]{map_lgl}(vec, \(i) is_matrix(i, ...))}. A boolean vector of the same length as the input is guaranteed.
#' - Calls to \code{is_not_matrix}/\code{are_not_matrix} negate the output of \code{is_matrix}/\code{are_matrix}.
#' @md
#'
#' @param x any\cr
#' Object to check.
#' @param mode \code{character(1)}\cr
#' Storage mode of the array. Arrays can hold vectors, i.e. \dQuote{logical},
#' \dQuote{integer}, \dQuote{integerish}, \dQuote{double}, \dQuote{numeric}, \dQuote{complex},
#' \dQuote{character} and \dQuote{list}. You can also specify \dQuote{atomic}
#' here to explicitly prohibit lists. Default is \code{NULL} (no check).
#' If all values of \code{x} are missing, this check is skipped.
#' @param any.missing \code{logical(1)}\cr
#' Are missing values allowed? Default is \code{TRUE}.
#' @param all.missing \code{logical(1)}\cr
#' Are matrices with only missing values allowed? Default is \code{TRUE}.
#' @param min.rows \code{integer(1)}\cr
#' Minimum number of rows.
#' @param max.rows \code{integer(1)}\cr
#' Maximum number of rows.
#' @param min.cols \code{integer(1)}\cr
#' Minimum number of columns.
#' @param max.cols \code{integer(1)}\cr
#' Maximum number of columns.
#' @param nrows \code{integer(1)}\cr
#' Exact number of rows.
#' @param ncols \code{integer(1)}\cr
#' Exact number of columns.
#' @param row.names \code{character(1)}\cr
#' Check for row names. Default is \dQuote{NULL} (no check).
#' See \code{\link{checkNamed}} for possible values.
#' Note that you can use \code{\link{checkSubset}} to check for a specific set of names.
#' @param col.names \code{character(1)}\cr
#' Check for column names. Default is \dQuote{NULL} (no check).
#' See \code{\link{checkNamed}} for possible values.
#' Note that you can use \code{\link{checkSubset}} to test for a specific set of names.
#' @param null.ok \code{logical(1)}\cr
#' If set to \code{TRUE}, \code{x} may also be \code{NULL}.
#' In this case only a type check of \code{x} is performed, all additional checks are disabled.
#' @name is_matrix
#' @importFrom checkmate test_matrix
#' @importFrom purrr map_lgl
#' @export
is_matrix <- ensure_atomic_boolean('test_matrix', 'checkmate')
#' 
#' @rdname is_matrix
#' @export
is_not_matrix <- function(x, mode, any.missing, all.missing, min.rows, max.rows, min.cols, max.cols, nrows, ncols, row.names, col.names, null.ok) !is_matrix(x, mode, any.missing, all.missing, min.rows, max.rows, min.cols, max.cols, nrows, ncols, row.names, col.names, null.ok)
#' 
#' @rdname is_matrix
#' @export
are_matrix <- function(x, mode, any.missing, all.missing, min.rows, max.rows, min.cols, max.cols, nrows, ncols, row.names, col.names, null.ok) {
      purrr::map_lgl(x, \(x) is_matrix(x = x, mode = mode, any.missing = any.missing, all.missing = all.missing, min.rows = min.rows, max.rows = max.rows, min.cols = min.cols, max.cols = max.cols, nrows = nrows, ncols = ncols, row.names = row.names, col.names = col.names, null.ok = null.ok))
    }
#' 
#' @rdname is_matrix
#' @export
are_not_matrix <- function(x, mode, any.missing, all.missing, min.rows, max.rows, min.cols, max.cols, nrows, ncols, row.names, col.names, null.ok) !are_matrix(x = x, mode = mode, any.missing = any.missing, all.missing = all.missing, min.rows = min.rows, max.rows = max.rows, min.cols = min.cols, max.cols = max.cols, nrows = nrows, ncols = ncols, row.names = row.names, col.names = col.names, null.ok = null.ok)


#' @aliases are_numeric, is_not_numeric, are_not_numeric

#' @title Check that an argument is a vector of type numeric
#'
#' @description This is a re-export of \code{\link[checkmate:checkNumeric]{checkmate::test_numeric()}}, modified to have standardised naming and standardised vector handling.
#' Documentation is atuomatically generated from the original package documentation. See the \code{\link[checkmate:checkNumeric]{original}} for full details.
#' 
#' Vectors of storage type \dQuote{integer} and \dQuote{double} count as \dQuote{numeric}, c.f. \code{\link{is.numeric}}.
#' To explicitly check for real integer or double vectors, see \code{\link{checkInteger}}, \code{\link{checkIntegerish}} or
#' \code{\link{checkDouble}}.
#' 
#' 
#' @returns 
#' - Calls to \code{is_numeric} are guaranteed to return a scalar boolean (ie. a single \code{TRUE} or \code{FALSE} value). If an argument of length > 1 is given, \code{FALSE} is returned.
#' - \code{are_numeric} is a wrapper around \code{\link[purrr]{map_lgl}(vec, \(i) is_numeric(i, ...))}. A boolean vector of the same length as the input is guaranteed.
#' - Calls to \code{is_not_numeric}/\code{are_not_numeric} negate the output of \code{is_numeric}/\code{are_numeric}.
#' @md
#'
#' @param x any\cr
#' Object to check.
#' @param lower \code{numeric(1)}\cr
#' Lower value all elements of \code{x} must be greater than or equal to.
#' @param upper \code{numeric(1)}\cr
#' Upper value all elements of \code{x} must be lower than or equal to.
#' @param finite \code{logical(1)}\cr
#' Check for only finite values? Default is \code{FALSE}.
#' @param any.missing \code{logical(1)}\cr
#' Are vectors with missing values allowed? Default is \code{TRUE}.
#' @param all.missing \code{logical(1)}\cr
#' Are vectors with no non-missing values allowed? Default is \code{TRUE}.
#' Note that empty vectors do not have non-missing values.
#' @param len \code{integer(1)}\cr
#' Exact expected length of \code{x}.
#' @param min.len \code{integer(1)}\cr
#' Minimal length of \code{x}.
#' @param max.len \code{integer(1)}\cr
#' Maximal length of \code{x}.
#' @param unique \code{logical(1)}\cr
#' Must all values be unique? Default is \code{FALSE}.
#' @param sorted \code{logical(1)}\cr
#' Elements must be sorted in ascending order. Missing values are ignored.
#' @param names \code{character(1)}\cr
#' Check for names. See \code{\link{checkNamed}} for possible values.
#' Default is \dQuote{any} which performs no check at all.
#' Note that you can use \code{\link{checkSubset}} to check for a specific set of names.
#' @param typed.missing \code{logical(1)}\cr
#' If set to \code{FALSE} (default), all types of missing values (\code{NA}, \code{NA_integer_},
#' \code{NA_real_}, \code{NA_character_} or \code{NA_character_}) as well as empty vectors are allowed
#' while type-checking atomic input.
#' Set to \code{TRUE} to enable strict type checking.
#' @param null.ok \code{logical(1)}\cr
#' If set to \code{TRUE}, \code{x} may also be \code{NULL}.
#' In this case only a type check of \code{x} is performed, all additional checks are disabled.
#' @name is_numeric
#' @importFrom checkmate test_numeric
#' @importFrom purrr map_lgl
#' @export
is_numeric <- ensure_atomic_boolean('test_numeric', 'checkmate')
#' 
#' @rdname is_numeric
#' @export
is_not_numeric <- function(x, lower, upper, finite, any.missing, all.missing, len, min.len, max.len, unique, sorted, names, typed.missing, null.ok) !is_numeric(x, lower, upper, finite, any.missing, all.missing, len, min.len, max.len, unique, sorted, names, typed.missing, null.ok)
#' 
#' @rdname is_numeric
#' @export
are_numeric <- function(x, lower, upper, finite, any.missing, all.missing, len, min.len, max.len, unique, sorted, names, typed.missing, null.ok) {
      purrr::map_lgl(x, \(x) is_numeric(x = x, lower = lower, upper = upper, finite = finite, any.missing = any.missing, all.missing = all.missing, len = len, min.len = min.len, max.len = max.len, unique = unique, sorted = sorted, names = names, typed.missing = typed.missing, null.ok = null.ok))
    }
#' 
#' @rdname is_numeric
#' @export
are_not_numeric <- function(x, lower, upper, finite, any.missing, all.missing, len, min.len, max.len, unique, sorted, names, typed.missing, null.ok) !are_numeric(x = x, lower = lower, upper = upper, finite = finite, any.missing = any.missing, all.missing = all.missing, len = len, min.len = min.len, max.len = max.len, unique = unique, sorted = sorted, names = names, typed.missing = typed.missing, null.ok = null.ok)


#' @aliases are_string, is_not_string, are_not_string

#' @title Check if an argument is a string
#'
#' @description This is a re-export of \code{\link[checkmate:checkString]{checkmate::test_string()}}, modified to have standardised naming and standardised vector handling.
#' Documentation is atuomatically generated from the original package documentation. See the \code{\link[checkmate:checkString]{original}} for full details.
#' 
#' A string is defined as a scalar character vector.
#' To check for vectors of arbitrary length, see \code{\link{checkCharacter}}.
#' 
#' 
#' @returns 
#' - Calls to \code{is_string} are guaranteed to return a scalar boolean (ie. a single \code{TRUE} or \code{FALSE} value). If an argument of length > 1 is given, \code{FALSE} is returned.
#' - \code{are_string} is a wrapper around \code{\link[purrr]{map_lgl}(vec, \(i) is_string(i, ...))}. A boolean vector of the same length as the input is guaranteed.
#' - Calls to \code{is_not_string}/\code{are_not_string} negate the output of \code{is_string}/\code{are_string}.
#' @md
#'
#' @param x any\cr
#' Object to check.
#' @param na.ok \code{logical(1)}\cr
#' Are missing values allowed? Default is \code{FALSE}.
#' @param n.chars \code{integer(1)}\cr
#' Exact number of characters for each element of \code{x}.
#' @param min.chars \code{integer(1)}\cr
#' Minimum number of characters for each element of \code{x}.
#' @param max.chars \code{integer(1)}\cr
#' Maximum number of characters for each element of \code{x}.
#' @param pattern \code{character(1L)}\cr
#' Regular expression as used in \code{\link{grepl}}.
#' All non-missing elements of \code{x} must comply to this pattern.
#' @param fixed \code{character(1)}\cr
#' Substring to detect in \code{x}. Will be used as \code{pattern} in \code{\link{grepl}}
#' with option \code{fixed} set to \code{TRUE}.
#' All non-missing elements of \code{x} must contain this substring.
#' @param ignore.case \code{logical(1)}\cr
#' See \code{\link{grepl}}. Default is \code{FALSE}.
#' @param null.ok \code{logical(1)}\cr
#' If set to \code{TRUE}, \code{x} may also be \code{NULL}.
#' In this case only a type check of \code{x} is performed, all additional checks are disabled.
#' @name is_string
#' @importFrom checkmate test_string
#' @importFrom purrr map_lgl
#' @export
is_string <- ensure_atomic_boolean('test_string', 'checkmate')
#' 
#' @rdname is_string
#' @export
is_not_string <- function(x, na.ok, n.chars, min.chars, max.chars, pattern, fixed, ignore.case, null.ok) !is_string(x, na.ok, n.chars, min.chars, max.chars, pattern, fixed, ignore.case, null.ok)
#' 
#' @rdname is_string
#' @export
are_string <- function(x, na.ok, n.chars, min.chars, max.chars, pattern, fixed, ignore.case, null.ok) {
      purrr::map_lgl(x, \(x) is_string(x = x, na.ok = na.ok, n.chars = n.chars, min.chars = min.chars, max.chars = max.chars, pattern = pattern, fixed = fixed, ignore.case = ignore.case, null.ok = null.ok))
    }
#' 
#' @rdname is_string
#' @export
are_not_string <- function(x, na.ok, n.chars, min.chars, max.chars, pattern, fixed, ignore.case, null.ok) !are_string(x = x, na.ok = na.ok, n.chars = n.chars, min.chars = min.chars, max.chars = max.chars, pattern = pattern, fixed = fixed, ignore.case = ignore.case, null.ok = null.ok)


#' @aliases are_factor, is_not_factor, are_not_factor

#' @title Check if an argument is a factor
#'
#' @description This is a re-export of \code{\link[checkmate:checkFactor]{checkmate::test_factor()}}, modified to have standardised naming and standardised vector handling.
#' Documentation is atuomatically generated from the original package documentation. See the \code{\link[checkmate:checkFactor]{original}} for full details.
#' 
#' Check if an argument is a factor.
#' 
#' 
#' @returns 
#' - Calls to \code{is_factor} are guaranteed to return a scalar boolean (ie. a single \code{TRUE} or \code{FALSE} value). If an argument of length > 1 is given, \code{FALSE} is returned.
#' - \code{are_factor} is a wrapper around \code{\link[purrr]{map_lgl}(vec, \(i) is_factor(i, ...))}. A boolean vector of the same length as the input is guaranteed.
#' - Calls to \code{is_not_factor}/\code{are_not_factor} negate the output of \code{is_factor}/\code{are_factor}.
#' @md
#'
#' @param x any\cr
#' Object to check.
#' @param levels \code{character}\cr
#' Vector of allowed factor levels.
#' @param ordered \code{logical(1)}\cr
#' Check for an ordered factor? If \code{FALSE} or \code{TRUE}, checks explicitly
#' for an unordered or ordered factor, respectively.
#' Default is \code{NA} which does not perform any additional check.
#' @param empty.levels.ok \code{logical(1)}\cr
#' Are empty levels allowed?
#' Default is \code{TRUE}.
#' @param any.missing \code{logical(1)}\cr
#' Are vectors with missing values allowed? Default is \code{TRUE}.
#' @param all.missing \code{logical(1)}\cr
#' Are vectors with no non-missing values allowed? Default is \code{TRUE}.
#' Note that empty vectors do not have non-missing values.
#' @param len \code{integer(1)}\cr
#' Exact expected length of \code{x}.
#' @param min.len \code{integer(1)}\cr
#' Minimal length of \code{x}.
#' @param max.len \code{integer(1)}\cr
#' Maximal length of \code{x}.
#' @param n.levels \code{integer(1)}\cr
#' Exact number of factor levels.
#' Default is \code{NULL} (no check).
#' @param min.levels \code{integer(1)}\cr
#' Minimum number of factor levels.
#' Default is \code{NULL} (no check).
#' @param max.levels \code{integer(1)}\cr
#' Maximum number of factor levels.
#' Default is \code{NULL} (no check).
#' @param unique \code{logical(1)}\cr
#' Must all values be unique? Default is \code{FALSE}.
#' @param names \code{character(1)}\cr
#' Check for names. See \code{\link{checkNamed}} for possible values.
#' Default is \dQuote{any} which performs no check at all.
#' Note that you can use \code{\link{checkSubset}} to check for a specific set of names.
#' @param null.ok \code{logical(1)}\cr
#' If set to \code{TRUE}, \code{x} may also be \code{NULL}.
#' In this case only a type check of \code{x} is performed, all additional checks are disabled.
#' @name is_factor
#' @importFrom checkmate test_factor
#' @importFrom purrr map_lgl
#' @export
is_factor <- ensure_atomic_boolean('test_factor', 'checkmate')
#' 
#' @rdname is_factor
#' @export
is_not_factor <- function(x, levels, ordered, empty.levels.ok, any.missing, all.missing, len, min.len, max.len, n.levels, min.levels, max.levels, unique, names, null.ok) !is_factor(x, levels, ordered, empty.levels.ok, any.missing, all.missing, len, min.len, max.len, n.levels, min.levels, max.levels, unique, names, null.ok)
#' 
#' @rdname is_factor
#' @export
are_factor <- function(x, levels, ordered, empty.levels.ok, any.missing, all.missing, len, min.len, max.len, n.levels, min.levels, max.levels, unique, names, null.ok) {
      purrr::map_lgl(x, \(x) is_factor(x = x, levels = levels, ordered = ordered, empty.levels.ok = empty.levels.ok, any.missing = any.missing, all.missing = all.missing, len = len, min.len = min.len, max.len = max.len, n.levels = n.levels, min.levels = min.levels, max.levels = max.levels, unique = unique, names = names, null.ok = null.ok))
    }
#' 
#' @rdname is_factor
#' @export
are_not_factor <- function(x, levels, ordered, empty.levels.ok, any.missing, all.missing, len, min.len, max.len, n.levels, min.levels, max.levels, unique, names, null.ok) !are_factor(x = x, levels = levels, ordered = ordered, empty.levels.ok = empty.levels.ok, any.missing = any.missing, all.missing = all.missing, len = len, min.len = min.len, max.len = max.len, n.levels = n.levels, min.levels = min.levels, max.levels = max.levels, unique = unique, names = names, null.ok = null.ok)


#' @aliases are_scalar_na, is_not_scalar_na, are_not_scalar_na

#' @title Check if an argument is a single missing value
#'
#' @description This is a re-export of \code{\link[checkmate:checkScalarNA]{checkmate::test_scalar_na()}}, modified to have standardised naming and standardised vector handling.
#' Documentation is atuomatically generated from the original package documentation. See the \code{\link[checkmate:checkScalarNA]{original}} for full details.
#' 
#' Check if an argument is a single missing value.
#' 
#' 
#' @returns 
#' - Calls to \code{is_scalar_na} are guaranteed to return a scalar boolean (ie. a single \code{TRUE} or \code{FALSE} value). If an argument of length > 1 is given, \code{FALSE} is returned.
#' - \code{are_scalar_na} is a wrapper around \code{\link[purrr]{map_lgl}(vec, \(i) is_scalar_na(i, ...))}. A boolean vector of the same length as the input is guaranteed.
#' - Calls to \code{is_not_scalar_na}/\code{are_not_scalar_na} negate the output of \code{is_scalar_na}/\code{are_scalar_na}.
#' @md
#'
#' @param x any\cr
#' Object to check.
#' @param null.ok \code{logical(1)}\cr
#' If set to \code{TRUE}, \code{x} may also be \code{NULL}.
#' In this case only a type check of \code{x} is performed, all additional checks are disabled.
#' @name is_scalar_na
#' @importFrom checkmate test_scalar_na
#' @importFrom purrr map_lgl
#' @export
is_scalar_na <- ensure_atomic_boolean('test_scalar_na', 'checkmate')
#' 
#' @rdname is_scalar_na
#' @export
is_not_scalar_na <- function(x, null.ok) !is_scalar_na(x, null.ok)
#' 
#' @rdname is_scalar_na
#' @export
are_scalar_na <- function(x, null.ok) {
      purrr::map_lgl(x, \(x) is_scalar_na(x = x, null.ok = null.ok))
    }
#' 
#' @rdname is_scalar_na
#' @export
are_not_scalar_na <- function(x, null.ok) !are_scalar_na(x = x, null.ok = null.ok)


#' @aliases are_syntactic_literal, is_not_syntactic_literal, are_not_syntactic_literal

#' @title Is an object an expression?
#'
#' @description This is a re-export of \code{\link[rlang:is_expression]{rlang::is_syntactic_literal()}}, modified to have standardised naming and standardised vector handling.
#' Documentation is atuomatically generated from the original package documentation. See the \code{\link[rlang:is_expression]{original}} for full details.
#' 
#' In rlang, an \emph{expression} is the return type of \code{\link{parse_expr()}}, the
#' set of objects that can be obtained from parsing R code. Under this
#' definition expressions include numbers, strings, \code{NULL}, symbols,
#' and function calls. These objects can be classified as:
#' \itemize{
#' \item{ Symbolic objects, i.e. symbols and function calls (for which
#' \code{is_symbolic()} returns \code{TRUE})
#' }\item{ Syntactic literals, i.e. scalar atomic objects and \code{NULL}
#' (testable with \code{is_syntactic_literal()})
#' }}
#' 
#' \code{is_expression()} returns \code{TRUE} if the input is either a symbolic
#' object or a syntactic literal. If a call, the elements of the call
#' must all be expressions as well. Unparsable calls are not
#' considered expressions in this narrow definition.
#' 
#' Note that in base R, there exists \code{\link{expression()}} vectors, a data
#' type similar to a list that supports special attributes created by
#' the parser called source references. This data type is not
#' supported in rlang.
#' 
#' 
#' @returns 
#' - Calls to \code{is_syntactic_literal} are guaranteed to return a scalar boolean (ie. a single \code{TRUE} or \code{FALSE} value). If an argument of length > 1 is given, \code{FALSE} is returned.
#' - \code{are_syntactic_literal} is a wrapper around \code{\link[purrr]{map_lgl}(vec, \(i) is_syntactic_literal(i, ...))}. A boolean vector of the same length as the input is guaranteed.
#' - Calls to \code{is_not_syntactic_literal}/\code{are_not_syntactic_literal} negate the output of \code{is_syntactic_literal}/\code{are_syntactic_literal}.
#' @md
#'
#' @param x An object to test.
#' @name is_syntactic_literal
#' @importFrom rlang is_syntactic_literal
#' @importFrom purrr map_lgl
#' @export
is_syntactic_literal <- ensure_atomic_boolean('is_syntactic_literal', 'rlang')
#' 
#' @rdname is_syntactic_literal
#' @export
is_not_syntactic_literal <- function(x) !is_syntactic_literal(x)
#' 
#' @rdname is_syntactic_literal
#' @export
are_syntactic_literal <- function(x) {
      purrr::map_lgl(x, \(x) is_syntactic_literal(x = x))
    }
#' 
#' @rdname is_syntactic_literal
#' @export
are_not_syntactic_literal <- function(x) !are_syntactic_literal(x = x)


#' @aliases are_lang, is_not_lang, are_not_lang

#' @title Is object a call?
#'
#' @description This is a re-export of \code{\link[rlang:is_lang]{rlang::is_lang()}}, modified to have standardised naming and standardised vector handling.
#' Documentation is atuomatically generated from the original package documentation. See the \code{\link[rlang:is_lang]{original}} for full details.
#' 
#' \ifelse{html}{\href{https://lifecycle.r-lib.org/articles/stages.html#deprecated}{\figure{lifecycle-deprecated.svg}{options: alt='[Deprecated]'}}}{\strong{[Deprecated]}}
#' These functions are deprecated, please use \code{\link{is_call()}} and its \code{n}
#' argument instead.
#' 
#' 
#' @returns 
#' - Calls to \code{is_lang} are guaranteed to return a scalar boolean (ie. a single \code{TRUE} or \code{FALSE} value). If an argument of length > 1 is given, \code{FALSE} is returned.
#' - \code{are_lang} is a wrapper around \code{\link[purrr]{map_lgl}(vec, \(i) is_lang(i, ...))}. A boolean vector of the same length as the input is guaranteed.
#' - Calls to \code{is_not_lang}/\code{are_not_lang} negate the output of \code{is_lang}/\code{are_lang}.
#' @md
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
#' @importFrom purrr map_lgl
#' @export
is_lang <- ensure_atomic_boolean('is_lang', 'rlang')
#' 
#' @rdname is_lang
#' @export
is_not_lang <- function(x, name, n, ns) !is_lang(x, name, n, ns)
#' 
#' @rdname is_lang
#' @export
are_lang <- function(x, name, n, ns) {
      purrr::map_lgl(x, \(x) is_lang(x = x, name = name, n = n, ns = ns))
    }
#' 
#' @rdname is_lang
#' @export
are_not_lang <- function(x, name, n, ns) !are_lang(x = x, name = name, n = n, ns = ns)


#' @aliases are_call_simple, is_not_call_simple, are_not_call_simple

#' @title Extract function name or namespace of a call
#'
#' @description This is a re-export of \code{\link[rlang:call_name]{rlang::is_call_simple()}}, modified to have standardised naming and standardised vector handling.
#' Documentation is atuomatically generated from the original package documentation. See the \code{\link[rlang:call_name]{original}} for full details.
#' 
#' \code{call_name()} and \code{call_ns()} extract the function name or
#' namespace of \emph{simple} calls as a string. They return \code{NULL} for
#' complex calls.
#' \itemize{
#' \item{ Simple calls: \code{foo()}, \code{bar::foo()}.
#' }\item{ Complex calls: \code{foo()()}, \code{bar::foo}, \code{foo$bar()}, \code{(function() NULL)()}.
#' }}
#' 
#' The \code{is_call_simple()} predicate helps you determine whether a call
#' is simple. There are two invariants you can count on:
#' \enumerate{
#' \item{ If \code{is_call_simple(x)} returns \code{TRUE}, \code{call_name(x)} returns a
#' string. Otherwise it returns \code{NULL}.
#' }\item{ If \code{is_call_simple(x, ns = TRUE)} returns \code{TRUE}, \code{call_ns()}
#' returns a string. Otherwise it returns \code{NULL}.
#' }}.
#' 
#' 
#' @returns 
#' - Calls to \code{is_call_simple} are guaranteed to return a scalar boolean (ie. a single \code{TRUE} or \code{FALSE} value). If an argument of length > 1 is given, \code{FALSE} is returned.
#' - \code{are_call_simple} is a wrapper around \code{\link[purrr]{map_lgl}(vec, \(i) is_call_simple(i, ...))}. A boolean vector of the same length as the input is guaranteed.
#' - Calls to \code{is_not_call_simple}/\code{are_not_call_simple} negate the output of \code{is_call_simple}/\code{are_call_simple}.
#' @md
#'
#' @param x An object to test.
#' @param ns Whether call is namespaced. If \code{NULL}, \code{is_call_simple()}
#' is insensitive to namespaces. If \code{TRUE}, \code{is_call_simple()}
#' detects namespaced calls. If \code{FALSE}, it detects unnamespaced
#' calls.
#' @name is_call_simple
#' @importFrom rlang is_call_simple
#' @importFrom purrr map_lgl
#' @export
is_call_simple <- ensure_atomic_boolean('is_call_simple', 'rlang')
#' 
#' @rdname is_call_simple
#' @export
is_not_call_simple <- function(x, ns) !is_call_simple(x, ns)
#' 
#' @rdname is_call_simple
#' @export
are_call_simple <- function(x, ns) {
      purrr::map_lgl(x, \(x) is_call_simple(x = x, ns = ns))
    }
#' 
#' @rdname is_call_simple
#' @export
are_not_call_simple <- function(x, ns) !are_call_simple(x = x, ns = ns)


#' @aliases are_scalar_integer, is_not_scalar_integer, are_not_scalar_integer

#' @title Scalar type predicates
#'
#' @description This is a re-export of \code{\link[rlang:scalar-type-predicates]{rlang::is_scalar_integer()}}, modified to have standardised naming and standardised vector handling.
#' Documentation is atuomatically generated from the original package documentation. See the \code{\link[rlang:scalar-type-predicates]{original}} for full details.
#' 
#' These predicates check for a given type and whether the vector is
#' "scalar", that is, of length 1.
#' 
#' In addition to the length check, \code{is_string()} and \code{is_bool()}
#' return \code{FALSE} if their input is missing. This is useful for
#' type-checking arguments, when your function expects a single string
#' or a single \code{TRUE} or \code{FALSE}.
#' 
#' 
#' @returns 
#' - Calls to \code{is_scalar_integer} are guaranteed to return a scalar boolean (ie. a single \code{TRUE} or \code{FALSE} value). If an argument of length > 1 is given, \code{FALSE} is returned.
#' - \code{are_scalar_integer} is a wrapper around \code{\link[purrr]{map_lgl}(vec, \(i) is_scalar_integer(i, ...))}. A boolean vector of the same length as the input is guaranteed.
#' - Calls to \code{is_not_scalar_integer}/\code{are_not_scalar_integer} negate the output of \code{is_scalar_integer}/\code{are_scalar_integer}.
#' @md
#'
#' @param x object to be tested.
#' @name is_scalar_integer
#' @importFrom rlang is_scalar_integer
#' @importFrom purrr map_lgl
#' @export
is_scalar_integer <- ensure_atomic_boolean('is_scalar_integer', 'rlang')
#' 
#' @rdname is_scalar_integer
#' @export
is_not_scalar_integer <- function(x) !is_scalar_integer(x)
#' 
#' @rdname is_scalar_integer
#' @export
are_scalar_integer <- function(x) {
      purrr::map_lgl(x, \(x) is_scalar_integer(x = x))
    }
#' 
#' @rdname is_scalar_integer
#' @export
are_not_scalar_integer <- function(x) !are_scalar_integer(x = x)


#' @aliases are_zap, is_not_zap, are_not_zap

#' @title Create zap objects
#'
#' @description This is a re-export of \code{\link[rlang:zap]{rlang::is_zap()}}, modified to have standardised naming and standardised vector handling.
#' Documentation is atuomatically generated from the original package documentation. See the \code{\link[rlang:zap]{original}} for full details.
#' 
#' \code{zap()} creates a sentinel object that indicates that an object
#' should be removed. For instance, named zaps instruct \code{\link{env_bind()}}
#' and \code{\link{call_modify()}} to remove those objects from the environment or
#' the call.
#' 
#' The advantage of zap objects is that they unambiguously signal the
#' intent of removing an object. Sentinels like \code{NULL} or
#' \code{\link{missing_arg()}} are ambiguous because they represent valid R
#' objects.
#' 
#' 
#' @returns 
#' - Calls to \code{is_zap} are guaranteed to return a scalar boolean (ie. a single \code{TRUE} or \code{FALSE} value). If an argument of length > 1 is given, \code{FALSE} is returned.
#' - \code{are_zap} is a wrapper around \code{\link[purrr]{map_lgl}(vec, \(i) is_zap(i, ...))}. A boolean vector of the same length as the input is guaranteed.
#' - Calls to \code{is_not_zap}/\code{are_not_zap} negate the output of \code{is_zap}/\code{are_zap}.
#' @md
#'
#' @param x An object to test.
#' @name is_zap
#' @importFrom rlang is_zap
#' @importFrom purrr map_lgl
#' @export
is_zap <- ensure_atomic_boolean('is_zap', 'rlang')
#' 
#' @rdname is_zap
#' @export
is_not_zap <- function(x) !is_zap(x)
#' 
#' @rdname is_zap
#' @export
are_zap <- function(x) {
      purrr::map_lgl(x, \(x) is_zap(x = x))
    }
#' 
#' @rdname is_zap
#' @export
are_not_zap <- function(x) !are_zap(x = x)


#' @aliases are_lgl_na, is_not_lgl_na, are_not_lgl_na

#' @title Test for missing values
#'
#' @description This is a re-export of \code{\link[rlang:are_na]{rlang::is_lgl_na()}}, modified to have standardised naming and standardised vector handling.
#' Documentation is atuomatically generated from the original package documentation. See the \code{\link[rlang:are_na]{original}} for full details.
#' 
#' \ifelse{html}{\href{https://lifecycle.r-lib.org/articles/stages.html#questioning}{\figure{lifecycle-questioning.svg}{options: alt='[Questioning]'}}}{\strong{[Questioning]}}
#' 
#' \code{are_na()} checks for missing values in a vector and is equivalent
#' to \code{\link{base::is.na()}}. It is a vectorised predicate, meaning that its
#' output is always the same length as its input. On the other hand,
#' \code{is_na()} is a scalar predicate and always returns a scalar
#' boolean, \code{TRUE} or \code{FALSE}. If its input is not scalar, it returns
#' \code{FALSE}. Finally, there are typed versions that check for
#' particular \link{missing types}.
#' 
#' 
#' @returns 
#' - Calls to \code{is_lgl_na} are guaranteed to return a scalar boolean (ie. a single \code{TRUE} or \code{FALSE} value). If an argument of length > 1 is given, \code{FALSE} is returned.
#' - \code{are_lgl_na} is a wrapper around \code{\link[purrr]{map_lgl}(vec, \(i) is_lgl_na(i, ...))}. A boolean vector of the same length as the input is guaranteed.
#' - Calls to \code{is_not_lgl_na}/\code{are_not_lgl_na} negate the output of \code{is_lgl_na}/\code{are_lgl_na}.
#' @md
#'
#' @param x An object to test
#' @name is_lgl_na
#' @importFrom rlang is_lgl_na
#' @importFrom purrr map_lgl
#' @export
is_lgl_na <- ensure_atomic_boolean('is_lgl_na', 'rlang')
#' 
#' @rdname is_lgl_na
#' @export
is_not_lgl_na <- function(x) !is_lgl_na(x)
#' 
#' @rdname is_lgl_na
#' @export
are_lgl_na <- function(x) {
      purrr::map_lgl(x, \(x) is_lgl_na(x = x))
    }
#' 
#' @rdname is_lgl_na
#' @export
are_not_lgl_na <- function(x) !are_lgl_na(x = x)


#' @aliases are_bare_vector, is_not_bare_vector, are_not_bare_vector

#' @title Bare type predicates
#'
#' @description This is a re-export of \code{\link[rlang:bare-type-predicates]{rlang::is_bare_vector()}}, modified to have standardised naming and standardised vector handling.
#' Documentation is atuomatically generated from the original package documentation. See the \code{\link[rlang:bare-type-predicates]{original}} for full details.
#' 
#' These predicates check for a given type but only return \code{TRUE} for
#' bare R objects. Bare objects have no class attributes. For example,
#' a data frame is a list, but not a bare list.
#' 
#' 
#' @returns 
#' - Calls to \code{is_bare_vector} are guaranteed to return a scalar boolean (ie. a single \code{TRUE} or \code{FALSE} value). If an argument of length > 1 is given, \code{FALSE} is returned.
#' - \code{are_bare_vector} is a wrapper around \code{\link[purrr]{map_lgl}(vec, \(i) is_bare_vector(i, ...))}. A boolean vector of the same length as the input is guaranteed.
#' - Calls to \code{is_not_bare_vector}/\code{are_not_bare_vector} negate the output of \code{is_bare_vector}/\code{are_bare_vector}.
#' @md
#'
#' @param x Object to be tested.
#' @param n Expected length of a vector.
#' @name is_bare_vector
#' @importFrom rlang is_bare_vector
#' @importFrom purrr map_lgl
#' @export
is_bare_vector <- ensure_atomic_boolean('is_bare_vector', 'rlang')
#' 
#' @rdname is_bare_vector
#' @export
is_not_bare_vector <- function(x, n) !is_bare_vector(x, n)
#' 
#' @rdname is_bare_vector
#' @export
are_bare_vector <- function(x, n) {
      purrr::map_lgl(x, \(x) is_bare_vector(x = x, n = n))
    }
#' 
#' @rdname is_bare_vector
#' @export
are_not_bare_vector <- function(x, n) !are_bare_vector(x = x, n = n)


#' @aliases are_missing, is_not_missing, are_not_missing

#' @title Generate or handle a missing argument
#'
#' @description This is a re-export of \code{\link[rlang:missing_arg]{rlang::is_missing()}}, modified to have standardised naming and standardised vector handling.
#' Documentation is atuomatically generated from the original package documentation. See the \code{\link[rlang:missing_arg]{original}} for full details.
#' 
#' These functions help using the missing argument as a regular R
#' object.
#' \itemize{
#' \item{ \code{missing_arg()} generates a missing argument.
#' }\item{ \code{is_missing()} is like \code{\link{base::missing()}} but also supports
#' testing for missing arguments contained in other objects like
#' lists. It is also more consistent with default arguments which
#' are never treated as missing (see section below).
#' }\item{ \code{maybe_missing()} is useful to pass down an input that might be
#' missing to another function, potentially substituting by a
#' default value. It avoids triggering an "argument is missing" error.
#' }}.
#' 
#' 
#' @returns 
#' - Calls to \code{is_missing} are guaranteed to return a scalar boolean (ie. a single \code{TRUE} or \code{FALSE} value). If an argument of length > 1 is given, \code{FALSE} is returned.
#' - \code{are_missing} is a wrapper around \code{\link[purrr]{map_lgl}(vec, \(i) is_missing(i, ...))}. A boolean vector of the same length as the input is guaranteed.
#' - Calls to \code{is_not_missing}/\code{are_not_missing} negate the output of \code{is_missing}/\code{are_missing}.
#' @md
#'
#' @param x An object that might be the missing argument.
#' @name is_missing
#' @importFrom rlang is_missing
#' @importFrom purrr map_lgl
#' @export
is_missing <- ensure_atomic_boolean('is_missing', 'rlang')
#' 
#' @rdname is_missing
#' @export
is_not_missing <- function(x) !is_missing(x)
#' 
#' @rdname is_missing
#' @export
are_missing <- function(x) {
      purrr::map_lgl(x, \(x) is_missing(x = x))
    }
#' 
#' @rdname is_missing
#' @export
are_not_missing <- function(x) !are_missing(x = x)


#' @aliases are_scalar_double, is_not_scalar_double, are_not_scalar_double

#' @title Scalar type predicates
#'
#' @description This is a re-export of \code{\link[rlang:scalar-type-predicates]{rlang::is_scalar_double()}}, modified to have standardised naming and standardised vector handling.
#' Documentation is atuomatically generated from the original package documentation. See the \code{\link[rlang:scalar-type-predicates]{original}} for full details.
#' 
#' These predicates check for a given type and whether the vector is
#' "scalar", that is, of length 1.
#' 
#' In addition to the length check, \code{is_string()} and \code{is_bool()}
#' return \code{FALSE} if their input is missing. This is useful for
#' type-checking arguments, when your function expects a single string
#' or a single \code{TRUE} or \code{FALSE}.
#' 
#' 
#' @returns 
#' - Calls to \code{is_scalar_double} are guaranteed to return a scalar boolean (ie. a single \code{TRUE} or \code{FALSE} value). If an argument of length > 1 is given, \code{FALSE} is returned.
#' - \code{are_scalar_double} is a wrapper around \code{\link[purrr]{map_lgl}(vec, \(i) is_scalar_double(i, ...))}. A boolean vector of the same length as the input is guaranteed.
#' - Calls to \code{is_not_scalar_double}/\code{are_not_scalar_double} negate the output of \code{is_scalar_double}/\code{are_scalar_double}.
#' @md
#'
#' @param x object to be tested.
#' @name is_scalar_double
#' @importFrom rlang is_scalar_double
#' @importFrom purrr map_lgl
#' @export
is_scalar_double <- ensure_atomic_boolean('is_scalar_double', 'rlang')
#' 
#' @rdname is_scalar_double
#' @export
is_not_scalar_double <- function(x) !is_scalar_double(x)
#' 
#' @rdname is_scalar_double
#' @export
are_scalar_double <- function(x) {
      purrr::map_lgl(x, \(x) is_scalar_double(x = x))
    }
#' 
#' @rdname is_scalar_double
#' @export
are_not_scalar_double <- function(x) !are_scalar_double(x = x)


#' @aliases are_call, is_not_call, are_not_call

#' @title Is object a call?
#'
#' @description This is a re-export of \code{\link[rlang:is_call]{rlang::is_call()}}, modified to have standardised naming and standardised vector handling.
#' Documentation is atuomatically generated from the original package documentation. See the \code{\link[rlang:is_call]{original}} for full details.
#' 
#' This function tests if \code{x} is a \link{call}. This is a
#' pattern-matching predicate that returns \code{FALSE} if \code{name} and \code{n}
#' are supplied and the call does not match these properties.
#' 
#' 
#' @returns 
#' - Calls to \code{is_call} are guaranteed to return a scalar boolean (ie. a single \code{TRUE} or \code{FALSE} value). If an argument of length > 1 is given, \code{FALSE} is returned.
#' - \code{are_call} is a wrapper around \code{\link[purrr]{map_lgl}(vec, \(i) is_call(i, ...))}. A boolean vector of the same length as the input is guaranteed.
#' - Calls to \code{is_not_call}/\code{are_not_call} negate the output of \code{is_call}/\code{are_call}.
#' @md
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
#' @importFrom purrr map_lgl
#' @export
is_call <- ensure_atomic_boolean('is_call', 'rlang')
#' 
#' @rdname is_call
#' @export
is_not_call <- function(x, name, n, ns) !is_call(x, name, n, ns)
#' 
#' @rdname is_call
#' @export
are_call <- function(x, name, n, ns) {
      purrr::map_lgl(x, \(x) is_call(x = x, name = name, n = n, ns = ns))
    }
#' 
#' @rdname is_call
#' @export
are_not_call <- function(x, name, n, ns) !are_call(x = x, name = name, n = n, ns = ns)


#' @aliases are_spliced_bare, is_not_spliced_bare, are_not_spliced_bare

#' @title Splice values at dots collection time
#'
#' @description This is a re-export of \code{\link[rlang:splice]{rlang::is_spliced_bare()}}, modified to have standardised naming and standardised vector handling.
#' Documentation is atuomatically generated from the original package documentation. See the \code{\link[rlang:splice]{original}} for full details.
#' 
#' The splicing operator \verb{!!!} operates both in values contexts like
#' \code{\link{list2()}} and \code{\link{dots_list()}}, and in metaprogramming contexts like
#' \code{\link{expr()}}, \code{\link{enquos()}}, or \code{\link{inject()}}. While the end result looks the
#' same, the implementation is different and much more efficient in
#' the value cases. This difference in implementation may cause
#' performance issues for instance when going from:
#' 
#' \if{html}{\out{<div class="sourceCode r">}}\preformatted{xs <- list(2, 3)
#' list2(1, !!!xs, 4)
#' }\if{html}{\out{</div>}}
#' 
#' to:
#' 
#' \if{html}{\out{<div class="sourceCode r">}}\preformatted{inject(list2(1, !!!xs, 4))
#' }\if{html}{\out{</div>}}
#' 
#' In the former case, the performant value-splicing is used. In the
#' latter case, the slow metaprogramming splicing is used.
#' 
#' A common practical case where this may occur is when code is
#' wrapped inside a tidyeval context like \code{dplyr::mutate()}. In this
#' case, the metaprogramming operator \verb{!!!} will take over the
#' value-splicing operator, causing an unexpected slowdown.
#' 
#' To avoid this in performance-critical code, use \code{splice()} instead
#' of \verb{!!!}:
#' 
#' \if{html}{\out{<div class="sourceCode r">}}\preformatted{# These both use the fast splicing:
#' list2(1, splice(xs), 4)
#' inject(list2(1, splice(xs), 4))
#' }\if{html}{\out{</div>}}.
#' 
#' 
#' @returns 
#' - Calls to \code{is_spliced_bare} are guaranteed to return a scalar boolean (ie. a single \code{TRUE} or \code{FALSE} value). If an argument of length > 1 is given, \code{FALSE} is returned.
#' - \code{are_spliced_bare} is a wrapper around \code{\link[purrr]{map_lgl}(vec, \(i) is_spliced_bare(i, ...))}. A boolean vector of the same length as the input is guaranteed.
#' - Calls to \code{is_not_spliced_bare}/\code{are_not_spliced_bare} negate the output of \code{is_spliced_bare}/\code{are_spliced_bare}.
#' @md
#'
#' @param x A list or vector to splice non-eagerly.
#' @name is_spliced_bare
#' @importFrom rlang is_spliced_bare
#' @importFrom purrr map_lgl
#' @export
is_spliced_bare <- ensure_atomic_boolean('is_spliced_bare', 'rlang')
#' 
#' @rdname is_spliced_bare
#' @export
is_not_spliced_bare <- function(x) !is_spliced_bare(x)
#' 
#' @rdname is_spliced_bare
#' @export
are_spliced_bare <- function(x) {
      purrr::map_lgl(x, \(x) is_spliced_bare(x = x))
    }
#' 
#' @rdname is_spliced_bare
#' @export
are_not_spliced_bare <- function(x) !are_spliced_bare(x = x)


#' @aliases are_spliced, is_not_spliced, are_not_spliced

#' @title Splice values at dots collection time
#'
#' @description This is a re-export of \code{\link[rlang:splice]{rlang::is_spliced()}}, modified to have standardised naming and standardised vector handling.
#' Documentation is atuomatically generated from the original package documentation. See the \code{\link[rlang:splice]{original}} for full details.
#' 
#' The splicing operator \verb{!!!} operates both in values contexts like
#' \code{\link{list2()}} and \code{\link{dots_list()}}, and in metaprogramming contexts like
#' \code{\link{expr()}}, \code{\link{enquos()}}, or \code{\link{inject()}}. While the end result looks the
#' same, the implementation is different and much more efficient in
#' the value cases. This difference in implementation may cause
#' performance issues for instance when going from:
#' 
#' \if{html}{\out{<div class="sourceCode r">}}\preformatted{xs <- list(2, 3)
#' list2(1, !!!xs, 4)
#' }\if{html}{\out{</div>}}
#' 
#' to:
#' 
#' \if{html}{\out{<div class="sourceCode r">}}\preformatted{inject(list2(1, !!!xs, 4))
#' }\if{html}{\out{</div>}}
#' 
#' In the former case, the performant value-splicing is used. In the
#' latter case, the slow metaprogramming splicing is used.
#' 
#' A common practical case where this may occur is when code is
#' wrapped inside a tidyeval context like \code{dplyr::mutate()}. In this
#' case, the metaprogramming operator \verb{!!!} will take over the
#' value-splicing operator, causing an unexpected slowdown.
#' 
#' To avoid this in performance-critical code, use \code{splice()} instead
#' of \verb{!!!}:
#' 
#' \if{html}{\out{<div class="sourceCode r">}}\preformatted{# These both use the fast splicing:
#' list2(1, splice(xs), 4)
#' inject(list2(1, splice(xs), 4))
#' }\if{html}{\out{</div>}}.
#' 
#' 
#' @returns 
#' - Calls to \code{is_spliced} are guaranteed to return a scalar boolean (ie. a single \code{TRUE} or \code{FALSE} value). If an argument of length > 1 is given, \code{FALSE} is returned.
#' - \code{are_spliced} is a wrapper around \code{\link[purrr]{map_lgl}(vec, \(i) is_spliced(i, ...))}. A boolean vector of the same length as the input is guaranteed.
#' - Calls to \code{is_not_spliced}/\code{are_not_spliced} negate the output of \code{is_spliced}/\code{are_spliced}.
#' @md
#'
#' @param x A list or vector to splice non-eagerly.
#' @name is_spliced
#' @importFrom rlang is_spliced
#' @importFrom purrr map_lgl
#' @export
is_spliced <- ensure_atomic_boolean('is_spliced', 'rlang')
#' 
#' @rdname is_spliced
#' @export
is_not_spliced <- function(x) !is_spliced(x)
#' 
#' @rdname is_spliced
#' @export
are_spliced <- function(x) {
      purrr::map_lgl(x, \(x) is_spliced(x = x))
    }
#' 
#' @rdname is_spliced
#' @export
are_not_spliced <- function(x) !are_spliced(x = x)


#' @aliases are_closure, is_not_closure, are_not_closure

#' @title Is object a function?
#'
#' @description This is a re-export of \code{\link[rlang:is_function]{rlang::is_closure()}}, modified to have standardised naming and standardised vector handling.
#' Documentation is atuomatically generated from the original package documentation. See the \code{\link[rlang:is_function]{original}} for full details.
#' 
#' The R language defines two different types of functions: primitive
#' functions, which are low-level, and closures, which are the regular
#' kind of functions.
#' 
#' 
#' @returns 
#' - Calls to \code{is_closure} are guaranteed to return a scalar boolean (ie. a single \code{TRUE} or \code{FALSE} value). If an argument of length > 1 is given, \code{FALSE} is returned.
#' - \code{are_closure} is a wrapper around \code{\link[purrr]{map_lgl}(vec, \(i) is_closure(i, ...))}. A boolean vector of the same length as the input is guaranteed.
#' - Calls to \code{is_not_closure}/\code{are_not_closure} negate the output of \code{is_closure}/\code{are_closure}.
#' @md
#'
#' @param x Object to be tested.
#' @name is_closure
#' @importFrom rlang is_closure
#' @importFrom purrr map_lgl
#' @export
is_closure <- ensure_atomic_boolean('is_closure', 'rlang')
#' 
#' @rdname is_closure
#' @export
is_not_closure <- function(x) !is_closure(x)
#' 
#' @rdname is_closure
#' @export
are_closure <- function(x) {
      purrr::map_lgl(x, \(x) is_closure(x = x))
    }
#' 
#' @rdname is_closure
#' @export
are_not_closure <- function(x) !are_closure(x = x)


#' @aliases are_empty, is_not_empty, are_not_empty

#' @title Is object an empty vector or NULL?
#'
#' @description This is a re-export of \code{\link[rlang:is_empty]{rlang::is_empty()}}, modified to have standardised naming and standardised vector handling.
#' Documentation is atuomatically generated from the original package documentation. See the \code{\link[rlang:is_empty]{original}} for full details.
#' 
#' Is object an empty vector or NULL?.
#' 
#' 
#' @returns 
#' - Calls to \code{is_empty} are guaranteed to return a scalar boolean (ie. a single \code{TRUE} or \code{FALSE} value). If an argument of length > 1 is given, \code{FALSE} is returned.
#' - \code{are_empty} is a wrapper around \code{\link[purrr]{map_lgl}(vec, \(i) is_empty(i, ...))}. A boolean vector of the same length as the input is guaranteed.
#' - Calls to \code{is_not_empty}/\code{are_not_empty} negate the output of \code{is_empty}/\code{are_empty}.
#' @md
#'
#' @param x object to test
#' @name is_empty
#' @importFrom rlang is_empty
#' @importFrom purrr map_lgl
#' @export
is_empty <- ensure_atomic_boolean('is_empty', 'rlang')
#' 
#' @rdname is_empty
#' @export
is_not_empty <- function(x) !is_empty(x)
#' 
#' @rdname is_empty
#' @export
are_empty <- function(x) {
      purrr::map_lgl(x, \(x) is_empty(x = x))
    }
#' 
#' @rdname is_empty
#' @export
are_not_empty <- function(x) !are_empty(x = x)


#' @aliases are_bare_double, is_not_bare_double, are_not_bare_double

#' @title Bare type predicates
#'
#' @description This is a re-export of \code{\link[rlang:bare-type-predicates]{rlang::is_bare_double()}}, modified to have standardised naming and standardised vector handling.
#' Documentation is atuomatically generated from the original package documentation. See the \code{\link[rlang:bare-type-predicates]{original}} for full details.
#' 
#' These predicates check for a given type but only return \code{TRUE} for
#' bare R objects. Bare objects have no class attributes. For example,
#' a data frame is a list, but not a bare list.
#' 
#' 
#' @returns 
#' - Calls to \code{is_bare_double} are guaranteed to return a scalar boolean (ie. a single \code{TRUE} or \code{FALSE} value). If an argument of length > 1 is given, \code{FALSE} is returned.
#' - \code{are_bare_double} is a wrapper around \code{\link[purrr]{map_lgl}(vec, \(i) is_bare_double(i, ...))}. A boolean vector of the same length as the input is guaranteed.
#' - Calls to \code{is_not_bare_double}/\code{are_not_bare_double} negate the output of \code{is_bare_double}/\code{are_bare_double}.
#' @md
#'
#' @param x Object to be tested.
#' @param n Expected length of a vector.
#' @name is_bare_double
#' @importFrom rlang is_bare_double
#' @importFrom purrr map_lgl
#' @export
is_bare_double <- ensure_atomic_boolean('is_bare_double', 'rlang')
#' 
#' @rdname is_bare_double
#' @export
is_not_bare_double <- function(x, n) !is_bare_double(x, n)
#' 
#' @rdname is_bare_double
#' @export
are_bare_double <- function(x, n) {
      purrr::map_lgl(x, \(x) is_bare_double(x = x, n = n))
    }
#' 
#' @rdname is_bare_double
#' @export
are_not_bare_double <- function(x, n) !are_bare_double(x = x, n = n)


#' @aliases are_primitive_lazy, is_not_primitive_lazy, are_not_primitive_lazy

#' @title Is object a function?
#'
#' @description This is a re-export of \code{\link[rlang:is_function]{rlang::is_primitive_lazy()}}, modified to have standardised naming and standardised vector handling.
#' Documentation is atuomatically generated from the original package documentation. See the \code{\link[rlang:is_function]{original}} for full details.
#' 
#' The R language defines two different types of functions: primitive
#' functions, which are low-level, and closures, which are the regular
#' kind of functions.
#' 
#' 
#' @returns 
#' - Calls to \code{is_primitive_lazy} are guaranteed to return a scalar boolean (ie. a single \code{TRUE} or \code{FALSE} value). If an argument of length > 1 is given, \code{FALSE} is returned.
#' - \code{are_primitive_lazy} is a wrapper around \code{\link[purrr]{map_lgl}(vec, \(i) is_primitive_lazy(i, ...))}. A boolean vector of the same length as the input is guaranteed.
#' - Calls to \code{is_not_primitive_lazy}/\code{are_not_primitive_lazy} negate the output of \code{is_primitive_lazy}/\code{are_primitive_lazy}.
#' @md
#'
#' @param x Object to be tested.
#' @name is_primitive_lazy
#' @importFrom rlang is_primitive_lazy
#' @importFrom purrr map_lgl
#' @export
is_primitive_lazy <- ensure_atomic_boolean('is_primitive_lazy', 'rlang')
#' 
#' @rdname is_primitive_lazy
#' @export
is_not_primitive_lazy <- function(x) !is_primitive_lazy(x)
#' 
#' @rdname is_primitive_lazy
#' @export
are_primitive_lazy <- function(x) {
      purrr::map_lgl(x, \(x) is_primitive_lazy(x = x))
    }
#' 
#' @rdname is_primitive_lazy
#' @export
are_not_primitive_lazy <- function(x) !are_primitive_lazy(x = x)


#' @aliases are_scalar_character, is_not_scalar_character, are_not_scalar_character

#' @title Scalar type predicates
#'
#' @description This is a re-export of \code{\link[rlang:scalar-type-predicates]{rlang::is_scalar_character()}}, modified to have standardised naming and standardised vector handling.
#' Documentation is atuomatically generated from the original package documentation. See the \code{\link[rlang:scalar-type-predicates]{original}} for full details.
#' 
#' These predicates check for a given type and whether the vector is
#' "scalar", that is, of length 1.
#' 
#' In addition to the length check, \code{is_string()} and \code{is_bool()}
#' return \code{FALSE} if their input is missing. This is useful for
#' type-checking arguments, when your function expects a single string
#' or a single \code{TRUE} or \code{FALSE}.
#' 
#' 
#' @returns 
#' - Calls to \code{is_scalar_character} are guaranteed to return a scalar boolean (ie. a single \code{TRUE} or \code{FALSE} value). If an argument of length > 1 is given, \code{FALSE} is returned.
#' - \code{are_scalar_character} is a wrapper around \code{\link[purrr]{map_lgl}(vec, \(i) is_scalar_character(i, ...))}. A boolean vector of the same length as the input is guaranteed.
#' - Calls to \code{is_not_scalar_character}/\code{are_not_scalar_character} negate the output of \code{is_scalar_character}/\code{are_scalar_character}.
#' @md
#'
#' @param x object to be tested.
#' @name is_scalar_character
#' @importFrom rlang is_scalar_character
#' @importFrom purrr map_lgl
#' @export
is_scalar_character <- ensure_atomic_boolean('is_scalar_character', 'rlang')
#' 
#' @rdname is_scalar_character
#' @export
is_not_scalar_character <- function(x) !is_scalar_character(x)
#' 
#' @rdname is_scalar_character
#' @export
are_scalar_character <- function(x) {
      purrr::map_lgl(x, \(x) is_scalar_character(x = x))
    }
#' 
#' @rdname is_scalar_character
#' @export
are_not_scalar_character <- function(x) !are_scalar_character(x = x)


#' @aliases are_named2, is_not_named2, are_not_named2

#' @title Is object named?
#'
#' @description This is a re-export of \code{\link[rlang:is_named]{rlang::is_named2()}}, modified to have standardised naming and standardised vector handling.
#' Documentation is atuomatically generated from the original package documentation. See the \code{\link[rlang:is_named]{original}} for full details.
#' 
#' \itemize{
#' \item{ \code{is_named()} is a scalar predicate that checks that \code{x} has a
#' \code{names} attribute and that none of the names are missing or empty
#' (\code{NA} or \code{""}).
#' }\item{ \code{is_named2()} is like \code{is_named()} but always returns \code{TRUE} for
#' empty vectors, even those that don't have a \code{names} attribute.
#' In other words, it tests for the property that each element of a
#' vector is named. \code{is_named2()} composes well with \code{\link{names2()}}
#' whereas \code{is_named()} composes with \code{names()}.
#' }\item{ \code{have_name()} is a vectorised variant.
#' }}.
#' 
#' 
#' @returns 
#' - Calls to \code{is_named2} are guaranteed to return a scalar boolean (ie. a single \code{TRUE} or \code{FALSE} value). If an argument of length > 1 is given, \code{FALSE} is returned.
#' - \code{are_named2} is a wrapper around \code{\link[purrr]{map_lgl}(vec, \(i) is_named2(i, ...))}. A boolean vector of the same length as the input is guaranteed.
#' - Calls to \code{is_not_named2}/\code{are_not_named2} negate the output of \code{is_named2}/\code{are_named2}.
#' @md
#'
#' @param x A vector to test.
#' @name is_named2
#' @importFrom rlang is_named2
#' @importFrom purrr map_lgl
#' @export
is_named2 <- ensure_atomic_boolean('is_named2', 'rlang')
#' 
#' @rdname is_named2
#' @export
is_not_named2 <- function(x) !is_named2(x)
#' 
#' @rdname is_named2
#' @export
are_named2 <- function(x) {
      purrr::map_lgl(x, \(x) is_named2(x = x))
    }
#' 
#' @rdname is_named2
#' @export
are_not_named2 <- function(x) !are_named2(x = x)


#' @aliases are_scoped, is_not_scoped, are_not_scoped

#' @title Deprecated \code{scoped} functions
#'
#' @description This is a re-export of \code{\link[rlang:scoped_env]{rlang::is_scoped()}}, modified to have standardised naming and standardised vector handling.
#' Documentation is atuomatically generated from the original package documentation. See the \code{\link[rlang:scoped_env]{original}} for full details.
#' 
#' \ifelse{html}{\href{https://lifecycle.r-lib.org/articles/stages.html#deprecated}{\figure{lifecycle-deprecated.svg}{options: alt='[Deprecated]'}}}{\strong{[Deprecated]}}
#' 
#' These functions are deprecated as of rlang 0.3.0. Please use
#' \code{\link{is_attached()}} instead.
#' 
#' 
#' @returns 
#' - Calls to \code{is_scoped} are guaranteed to return a scalar boolean (ie. a single \code{TRUE} or \code{FALSE} value). If an argument of length > 1 is given, \code{FALSE} is returned.
#' - \code{are_scoped} is a wrapper around \code{\link[purrr]{map_lgl}(vec, \(i) is_scoped(i, ...))}. A boolean vector of the same length as the input is guaranteed.
#' - Calls to \code{is_not_scoped}/\code{are_not_scoped} negate the output of \code{is_scoped}/\code{are_scoped}.
#' @md
#'
#' @param nm The name of an environment attached to the search
#' path. Call \code{\link{base::search()}} to see what is currently on the path.
#' @name is_scoped
#' @importFrom rlang is_scoped
#' @importFrom purrr map_lgl
#' @export
is_scoped <- ensure_atomic_boolean('is_scoped', 'rlang')
#' 
#' @rdname is_scoped
#' @export
is_not_scoped <- function(nm) !is_scoped(nm)
#' 
#' @rdname is_scoped
#' @export
are_scoped <- function(nm) {
      purrr::map_lgl(nm, \(nm) is_scoped(nm = nm))
    }
#' 
#' @rdname is_scoped
#' @export
are_not_scoped <- function(nm) !are_scoped(nm = nm)


#' @aliases are_bare_string, is_not_bare_string, are_not_bare_string

#' @title Bare type predicates
#'
#' @description This is a re-export of \code{\link[rlang:bare-type-predicates]{rlang::is_bare_string()}}, modified to have standardised naming and standardised vector handling.
#' Documentation is atuomatically generated from the original package documentation. See the \code{\link[rlang:bare-type-predicates]{original}} for full details.
#' 
#' These predicates check for a given type but only return \code{TRUE} for
#' bare R objects. Bare objects have no class attributes. For example,
#' a data frame is a list, but not a bare list.
#' 
#' 
#' @returns 
#' - Calls to \code{is_bare_string} are guaranteed to return a scalar boolean (ie. a single \code{TRUE} or \code{FALSE} value). If an argument of length > 1 is given, \code{FALSE} is returned.
#' - \code{are_bare_string} is a wrapper around \code{\link[purrr]{map_lgl}(vec, \(i) is_bare_string(i, ...))}. A boolean vector of the same length as the input is guaranteed.
#' - Calls to \code{is_not_bare_string}/\code{are_not_bare_string} negate the output of \code{is_bare_string}/\code{are_bare_string}.
#' @md
#'
#' @param x Object to be tested.
#' @param n Expected length of a vector.
#' @name is_bare_string
#' @importFrom rlang is_bare_string
#' @importFrom purrr map_lgl
#' @export
is_bare_string <- ensure_atomic_boolean('is_bare_string', 'rlang')
#' 
#' @rdname is_bare_string
#' @export
is_not_bare_string <- function(x, n) !is_bare_string(x, n)
#' 
#' @rdname is_bare_string
#' @export
are_bare_string <- function(x, n) {
      purrr::map_lgl(x, \(x) is_bare_string(x = x, n = n))
    }
#' 
#' @rdname is_bare_string
#' @export
are_not_bare_string <- function(x, n) !are_bare_string(x = x, n = n)


#' @aliases are_namespace, is_not_namespace, are_not_namespace

#' @title Is an object a namespace environment?
#'
#' @description This is a re-export of \code{\link[rlang:is_namespace]{rlang::is_namespace()}}, modified to have standardised naming and standardised vector handling.
#' Documentation is atuomatically generated from the original package documentation. See the \code{\link[rlang:is_namespace]{original}} for full details.
#' 
#' Is an object a namespace environment?.
#' 
#' 
#' @returns 
#' - Calls to \code{is_namespace} are guaranteed to return a scalar boolean (ie. a single \code{TRUE} or \code{FALSE} value). If an argument of length > 1 is given, \code{FALSE} is returned.
#' - \code{are_namespace} is a wrapper around \code{\link[purrr]{map_lgl}(vec, \(i) is_namespace(i, ...))}. A boolean vector of the same length as the input is guaranteed.
#' - Calls to \code{is_not_namespace}/\code{are_not_namespace} negate the output of \code{is_namespace}/\code{are_namespace}.
#' @md
#'
#' @param x An object to test.
#' @name is_namespace
#' @importFrom rlang is_namespace
#' @importFrom purrr map_lgl
#' @export
is_namespace <- ensure_atomic_boolean('is_namespace', 'rlang')
#' 
#' @rdname is_namespace
#' @export
is_not_namespace <- function(x) !is_namespace(x)
#' 
#' @rdname is_namespace
#' @export
are_namespace <- function(x) {
      purrr::map_lgl(x, \(x) is_namespace(x = x))
    }
#' 
#' @rdname is_namespace
#' @export
are_not_namespace <- function(x) !are_namespace(x = x)


#' @aliases are_bare_character, is_not_bare_character, are_not_bare_character

#' @title Bare type predicates
#'
#' @description This is a re-export of \code{\link[rlang:bare-type-predicates]{rlang::is_bare_character()}}, modified to have standardised naming and standardised vector handling.
#' Documentation is atuomatically generated from the original package documentation. See the \code{\link[rlang:bare-type-predicates]{original}} for full details.
#' 
#' These predicates check for a given type but only return \code{TRUE} for
#' bare R objects. Bare objects have no class attributes. For example,
#' a data frame is a list, but not a bare list.
#' 
#' 
#' @returns 
#' - Calls to \code{is_bare_character} are guaranteed to return a scalar boolean (ie. a single \code{TRUE} or \code{FALSE} value). If an argument of length > 1 is given, \code{FALSE} is returned.
#' - \code{are_bare_character} is a wrapper around \code{\link[purrr]{map_lgl}(vec, \(i) is_bare_character(i, ...))}. A boolean vector of the same length as the input is guaranteed.
#' - Calls to \code{is_not_bare_character}/\code{are_not_bare_character} negate the output of \code{is_bare_character}/\code{are_bare_character}.
#' @md
#'
#' @param x Object to be tested.
#' @param n Expected length of a vector.
#' @name is_bare_character
#' @importFrom rlang is_bare_character
#' @importFrom purrr map_lgl
#' @export
is_bare_character <- ensure_atomic_boolean('is_bare_character', 'rlang')
#' 
#' @rdname is_bare_character
#' @export
is_not_bare_character <- function(x, n) !is_bare_character(x, n)
#' 
#' @rdname is_bare_character
#' @export
are_bare_character <- function(x, n) {
      purrr::map_lgl(x, \(x) is_bare_character(x = x, n = n))
    }
#' 
#' @rdname is_bare_character
#' @export
are_not_bare_character <- function(x, n) !are_bare_character(x = x, n = n)


#' @aliases are_scalar_logical, is_not_scalar_logical, are_not_scalar_logical

#' @title Scalar type predicates
#'
#' @description This is a re-export of \code{\link[rlang:scalar-type-predicates]{rlang::is_scalar_logical()}}, modified to have standardised naming and standardised vector handling.
#' Documentation is atuomatically generated from the original package documentation. See the \code{\link[rlang:scalar-type-predicates]{original}} for full details.
#' 
#' These predicates check for a given type and whether the vector is
#' "scalar", that is, of length 1.
#' 
#' In addition to the length check, \code{is_string()} and \code{is_bool()}
#' return \code{FALSE} if their input is missing. This is useful for
#' type-checking arguments, when your function expects a single string
#' or a single \code{TRUE} or \code{FALSE}.
#' 
#' 
#' @returns 
#' - Calls to \code{is_scalar_logical} are guaranteed to return a scalar boolean (ie. a single \code{TRUE} or \code{FALSE} value). If an argument of length > 1 is given, \code{FALSE} is returned.
#' - \code{are_scalar_logical} is a wrapper around \code{\link[purrr]{map_lgl}(vec, \(i) is_scalar_logical(i, ...))}. A boolean vector of the same length as the input is guaranteed.
#' - Calls to \code{is_not_scalar_logical}/\code{are_not_scalar_logical} negate the output of \code{is_scalar_logical}/\code{are_scalar_logical}.
#' @md
#'
#' @param x object to be tested.
#' @name is_scalar_logical
#' @importFrom rlang is_scalar_logical
#' @importFrom purrr map_lgl
#' @export
is_scalar_logical <- ensure_atomic_boolean('is_scalar_logical', 'rlang')
#' 
#' @rdname is_scalar_logical
#' @export
is_not_scalar_logical <- function(x) !is_scalar_logical(x)
#' 
#' @rdname is_scalar_logical
#' @export
are_scalar_logical <- function(x) {
      purrr::map_lgl(x, \(x) is_scalar_logical(x = x))
    }
#' 
#' @rdname is_scalar_logical
#' @export
are_not_scalar_logical <- function(x) !are_scalar_logical(x = x)


#' @aliases are_attached, is_not_attached, are_not_attached

#' @title Search path environments
#'
#' @description This is a re-export of \code{\link[rlang:search_envs]{rlang::is_attached()}}, modified to have standardised naming and standardised vector handling.
#' Documentation is atuomatically generated from the original package documentation. See the \code{\link[rlang:search_envs]{original}} for full details.
#' 
#' The search path is a chain of environments containing exported
#' functions of attached packages.
#' 
#' The API includes:
#' \itemize{
#' \item{ \code{\link{base::search()}} to get the names of environments attached to the
#' search path.
#' }\item{ \code{search_envs()} returns the environments on the search path as a
#' list.
#' }\item{ \code{pkg_env_name()} takes a bare package name and prefixes it with
#' \code{"package:"}. Attached package environments have search names of
#' the form \code{package:name}.
#' }\item{ \code{pkg_env()} takes a bare package name and returns the scoped
#' environment of packages if they are attached to the search path,
#' and throws an error otherwise. It is a shortcut for
#' \code{search_env(pkg_env_name("pkgname"))}.
#' }\item{ \code{global_env()} and \code{base_env()} (simple aliases for \code{\link{globalenv()}}
#' and \code{\link{baseenv()}}). These are respectively the first and last
#' environments of the search path.
#' }\item{ \code{is_attached()} returns \code{TRUE} when its argument (a search name
#' or a package environment) is attached to the search path.
#' }}.
#' 
#' 
#' @returns 
#' - Calls to \code{is_attached} are guaranteed to return a scalar boolean (ie. a single \code{TRUE} or \code{FALSE} value). If an argument of length > 1 is given, \code{FALSE} is returned.
#' - \code{are_attached} is a wrapper around \code{\link[purrr]{map_lgl}(vec, \(i) is_attached(i, ...))}. A boolean vector of the same length as the input is guaranteed.
#' - Calls to \code{is_not_attached}/\code{are_not_attached} negate the output of \code{is_attached}/\code{are_attached}.
#' @md
#'
#' @param x An environment or a search name.
#' @name is_attached
#' @importFrom rlang is_attached
#' @importFrom purrr map_lgl
#' @export
is_attached <- ensure_atomic_boolean('is_attached', 'rlang')
#' 
#' @rdname is_attached
#' @export
is_not_attached <- function(x) !is_attached(x)
#' 
#' @rdname is_attached
#' @export
are_attached <- function(x) {
      purrr::map_lgl(x, \(x) is_attached(x = x))
    }
#' 
#' @rdname is_attached
#' @export
are_not_attached <- function(x) !are_attached(x = x)


#' @aliases are_scalar_atomic, is_not_scalar_atomic, are_not_scalar_atomic

#' @title Scalar type predicates
#'
#' @description This is a re-export of \code{\link[rlang:scalar-type-predicates]{rlang::is_scalar_atomic()}}, modified to have standardised naming and standardised vector handling.
#' Documentation is atuomatically generated from the original package documentation. See the \code{\link[rlang:scalar-type-predicates]{original}} for full details.
#' 
#' These predicates check for a given type and whether the vector is
#' "scalar", that is, of length 1.
#' 
#' In addition to the length check, \code{is_string()} and \code{is_bool()}
#' return \code{FALSE} if their input is missing. This is useful for
#' type-checking arguments, when your function expects a single string
#' or a single \code{TRUE} or \code{FALSE}.
#' 
#' 
#' @returns 
#' - Calls to \code{is_scalar_atomic} are guaranteed to return a scalar boolean (ie. a single \code{TRUE} or \code{FALSE} value). If an argument of length > 1 is given, \code{FALSE} is returned.
#' - \code{are_scalar_atomic} is a wrapper around \code{\link[purrr]{map_lgl}(vec, \(i) is_scalar_atomic(i, ...))}. A boolean vector of the same length as the input is guaranteed.
#' - Calls to \code{is_not_scalar_atomic}/\code{are_not_scalar_atomic} negate the output of \code{is_scalar_atomic}/\code{are_scalar_atomic}.
#' @md
#'
#' @param x object to be tested.
#' @name is_scalar_atomic
#' @importFrom rlang is_scalar_atomic
#' @importFrom purrr map_lgl
#' @export
is_scalar_atomic <- ensure_atomic_boolean('is_scalar_atomic', 'rlang')
#' 
#' @rdname is_scalar_atomic
#' @export
is_not_scalar_atomic <- function(x) !is_scalar_atomic(x)
#' 
#' @rdname is_scalar_atomic
#' @export
are_scalar_atomic <- function(x) {
      purrr::map_lgl(x, \(x) is_scalar_atomic(x = x))
    }
#' 
#' @rdname is_scalar_atomic
#' @export
are_not_scalar_atomic <- function(x) !are_scalar_atomic(x = x)


#' @aliases are_bytes, is_not_bytes, are_not_bytes

#' @title Type predicates
#'
#' @description This is a re-export of \code{\link[rlang:type-predicates]{rlang::is_bytes()}}, modified to have standardised naming and standardised vector handling.
#' Documentation is atuomatically generated from the original package documentation. See the \code{\link[rlang:type-predicates]{original}} for full details.
#' 
#' These type predicates aim to make type testing in R more
#' consistent. They are wrappers around \code{\link{base::typeof()}}, so operate
#' at a level beneath S3/S4 etc.
#' 
#' 
#' @returns 
#' - Calls to \code{is_bytes} are guaranteed to return a scalar boolean (ie. a single \code{TRUE} or \code{FALSE} value). If an argument of length > 1 is given, \code{FALSE} is returned.
#' - \code{are_bytes} is a wrapper around \code{\link[purrr]{map_lgl}(vec, \(i) is_bytes(i, ...))}. A boolean vector of the same length as the input is guaranteed.
#' - Calls to \code{is_not_bytes}/\code{are_not_bytes} negate the output of \code{is_bytes}/\code{are_bytes}.
#' @md
#'
#' @param x Object to be tested.
#' @param n Expected length of a vector.
#' @name is_bytes
#' @importFrom rlang is_bytes
#' @importFrom purrr map_lgl
#' @export
is_bytes <- ensure_atomic_boolean('is_bytes', 'rlang')
#' 
#' @rdname is_bytes
#' @export
is_not_bytes <- function(x, n) !is_bytes(x, n)
#' 
#' @rdname is_bytes
#' @export
are_bytes <- function(x, n) {
      purrr::map_lgl(x, \(x) is_bytes(x = x, n = n))
    }
#' 
#' @rdname is_bytes
#' @export
are_not_bytes <- function(x, n) !are_bytes(x = x, n = n)


#' @aliases are_done_box, is_not_done_box, are_not_done_box

#' @title Box a final value for early termination
#'
#' @description This is a re-export of \code{\link[rlang:done]{rlang::is_done_box()}}, modified to have standardised naming and standardised vector handling.
#' Documentation is atuomatically generated from the original package documentation. See the \code{\link[rlang:done]{original}} for full details.
#' 
#' A value boxed with \code{done()} signals to its caller that it
#' should stop iterating. Use it to shortcircuit a loop.
#' 
#' 
#' @returns 
#' - Calls to \code{is_done_box} are guaranteed to return a scalar boolean (ie. a single \code{TRUE} or \code{FALSE} value). If an argument of length > 1 is given, \code{FALSE} is returned.
#' - \code{are_done_box} is a wrapper around \code{\link[purrr]{map_lgl}(vec, \(i) is_done_box(i, ...))}. A boolean vector of the same length as the input is guaranteed.
#' - Calls to \code{is_not_done_box}/\code{are_not_done_box} negate the output of \code{is_done_box}/\code{are_done_box}.
#' @md
#'
#' @param x For \code{done()}, a value to box. For \code{is_done_box()}, a
#' value to test.
#' @param empty Whether the box is empty. If \code{NULL}, \code{is_done_box()}
#' returns \code{TRUE} for all done boxes. If \code{TRUE}, it returns \code{TRUE}
#' only for empty boxes. Otherwise it returns \code{TRUE} only for
#' non-empty boxes.
#' @name is_done_box
#' @importFrom rlang is_done_box
#' @importFrom purrr map_lgl
#' @export
is_done_box <- ensure_atomic_boolean('is_done_box', 'rlang')
#' 
#' @rdname is_done_box
#' @export
is_not_done_box <- function(x, empty) !is_done_box(x, empty)
#' 
#' @rdname is_done_box
#' @export
are_done_box <- function(x, empty) {
      purrr::map_lgl(x, \(x) is_done_box(x = x, empty = empty))
    }
#' 
#' @rdname is_done_box
#' @export
are_not_done_box <- function(x, empty) !are_done_box(x = x, empty = empty)


#' @aliases are_bare_bytes, is_not_bare_bytes, are_not_bare_bytes

#' @title Bare type predicates
#'
#' @description This is a re-export of \code{\link[rlang:bare-type-predicates]{rlang::is_bare_bytes()}}, modified to have standardised naming and standardised vector handling.
#' Documentation is atuomatically generated from the original package documentation. See the \code{\link[rlang:bare-type-predicates]{original}} for full details.
#' 
#' These predicates check for a given type but only return \code{TRUE} for
#' bare R objects. Bare objects have no class attributes. For example,
#' a data frame is a list, but not a bare list.
#' 
#' 
#' @returns 
#' - Calls to \code{is_bare_bytes} are guaranteed to return a scalar boolean (ie. a single \code{TRUE} or \code{FALSE} value). If an argument of length > 1 is given, \code{FALSE} is returned.
#' - \code{are_bare_bytes} is a wrapper around \code{\link[purrr]{map_lgl}(vec, \(i) is_bare_bytes(i, ...))}. A boolean vector of the same length as the input is guaranteed.
#' - Calls to \code{is_not_bare_bytes}/\code{are_not_bare_bytes} negate the output of \code{is_bare_bytes}/\code{are_bare_bytes}.
#' @md
#'
#' @param x Object to be tested.
#' @param n Expected length of a vector.
#' @name is_bare_bytes
#' @importFrom rlang is_bare_bytes
#' @importFrom purrr map_lgl
#' @export
is_bare_bytes <- ensure_atomic_boolean('is_bare_bytes', 'rlang')
#' 
#' @rdname is_bare_bytes
#' @export
is_not_bare_bytes <- function(x, n) !is_bare_bytes(x, n)
#' 
#' @rdname is_bare_bytes
#' @export
are_bare_bytes <- function(x, n) {
      purrr::map_lgl(x, \(x) is_bare_bytes(x = x, n = n))
    }
#' 
#' @rdname is_bare_bytes
#' @export
are_not_bare_bytes <- function(x, n) !are_bare_bytes(x = x, n = n)


#' @aliases are_scalar_list, is_not_scalar_list, are_not_scalar_list

#' @title Scalar type predicates
#'
#' @description This is a re-export of \code{\link[rlang:scalar-type-predicates]{rlang::is_scalar_list()}}, modified to have standardised naming and standardised vector handling.
#' Documentation is atuomatically generated from the original package documentation. See the \code{\link[rlang:scalar-type-predicates]{original}} for full details.
#' 
#' These predicates check for a given type and whether the vector is
#' "scalar", that is, of length 1.
#' 
#' In addition to the length check, \code{is_string()} and \code{is_bool()}
#' return \code{FALSE} if their input is missing. This is useful for
#' type-checking arguments, when your function expects a single string
#' or a single \code{TRUE} or \code{FALSE}.
#' 
#' 
#' @returns 
#' - Calls to \code{is_scalar_list} are guaranteed to return a scalar boolean (ie. a single \code{TRUE} or \code{FALSE} value). If an argument of length > 1 is given, \code{FALSE} is returned.
#' - \code{are_scalar_list} is a wrapper around \code{\link[purrr]{map_lgl}(vec, \(i) is_scalar_list(i, ...))}. A boolean vector of the same length as the input is guaranteed.
#' - Calls to \code{is_not_scalar_list}/\code{are_not_scalar_list} negate the output of \code{is_scalar_list}/\code{are_scalar_list}.
#' @md
#'
#' @param x object to be tested.
#' @name is_scalar_list
#' @importFrom rlang is_scalar_list
#' @importFrom purrr map_lgl
#' @export
is_scalar_list <- ensure_atomic_boolean('is_scalar_list', 'rlang')
#' 
#' @rdname is_scalar_list
#' @export
is_not_scalar_list <- function(x) !is_scalar_list(x)
#' 
#' @rdname is_scalar_list
#' @export
are_scalar_list <- function(x) {
      purrr::map_lgl(x, \(x) is_scalar_list(x = x))
    }
#' 
#' @rdname is_scalar_list
#' @export
are_not_scalar_list <- function(x) !are_scalar_list(x = x)


#' @aliases are_pairlist, is_not_pairlist, are_not_pairlist

#' @title Is object a node or pairlist?
#'
#' @description This is a re-export of \code{\link[rlang:is_pairlist]{rlang::is_pairlist()}}, modified to have standardised naming and standardised vector handling.
#' Documentation is atuomatically generated from the original package documentation. See the \code{\link[rlang:is_pairlist]{original}} for full details.
#' 
#' \itemize{
#' \item{ \code{is_pairlist()} checks that \code{x} has type \code{pairlist}.
#' }\item{ \code{is_node()} checks that \code{x} has type \code{pairlist} or \code{language}.
#' It tests whether \code{x} is a node that has a CAR and a CDR,
#' including callable nodes (language objects).
#' }\item{ \code{is_node_list()} checks that \code{x} has type \code{pairlist} or \code{NULL}.
#' \code{NULL} is the empty node list.
#' }}.
#' 
#' 
#' @returns 
#' - Calls to \code{is_pairlist} are guaranteed to return a scalar boolean (ie. a single \code{TRUE} or \code{FALSE} value). If an argument of length > 1 is given, \code{FALSE} is returned.
#' - \code{are_pairlist} is a wrapper around \code{\link[purrr]{map_lgl}(vec, \(i) is_pairlist(i, ...))}. A boolean vector of the same length as the input is guaranteed.
#' - Calls to \code{is_not_pairlist}/\code{are_not_pairlist} negate the output of \code{is_pairlist}/\code{are_pairlist}.
#' @md
#'
#' @param x Object to test.
#' @name is_pairlist
#' @importFrom rlang is_pairlist
#' @importFrom purrr map_lgl
#' @export
is_pairlist <- ensure_atomic_boolean('is_pairlist', 'rlang')
#' 
#' @rdname is_pairlist
#' @export
is_not_pairlist <- function(x) !is_pairlist(x)
#' 
#' @rdname is_pairlist
#' @export
are_pairlist <- function(x) {
      purrr::map_lgl(x, \(x) is_pairlist(x = x))
    }
#' 
#' @rdname is_pairlist
#' @export
are_not_pairlist <- function(x) !are_pairlist(x = x)


#' @aliases are_installed, is_not_installed, are_not_installed

#' @title Are packages installed in any of the libraries?
#'
#' @description This is a re-export of \code{\link[rlang:is_installed]{rlang::is_installed()}}, modified to have standardised naming and standardised vector handling.
#' Documentation is atuomatically generated from the original package documentation. See the \code{\link[rlang:is_installed]{original}} for full details.
#' 
#' These functions check that packages are installed with minimal side
#' effects. If installed, the packages will be loaded but not
#' attached.
#' \itemize{
#' \item{ \code{is_installed()} doesn't interact with the user. It simply
#' returns \code{TRUE} or \code{FALSE} depending on whether the packages are
#' installed.
#' }\item{ In interactive sessions, \code{check_installed()} asks the user
#' whether to install missing packages. If the user accepts, the
#' packages are installed with \code{pak::pkg_install()} if available, or
#' \code{\link{utils::install.packages()}} otherwise. If the session is non
#' interactive or if the user chooses not to install the packages,
#' the current evaluation is aborted.
#' }}
#' 
#' You can disable the prompt by setting the
#' \code{rlib_restart_package_not_found} global option to \code{FALSE}. In that
#' case, missing packages always cause an error.
#' 
#' 
#' @returns 
#' - Calls to \code{is_installed} are guaranteed to return a scalar boolean (ie. a single \code{TRUE} or \code{FALSE} value). If an argument of length > 1 is given, \code{FALSE} is returned.
#' - \code{are_installed} is a wrapper around \code{\link[purrr]{map_lgl}(vec, \(i) is_installed(i, ...))}. A boolean vector of the same length as the input is guaranteed.
#' - Calls to \code{is_not_installed}/\code{are_not_installed} negate the output of \code{is_installed}/\code{are_installed}.
#' @md
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
#' @importFrom purrr map_lgl
#' @export
is_installed <- ensure_atomic_boolean('is_installed', 'rlang')
#' 
#' @rdname is_installed
#' @export
is_not_installed <- function(pkg, ..., version, compare) !is_installed(pkg, ..., version, compare)
#' 
#' @rdname is_installed
#' @export
are_installed <- function(pkg, ..., version, compare) {
      purrr::map_lgl(pkg, \(pkg) is_installed(pkg = pkg, ... = ..., version = version, compare = compare))
    }
#' 
#' @rdname is_installed
#' @export
are_not_installed <- function(pkg, ..., version, compare) !are_installed(pkg = pkg, ... = ..., version = version, compare = compare)


#' @aliases are_dbl_na, is_not_dbl_na, are_not_dbl_na

#' @title Test for missing values
#'
#' @description This is a re-export of \code{\link[rlang:are_na]{rlang::is_dbl_na()}}, modified to have standardised naming and standardised vector handling.
#' Documentation is atuomatically generated from the original package documentation. See the \code{\link[rlang:are_na]{original}} for full details.
#' 
#' \ifelse{html}{\href{https://lifecycle.r-lib.org/articles/stages.html#questioning}{\figure{lifecycle-questioning.svg}{options: alt='[Questioning]'}}}{\strong{[Questioning]}}
#' 
#' \code{are_na()} checks for missing values in a vector and is equivalent
#' to \code{\link{base::is.na()}}. It is a vectorised predicate, meaning that its
#' output is always the same length as its input. On the other hand,
#' \code{is_na()} is a scalar predicate and always returns a scalar
#' boolean, \code{TRUE} or \code{FALSE}. If its input is not scalar, it returns
#' \code{FALSE}. Finally, there are typed versions that check for
#' particular \link{missing types}.
#' 
#' 
#' @returns 
#' - Calls to \code{is_dbl_na} are guaranteed to return a scalar boolean (ie. a single \code{TRUE} or \code{FALSE} value). If an argument of length > 1 is given, \code{FALSE} is returned.
#' - \code{are_dbl_na} is a wrapper around \code{\link[purrr]{map_lgl}(vec, \(i) is_dbl_na(i, ...))}. A boolean vector of the same length as the input is guaranteed.
#' - Calls to \code{is_not_dbl_na}/\code{are_not_dbl_na} negate the output of \code{is_dbl_na}/\code{are_dbl_na}.
#' @md
#'
#' @param x An object to test
#' @name is_dbl_na
#' @importFrom rlang is_dbl_na
#' @importFrom purrr map_lgl
#' @export
is_dbl_na <- ensure_atomic_boolean('is_dbl_na', 'rlang')
#' 
#' @rdname is_dbl_na
#' @export
is_not_dbl_na <- function(x) !is_dbl_na(x)
#' 
#' @rdname is_dbl_na
#' @export
are_dbl_na <- function(x) {
      purrr::map_lgl(x, \(x) is_dbl_na(x = x))
    }
#' 
#' @rdname is_dbl_na
#' @export
are_not_dbl_na <- function(x) !are_dbl_na(x = x)


#' @aliases are_bare_integer, is_not_bare_integer, are_not_bare_integer

#' @title Bare type predicates
#'
#' @description This is a re-export of \code{\link[rlang:bare-type-predicates]{rlang::is_bare_integer()}}, modified to have standardised naming and standardised vector handling.
#' Documentation is atuomatically generated from the original package documentation. See the \code{\link[rlang:bare-type-predicates]{original}} for full details.
#' 
#' These predicates check for a given type but only return \code{TRUE} for
#' bare R objects. Bare objects have no class attributes. For example,
#' a data frame is a list, but not a bare list.
#' 
#' 
#' @returns 
#' - Calls to \code{is_bare_integer} are guaranteed to return a scalar boolean (ie. a single \code{TRUE} or \code{FALSE} value). If an argument of length > 1 is given, \code{FALSE} is returned.
#' - \code{are_bare_integer} is a wrapper around \code{\link[purrr]{map_lgl}(vec, \(i) is_bare_integer(i, ...))}. A boolean vector of the same length as the input is guaranteed.
#' - Calls to \code{is_not_bare_integer}/\code{are_not_bare_integer} negate the output of \code{is_bare_integer}/\code{are_bare_integer}.
#' @md
#'
#' @param x Object to be tested.
#' @param n Expected length of a vector.
#' @name is_bare_integer
#' @importFrom rlang is_bare_integer
#' @importFrom purrr map_lgl
#' @export
is_bare_integer <- ensure_atomic_boolean('is_bare_integer', 'rlang')
#' 
#' @rdname is_bare_integer
#' @export
is_not_bare_integer <- function(x, n) !is_bare_integer(x, n)
#' 
#' @rdname is_bare_integer
#' @export
are_bare_integer <- function(x, n) {
      purrr::map_lgl(x, \(x) is_bare_integer(x = x, n = n))
    }
#' 
#' @rdname is_bare_integer
#' @export
are_not_bare_integer <- function(x, n) !are_bare_integer(x = x, n = n)


#' @aliases are_copyable, is_not_copyable, are_not_copyable

#' @title Is an object copyable?
#'
#' @description This is a re-export of \code{\link[rlang:is_copyable]{rlang::is_copyable()}}, modified to have standardised naming and standardised vector handling.
#' Documentation is atuomatically generated from the original package documentation. See the \code{\link[rlang:is_copyable]{original}} for full details.
#' 
#' When an object is modified, R generally copies it (sometimes
#' lazily) to enforce \href{https://en.wikipedia.org/wiki/Value_semantics}{value semantics}.
#' However, some internal types are uncopyable. If you try to copy
#' them, either with \verb{<-} or by argument passing, you actually create
#' references to the original object rather than actual
#' copies. Modifying these references can thus have far reaching side
#' effects.
#' 
#' 
#' @returns 
#' - Calls to \code{is_copyable} are guaranteed to return a scalar boolean (ie. a single \code{TRUE} or \code{FALSE} value). If an argument of length > 1 is given, \code{FALSE} is returned.
#' - \code{are_copyable} is a wrapper around \code{\link[purrr]{map_lgl}(vec, \(i) is_copyable(i, ...))}. A boolean vector of the same length as the input is guaranteed.
#' - Calls to \code{is_not_copyable}/\code{are_not_copyable} negate the output of \code{is_copyable}/\code{are_copyable}.
#' @md
#'
#' @param x An object to test.
#' @name is_copyable
#' @importFrom rlang is_copyable
#' @importFrom purrr map_lgl
#' @export
is_copyable <- ensure_atomic_boolean('is_copyable', 'rlang')
#' 
#' @rdname is_copyable
#' @export
is_not_copyable <- function(x) !is_copyable(x)
#' 
#' @rdname is_copyable
#' @export
are_copyable <- function(x) {
      purrr::map_lgl(x, \(x) is_copyable(x = x))
    }
#' 
#' @rdname is_copyable
#' @export
are_not_copyable <- function(x) !are_copyable(x = x)


#' @aliases are_primitive_eager, is_not_primitive_eager, are_not_primitive_eager

#' @title Is object a function?
#'
#' @description This is a re-export of \code{\link[rlang:is_function]{rlang::is_primitive_eager()}}, modified to have standardised naming and standardised vector handling.
#' Documentation is atuomatically generated from the original package documentation. See the \code{\link[rlang:is_function]{original}} for full details.
#' 
#' The R language defines two different types of functions: primitive
#' functions, which are low-level, and closures, which are the regular
#' kind of functions.
#' 
#' 
#' @returns 
#' - Calls to \code{is_primitive_eager} are guaranteed to return a scalar boolean (ie. a single \code{TRUE} or \code{FALSE} value). If an argument of length > 1 is given, \code{FALSE} is returned.
#' - \code{are_primitive_eager} is a wrapper around \code{\link[purrr]{map_lgl}(vec, \(i) is_primitive_eager(i, ...))}. A boolean vector of the same length as the input is guaranteed.
#' - Calls to \code{is_not_primitive_eager}/\code{are_not_primitive_eager} negate the output of \code{is_primitive_eager}/\code{are_primitive_eager}.
#' @md
#'
#' @param x Object to be tested.
#' @name is_primitive_eager
#' @importFrom rlang is_primitive_eager
#' @importFrom purrr map_lgl
#' @export
is_primitive_eager <- ensure_atomic_boolean('is_primitive_eager', 'rlang')
#' 
#' @rdname is_primitive_eager
#' @export
is_not_primitive_eager <- function(x) !is_primitive_eager(x)
#' 
#' @rdname is_primitive_eager
#' @export
are_primitive_eager <- function(x) {
      purrr::map_lgl(x, \(x) is_primitive_eager(x = x))
    }
#' 
#' @rdname is_primitive_eager
#' @export
are_not_primitive_eager <- function(x) !are_primitive_eager(x = x)


#' @aliases are_dictionaryish, is_not_dictionaryish, are_not_dictionaryish

#' @title Is a vector uniquely named?
#'
#' @description This is a re-export of \code{\link[rlang:is_dictionaryish]{rlang::is_dictionaryish()}}, modified to have standardised naming and standardised vector handling.
#' Documentation is atuomatically generated from the original package documentation. See the \code{\link[rlang:is_dictionaryish]{original}} for full details.
#' 
#' Like \code{\link{is_named()}} but also checks that names are unique.
#' 
#' 
#' @returns 
#' - Calls to \code{is_dictionaryish} are guaranteed to return a scalar boolean (ie. a single \code{TRUE} or \code{FALSE} value). If an argument of length > 1 is given, \code{FALSE} is returned.
#' - \code{are_dictionaryish} is a wrapper around \code{\link[purrr]{map_lgl}(vec, \(i) is_dictionaryish(i, ...))}. A boolean vector of the same length as the input is guaranteed.
#' - Calls to \code{is_not_dictionaryish}/\code{are_not_dictionaryish} negate the output of \code{is_dictionaryish}/\code{are_dictionaryish}.
#' @md
#'
#' @param x A vector.
#' @name is_dictionaryish
#' @importFrom rlang is_dictionaryish
#' @importFrom purrr map_lgl
#' @export
is_dictionaryish <- ensure_atomic_boolean('is_dictionaryish', 'rlang')
#' 
#' @rdname is_dictionaryish
#' @export
is_not_dictionaryish <- function(x) !is_dictionaryish(x)
#' 
#' @rdname is_dictionaryish
#' @export
are_dictionaryish <- function(x) {
      purrr::map_lgl(x, \(x) is_dictionaryish(x = x))
    }
#' 
#' @rdname is_dictionaryish
#' @export
are_not_dictionaryish <- function(x) !are_dictionaryish(x = x)


#' @aliases are_quosure, is_not_quosure, are_not_quosure

#' @title Create a quosure from components
#'
#' @description This is a re-export of \code{\link[rlang:new_quosure]{rlang::is_quosure()}}, modified to have standardised naming and standardised vector handling.
#' Documentation is atuomatically generated from the original package documentation. See the \code{\link[rlang:new_quosure]{original}} for full details.
#' 
#' \itemize{
#' \item{ \code{new_quosure()} wraps any R object (including expressions,
#' formulas, or other quosures) into a \link{quosure}.
#' }\item{ \code{as_quosure()} is similar but it does not rewrap formulas and
#' quosures.
#' }}.
#' 
#' 
#' @returns 
#' - Calls to \code{is_quosure} are guaranteed to return a scalar boolean (ie. a single \code{TRUE} or \code{FALSE} value). If an argument of length > 1 is given, \code{FALSE} is returned.
#' - \code{are_quosure} is a wrapper around \code{\link[purrr]{map_lgl}(vec, \(i) is_quosure(i, ...))}. A boolean vector of the same length as the input is guaranteed.
#' - Calls to \code{is_not_quosure}/\code{are_not_quosure} negate the output of \code{is_quosure}/\code{are_quosure}.
#' @md
#'
#' @param x An object to test.
#' @name is_quosure
#' @importFrom rlang is_quosure
#' @importFrom purrr map_lgl
#' @export
is_quosure <- ensure_atomic_boolean('is_quosure', 'rlang')
#' 
#' @rdname is_quosure
#' @export
is_not_quosure <- function(x) !is_quosure(x)
#' 
#' @rdname is_quosure
#' @export
are_quosure <- function(x) {
      purrr::map_lgl(x, \(x) is_quosure(x = x))
    }
#' 
#' @rdname is_quosure
#' @export
are_not_quosure <- function(x) !are_quosure(x = x)


#' @aliases are_scalar_complex, is_not_scalar_complex, are_not_scalar_complex

#' @title Scalar type predicates
#'
#' @description This is a re-export of \code{\link[rlang:scalar-type-predicates]{rlang::is_scalar_complex()}}, modified to have standardised naming and standardised vector handling.
#' Documentation is atuomatically generated from the original package documentation. See the \code{\link[rlang:scalar-type-predicates]{original}} for full details.
#' 
#' These predicates check for a given type and whether the vector is
#' "scalar", that is, of length 1.
#' 
#' In addition to the length check, \code{is_string()} and \code{is_bool()}
#' return \code{FALSE} if their input is missing. This is useful for
#' type-checking arguments, when your function expects a single string
#' or a single \code{TRUE} or \code{FALSE}.
#' 
#' 
#' @returns 
#' - Calls to \code{is_scalar_complex} are guaranteed to return a scalar boolean (ie. a single \code{TRUE} or \code{FALSE} value). If an argument of length > 1 is given, \code{FALSE} is returned.
#' - \code{are_scalar_complex} is a wrapper around \code{\link[purrr]{map_lgl}(vec, \(i) is_scalar_complex(i, ...))}. A boolean vector of the same length as the input is guaranteed.
#' - Calls to \code{is_not_scalar_complex}/\code{are_not_scalar_complex} negate the output of \code{is_scalar_complex}/\code{are_scalar_complex}.
#' @md
#'
#' @param x object to be tested.
#' @name is_scalar_complex
#' @importFrom rlang is_scalar_complex
#' @importFrom purrr map_lgl
#' @export
is_scalar_complex <- ensure_atomic_boolean('is_scalar_complex', 'rlang')
#' 
#' @rdname is_scalar_complex
#' @export
is_not_scalar_complex <- function(x) !is_scalar_complex(x)
#' 
#' @rdname is_scalar_complex
#' @export
are_scalar_complex <- function(x) {
      purrr::map_lgl(x, \(x) is_scalar_complex(x = x))
    }
#' 
#' @rdname is_scalar_complex
#' @export
are_not_scalar_complex <- function(x) !are_scalar_complex(x = x)


#' @aliases are_box, is_not_box, are_not_box

#' @title Box a value
#'
#' @description This is a re-export of \code{\link[rlang:box]{rlang::is_box()}}, modified to have standardised naming and standardised vector handling.
#' Documentation is atuomatically generated from the original package documentation. See the \code{\link[rlang:box]{original}} for full details.
#' 
#' \code{new_box()} is similar to \code{\link{base::I()}} but it protects a value by
#' wrapping it in a scalar list rather than by adding an attribute.
#' \code{unbox()} retrieves the boxed value. \code{is_box()} tests whether an
#' object is boxed with optional class. \code{as_box()} ensures that a
#' value is wrapped in a box. \code{as_box_if()} does the same but only if
#' the value matches a predicate.
#' 
#' 
#' @returns 
#' - Calls to \code{is_box} are guaranteed to return a scalar boolean (ie. a single \code{TRUE} or \code{FALSE} value). If an argument of length > 1 is given, \code{FALSE} is returned.
#' - \code{are_box} is a wrapper around \code{\link[purrr]{map_lgl}(vec, \(i) is_box(i, ...))}. A boolean vector of the same length as the input is guaranteed.
#' - Calls to \code{is_not_box}/\code{are_not_box} negate the output of \code{is_box}/\code{are_box}.
#' @md
#'
#' @param class For \code{new_box()}, an additional class for the
#' boxed value (in addition to \code{rlang_box}). For \code{is_box()}, a class
#' or vector of classes passed to \code{\link{inherits_all()}}.
#' @param x An R object.
#' @name is_box
#' @importFrom rlang is_box
#' @importFrom purrr map_lgl
#' @export
is_box <- ensure_atomic_boolean('is_box', 'rlang')
#' 
#' @rdname is_box
#' @export
is_not_box <- function(x, class) !is_box(x, class)
#' 
#' @rdname is_box
#' @export
are_box <- function(x, class) {
      purrr::map_lgl(x, \(x) is_box(x = x, class = class))
    }
#' 
#' @rdname is_box
#' @export
are_not_box <- function(x, class) !are_box(x = x, class = class)


#' @aliases are_int_na, is_not_int_na, are_not_int_na

#' @title Test for missing values
#'
#' @description This is a re-export of \code{\link[rlang:are_na]{rlang::is_int_na()}}, modified to have standardised naming and standardised vector handling.
#' Documentation is atuomatically generated from the original package documentation. See the \code{\link[rlang:are_na]{original}} for full details.
#' 
#' \ifelse{html}{\href{https://lifecycle.r-lib.org/articles/stages.html#questioning}{\figure{lifecycle-questioning.svg}{options: alt='[Questioning]'}}}{\strong{[Questioning]}}
#' 
#' \code{are_na()} checks for missing values in a vector and is equivalent
#' to \code{\link{base::is.na()}}. It is a vectorised predicate, meaning that its
#' output is always the same length as its input. On the other hand,
#' \code{is_na()} is a scalar predicate and always returns a scalar
#' boolean, \code{TRUE} or \code{FALSE}. If its input is not scalar, it returns
#' \code{FALSE}. Finally, there are typed versions that check for
#' particular \link{missing types}.
#' 
#' 
#' @returns 
#' - Calls to \code{is_int_na} are guaranteed to return a scalar boolean (ie. a single \code{TRUE} or \code{FALSE} value). If an argument of length > 1 is given, \code{FALSE} is returned.
#' - \code{are_int_na} is a wrapper around \code{\link[purrr]{map_lgl}(vec, \(i) is_int_na(i, ...))}. A boolean vector of the same length as the input is guaranteed.
#' - Calls to \code{is_not_int_na}/\code{are_not_int_na} negate the output of \code{is_int_na}/\code{are_int_na}.
#' @md
#'
#' @param x An object to test
#' @name is_int_na
#' @importFrom rlang is_int_na
#' @importFrom purrr map_lgl
#' @export
is_int_na <- ensure_atomic_boolean('is_int_na', 'rlang')
#' 
#' @rdname is_int_na
#' @export
is_not_int_na <- function(x) !is_int_na(x)
#' 
#' @rdname is_int_na
#' @export
are_int_na <- function(x) {
      purrr::map_lgl(x, \(x) is_int_na(x = x))
    }
#' 
#' @rdname is_int_na
#' @export
are_not_int_na <- function(x) !are_int_na(x = x)


#' @aliases are_reference, is_not_reference, are_not_reference

#' @title Is an object referencing another?
#'
#' @description This is a re-export of \code{\link[rlang:is_reference]{rlang::is_reference()}}, modified to have standardised naming and standardised vector handling.
#' Documentation is atuomatically generated from the original package documentation. See the \code{\link[rlang:is_reference]{original}} for full details.
#' 
#' There are typically two situations where two symbols may refer to
#' the same object.
#' \itemize{
#' \item{ R objects usually have copy-on-write semantics. This is an
#' optimisation that ensures that objects are only copied if
#' needed. When you copy a vector, no memory is actually copied
#' until you modify either the original object or the copy is
#' modified.
#' 
#' Note that the copy-on-write optimisation is an implementation
#' detail that is not guaranteed by the specification of the R
#' language.
#' }\item{ Assigning an \link{uncopyable} object (like an
#' environment) creates a reference. These objects are never copied
#' even if you modify one of the references.
#' }}.
#' 
#' 
#' @returns 
#' - Calls to \code{is_reference} are guaranteed to return a scalar boolean (ie. a single \code{TRUE} or \code{FALSE} value). If an argument of length > 1 is given, \code{FALSE} is returned.
#' - \code{are_reference} is a wrapper around \code{\link[purrr]{map_lgl}(vec, \(i) is_reference(i, ...))}. A boolean vector of the same length as the input is guaranteed.
#' - Calls to \code{is_not_reference}/\code{are_not_reference} negate the output of \code{is_reference}/\code{are_reference}.
#' @md
#'
#' @param x,y R objects.
#' @name is_reference
#' @importFrom rlang is_reference
#' @importFrom purrr map_lgl
#' @export
is_reference <- ensure_atomic_boolean('is_reference', 'rlang')
#' 
#' @rdname is_reference
#' @export
is_not_reference <- function(x, y) !is_reference(x, y)
#' 
#' @rdname is_reference
#' @export
are_reference <- function(x, y) {
      purrr::map_lgl(x, \(x) is_reference(x = x, y = y))
    }
#' 
#' @rdname is_reference
#' @export
are_not_reference <- function(x, y) !are_reference(x = x, y = y)


#' @aliases are_bare_atomic, is_not_bare_atomic, are_not_bare_atomic

#' @title Bare type predicates
#'
#' @description This is a re-export of \code{\link[rlang:bare-type-predicates]{rlang::is_bare_atomic()}}, modified to have standardised naming and standardised vector handling.
#' Documentation is atuomatically generated from the original package documentation. See the \code{\link[rlang:bare-type-predicates]{original}} for full details.
#' 
#' These predicates check for a given type but only return \code{TRUE} for
#' bare R objects. Bare objects have no class attributes. For example,
#' a data frame is a list, but not a bare list.
#' 
#' 
#' @returns 
#' - Calls to \code{is_bare_atomic} are guaranteed to return a scalar boolean (ie. a single \code{TRUE} or \code{FALSE} value). If an argument of length > 1 is given, \code{FALSE} is returned.
#' - \code{are_bare_atomic} is a wrapper around \code{\link[purrr]{map_lgl}(vec, \(i) is_bare_atomic(i, ...))}. A boolean vector of the same length as the input is guaranteed.
#' - Calls to \code{is_not_bare_atomic}/\code{are_not_bare_atomic} negate the output of \code{is_bare_atomic}/\code{are_bare_atomic}.
#' @md
#'
#' @param x Object to be tested.
#' @param n Expected length of a vector.
#' @name is_bare_atomic
#' @importFrom rlang is_bare_atomic
#' @importFrom purrr map_lgl
#' @export
is_bare_atomic <- ensure_atomic_boolean('is_bare_atomic', 'rlang')
#' 
#' @rdname is_bare_atomic
#' @export
is_not_bare_atomic <- function(x, n) !is_bare_atomic(x, n)
#' 
#' @rdname is_bare_atomic
#' @export
are_bare_atomic <- function(x, n) {
      purrr::map_lgl(x, \(x) is_bare_atomic(x = x, n = n))
    }
#' 
#' @rdname is_bare_atomic
#' @export
are_not_bare_atomic <- function(x, n) !are_bare_atomic(x = x, n = n)


#' @aliases are_bare_integerish, is_not_bare_integerish, are_not_bare_integerish

#' @title Is a vector integer-like?
#'
#' @description This is a re-export of \code{\link[rlang:is_integerish]{rlang::is_bare_integerish()}}, modified to have standardised naming and standardised vector handling.
#' Documentation is atuomatically generated from the original package documentation. See the \code{\link[rlang:is_integerish]{original}} for full details.
#' 
#' These predicates check whether R considers a number vector to be
#' integer-like, according to its own tolerance check (which is in
#' fact delegated to the C library). This function is not adapted to
#' data analysis, see the help for \code{\link{base::is.integer()}} for examples
#' of how to check for whole numbers.
#' 
#' Things to consider when checking for integer-like doubles:
#' \itemize{
#' \item{ This check can be expensive because the whole double vector has
#' to be traversed and checked.
#' }\item{ Large double values may be integerish but may still not be
#' coercible to integer. This is because integers in R only support
#' values up to \code{2^31 - 1} while numbers stored as double can be
#' much larger.
#' }}.
#' 
#' 
#' @returns 
#' - Calls to \code{is_bare_integerish} are guaranteed to return a scalar boolean (ie. a single \code{TRUE} or \code{FALSE} value). If an argument of length > 1 is given, \code{FALSE} is returned.
#' - \code{are_bare_integerish} is a wrapper around \code{\link[purrr]{map_lgl}(vec, \(i) is_bare_integerish(i, ...))}. A boolean vector of the same length as the input is guaranteed.
#' - Calls to \code{is_not_bare_integerish}/\code{are_not_bare_integerish} negate the output of \code{is_bare_integerish}/\code{are_bare_integerish}.
#' @md
#'
#' @param x Object to be tested.
#' @param n Expected length of a vector.
#' @param finite Whether all values of the vector are finite. The
#' non-finite values are \code{NA}, \code{Inf}, \code{-Inf} and \code{NaN}. Setting this
#' to something other than \code{NULL} can be expensive because the whole
#' vector needs to be traversed and checked.
#' @name is_bare_integerish
#' @importFrom rlang is_bare_integerish
#' @importFrom purrr map_lgl
#' @export
is_bare_integerish <- ensure_atomic_boolean('is_bare_integerish', 'rlang')
#' 
#' @rdname is_bare_integerish
#' @export
is_not_bare_integerish <- function(x, n, finite) !is_bare_integerish(x, n, finite)
#' 
#' @rdname is_bare_integerish
#' @export
are_bare_integerish <- function(x, n, finite) {
      purrr::map_lgl(x, \(x) is_bare_integerish(x = x, n = n, finite = finite))
    }
#' 
#' @rdname is_bare_integerish
#' @export
are_not_bare_integerish <- function(x, n, finite) !are_bare_integerish(x = x, n = n, finite = finite)


#' @aliases are_chr_na, is_not_chr_na, are_not_chr_na

#' @title Test for missing values
#'
#' @description This is a re-export of \code{\link[rlang:are_na]{rlang::is_chr_na()}}, modified to have standardised naming and standardised vector handling.
#' Documentation is atuomatically generated from the original package documentation. See the \code{\link[rlang:are_na]{original}} for full details.
#' 
#' \ifelse{html}{\href{https://lifecycle.r-lib.org/articles/stages.html#questioning}{\figure{lifecycle-questioning.svg}{options: alt='[Questioning]'}}}{\strong{[Questioning]}}
#' 
#' \code{are_na()} checks for missing values in a vector and is equivalent
#' to \code{\link{base::is.na()}}. It is a vectorised predicate, meaning that its
#' output is always the same length as its input. On the other hand,
#' \code{is_na()} is a scalar predicate and always returns a scalar
#' boolean, \code{TRUE} or \code{FALSE}. If its input is not scalar, it returns
#' \code{FALSE}. Finally, there are typed versions that check for
#' particular \link{missing types}.
#' 
#' 
#' @returns 
#' - Calls to \code{is_chr_na} are guaranteed to return a scalar boolean (ie. a single \code{TRUE} or \code{FALSE} value). If an argument of length > 1 is given, \code{FALSE} is returned.
#' - \code{are_chr_na} is a wrapper around \code{\link[purrr]{map_lgl}(vec, \(i) is_chr_na(i, ...))}. A boolean vector of the same length as the input is guaranteed.
#' - Calls to \code{is_not_chr_na}/\code{are_not_chr_na} negate the output of \code{is_chr_na}/\code{are_chr_na}.
#' @md
#'
#' @param x An object to test
#' @name is_chr_na
#' @importFrom rlang is_chr_na
#' @importFrom purrr map_lgl
#' @export
is_chr_na <- ensure_atomic_boolean('is_chr_na', 'rlang')
#' 
#' @rdname is_chr_na
#' @export
is_not_chr_na <- function(x) !is_chr_na(x)
#' 
#' @rdname is_chr_na
#' @export
are_chr_na <- function(x) {
      purrr::map_lgl(x, \(x) is_chr_na(x = x))
    }
#' 
#' @rdname is_chr_na
#' @export
are_not_chr_na <- function(x) !are_chr_na(x = x)


#' @aliases are_na, is_not_na, are_not_na

#' @title Test for missing values
#'
#' @description This is a re-export of \code{\link[rlang:are_na]{rlang::is_na()}}, modified to have standardised naming and standardised vector handling.
#' Documentation is atuomatically generated from the original package documentation. See the \code{\link[rlang:are_na]{original}} for full details.
#' 
#' \ifelse{html}{\href{https://lifecycle.r-lib.org/articles/stages.html#questioning}{\figure{lifecycle-questioning.svg}{options: alt='[Questioning]'}}}{\strong{[Questioning]}}
#' 
#' \code{are_na()} checks for missing values in a vector and is equivalent
#' to \code{\link{base::is.na()}}. It is a vectorised predicate, meaning that its
#' output is always the same length as its input. On the other hand,
#' \code{is_na()} is a scalar predicate and always returns a scalar
#' boolean, \code{TRUE} or \code{FALSE}. If its input is not scalar, it returns
#' \code{FALSE}. Finally, there are typed versions that check for
#' particular \link{missing types}.
#' 
#' 
#' @returns 
#' - Calls to \code{is_na} are guaranteed to return a scalar boolean (ie. a single \code{TRUE} or \code{FALSE} value). If an argument of length > 1 is given, \code{FALSE} is returned.
#' - \code{are_na} is a wrapper around \code{\link[purrr]{map_lgl}(vec, \(i) is_na(i, ...))}. A boolean vector of the same length as the input is guaranteed.
#' - Calls to \code{is_not_na}/\code{are_not_na} negate the output of \code{is_na}/\code{are_na}.
#' @md
#'
#' @param x An object to test
#' @name is_na
#' @importFrom rlang is_na
#' @importFrom purrr map_lgl
#' @export
is_na <- ensure_atomic_boolean('is_na', 'rlang')
#' 
#' @rdname is_na
#' @export
is_not_na <- function(x) !is_na(x)
#' 
#' @rdname is_na
#' @export
are_na <- function(x) {
      purrr::map_lgl(x, \(x) is_na(x = x))
    }
#' 
#' @rdname is_na
#' @export
are_not_na <- function(x) !are_na(x = x)


#' @aliases are_condition, is_not_condition, are_not_condition

#' @title Is object a condition?
#'
#' @description This is a re-export of \code{\link[rlang:is_condition]{rlang::is_condition()}}, modified to have standardised naming and standardised vector handling.
#' Documentation is atuomatically generated from the original package documentation. See the \code{\link[rlang:is_condition]{original}} for full details.
#' 
#' Is object a condition?.
#' 
#' 
#' @returns 
#' - Calls to \code{is_condition} are guaranteed to return a scalar boolean (ie. a single \code{TRUE} or \code{FALSE} value). If an argument of length > 1 is given, \code{FALSE} is returned.
#' - \code{are_condition} is a wrapper around \code{\link[purrr]{map_lgl}(vec, \(i) is_condition(i, ...))}. A boolean vector of the same length as the input is guaranteed.
#' - Calls to \code{is_not_condition}/\code{are_not_condition} negate the output of \code{is_condition}/\code{are_condition}.
#' @md
#'
#' @param x An object to test.
#' @name is_condition
#' @importFrom rlang is_condition
#' @importFrom purrr map_lgl
#' @export
is_condition <- ensure_atomic_boolean('is_condition', 'rlang')
#' 
#' @rdname is_condition
#' @export
is_not_condition <- function(x) !is_condition(x)
#' 
#' @rdname is_condition
#' @export
are_condition <- function(x) {
      purrr::map_lgl(x, \(x) is_condition(x = x))
    }
#' 
#' @rdname is_condition
#' @export
are_not_condition <- function(x) !are_condition(x = x)


#' @aliases are_interactive, is_not_interactive, are_not_interactive

#' @title Is R running interactively?
#'
#' @description This is a re-export of \code{\link[rlang:is_interactive]{rlang::is_interactive()}}, modified to have standardised naming and standardised vector handling.
#' Documentation is atuomatically generated from the original package documentation. See the \code{\link[rlang:is_interactive]{original}} for full details.
#' 
#' Like \code{\link{base::interactive()}}, \code{is_interactive()} returns \code{TRUE} when
#' the function runs interactively and \code{FALSE} when it runs in batch
#' mode. It also checks, in this order:
#' \itemize{
#' \item{ The \code{rlang_interactive} global option. If set to a single \code{TRUE}
#' or \code{FALSE}, \code{is_interactive()} returns that value immediately. This
#' escape hatch is useful in unit tests or to manually turn on
#' interactive features in RMarkdown outputs.
#' }\item{ Whether knitr or testthat is in progress, in which case
#' \code{is_interactive()} returns \code{FALSE}.
#' }}
#' 
#' \code{with_interactive()} and \code{local_interactive()} set the global
#' option conveniently.
#' 
#' 
#' @returns 
#' - Calls to \code{is_interactive} are guaranteed to return a scalar boolean (ie. a single \code{TRUE} or \code{FALSE} value). If an argument of length > 1 is given, \code{FALSE} is returned.
#' - \code{are_interactive} is a wrapper around \code{\link[purrr]{map_lgl}(vec, \(i) is_interactive(i, ...))}. A boolean vector of the same length as the input is guaranteed.
#' - Calls to \code{is_not_interactive}/\code{are_not_interactive} negate the output of \code{is_interactive}/\code{are_interactive}.
#' @md
#'
#' @name is_interactive
#' @importFrom rlang is_interactive
#' @importFrom purrr map_lgl
#' @export
is_interactive <- ensure_atomic_boolean('is_interactive', 'rlang')
#' 
#' @rdname is_interactive
#' @export
is_not_interactive <- function() !is_interactive()

#' @aliases are_quosures, is_not_quosures, are_not_quosures

#' @title Create a list of quosures
#'
#' @description This is a re-export of \code{\link[rlang:new_quosures]{rlang::is_quosures()}}, modified to have standardised naming and standardised vector handling.
#' Documentation is atuomatically generated from the original package documentation. See the \code{\link[rlang:new_quosures]{original}} for full details.
#' 
#' This small S3 class provides methods for \code{[} and \code{c()} and ensures
#' the following invariants:
#' \itemize{
#' \item{ The list only contains quosures.
#' }\item{ It is always named, possibly with a vector of empty strings.
#' }}
#' 
#' \code{new_quosures()} takes a list of quosures and adds the \code{quosures}
#' class and a vector of empty names if needed. \code{as_quosures()} calls
#' \code{\link{as_quosure()}} on all elements before creating the \code{quosures}
#' object.
#' 
#' 
#' @returns 
#' - Calls to \code{is_quosures} are guaranteed to return a scalar boolean (ie. a single \code{TRUE} or \code{FALSE} value). If an argument of length > 1 is given, \code{FALSE} is returned.
#' - \code{are_quosures} is a wrapper around \code{\link[purrr]{map_lgl}(vec, \(i) is_quosures(i, ...))}. A boolean vector of the same length as the input is guaranteed.
#' - Calls to \code{is_not_quosures}/\code{are_not_quosures} negate the output of \code{is_quosures}/\code{are_quosures}.
#' @md
#'
#' @param x A list of quosures or objects to coerce to quosures.
#' @name is_quosures
#' @importFrom rlang is_quosures
#' @importFrom purrr map_lgl
#' @export
is_quosures <- ensure_atomic_boolean('is_quosures', 'rlang')
#' 
#' @rdname is_quosures
#' @export
is_not_quosures <- function(x) !is_quosures(x)
#' 
#' @rdname is_quosures
#' @export
are_quosures <- function(x) {
      purrr::map_lgl(x, \(x) is_quosures(x = x))
    }
#' 
#' @rdname is_quosures
#' @export
are_not_quosures <- function(x) !are_quosures(x = x)


#' @aliases are_message, is_not_message, are_not_message

#' @title Is object a condition?
#'
#' @description This is a re-export of \code{\link[rlang:is_condition]{rlang::is_message()}}, modified to have standardised naming and standardised vector handling.
#' Documentation is atuomatically generated from the original package documentation. See the \code{\link[rlang:is_condition]{original}} for full details.
#' 
#' Is object a condition?.
#' 
#' 
#' @returns 
#' - Calls to \code{is_message} are guaranteed to return a scalar boolean (ie. a single \code{TRUE} or \code{FALSE} value). If an argument of length > 1 is given, \code{FALSE} is returned.
#' - \code{are_message} is a wrapper around \code{\link[purrr]{map_lgl}(vec, \(i) is_message(i, ...))}. A boolean vector of the same length as the input is guaranteed.
#' - Calls to \code{is_not_message}/\code{are_not_message} negate the output of \code{is_message}/\code{are_message}.
#' @md
#'
#' @param x An object to test.
#' @name is_message
#' @importFrom rlang is_message
#' @importFrom purrr map_lgl
#' @export
is_message <- ensure_atomic_boolean('is_message', 'rlang')
#' 
#' @rdname is_message
#' @export
is_not_message <- function(x) !is_message(x)
#' 
#' @rdname is_message
#' @export
are_message <- function(x) {
      purrr::map_lgl(x, \(x) is_message(x = x))
    }
#' 
#' @rdname is_message
#' @export
are_not_message <- function(x) !are_message(x = x)


#' @aliases are_bare_logical, is_not_bare_logical, are_not_bare_logical

#' @title Bare type predicates
#'
#' @description This is a re-export of \code{\link[rlang:bare-type-predicates]{rlang::is_bare_logical()}}, modified to have standardised naming and standardised vector handling.
#' Documentation is atuomatically generated from the original package documentation. See the \code{\link[rlang:bare-type-predicates]{original}} for full details.
#' 
#' These predicates check for a given type but only return \code{TRUE} for
#' bare R objects. Bare objects have no class attributes. For example,
#' a data frame is a list, but not a bare list.
#' 
#' 
#' @returns 
#' - Calls to \code{is_bare_logical} are guaranteed to return a scalar boolean (ie. a single \code{TRUE} or \code{FALSE} value). If an argument of length > 1 is given, \code{FALSE} is returned.
#' - \code{are_bare_logical} is a wrapper around \code{\link[purrr]{map_lgl}(vec, \(i) is_bare_logical(i, ...))}. A boolean vector of the same length as the input is guaranteed.
#' - Calls to \code{is_not_bare_logical}/\code{are_not_bare_logical} negate the output of \code{is_bare_logical}/\code{are_bare_logical}.
#' @md
#'
#' @param x Object to be tested.
#' @param n Expected length of a vector.
#' @name is_bare_logical
#' @importFrom rlang is_bare_logical
#' @importFrom purrr map_lgl
#' @export
is_bare_logical <- ensure_atomic_boolean('is_bare_logical', 'rlang')
#' 
#' @rdname is_bare_logical
#' @export
is_not_bare_logical <- function(x, n) !is_bare_logical(x, n)
#' 
#' @rdname is_bare_logical
#' @export
are_bare_logical <- function(x, n) {
      purrr::map_lgl(x, \(x) is_bare_logical(x = x, n = n))
    }
#' 
#' @rdname is_bare_logical
#' @export
are_not_bare_logical <- function(x, n) !are_bare_logical(x = x, n = n)


#' @aliases are_bare_formula, is_not_bare_formula, are_not_bare_formula

#' @title Is object a formula?
#'
#' @description This is a re-export of \code{\link[rlang:is_formula]{rlang::is_bare_formula()}}, modified to have standardised naming and standardised vector handling.
#' Documentation is atuomatically generated from the original package documentation. See the \code{\link[rlang:is_formula]{original}} for full details.
#' 
#' \code{is_formula()} tests whether \code{x} is a call to \code{~}. \code{is_bare_formula()}
#' tests in addition that \code{x} does not inherit from anything else than
#' \code{"formula"}.
#' 
#' \strong{Note}: When we first implemented \code{is_formula()}, we thought it
#' best to treat unevaluated formulas as formulas by default (see
#' section below). Now we think this default introduces too many edge
#' cases in normal code. We recommend always supplying \code{scoped = TRUE}. Unevaluated formulas can be handled via a \code{is_call(x, "~")}
#' branch.
#' 
#' 
#' @returns 
#' - Calls to \code{is_bare_formula} are guaranteed to return a scalar boolean (ie. a single \code{TRUE} or \code{FALSE} value). If an argument of length > 1 is given, \code{FALSE} is returned.
#' - \code{are_bare_formula} is a wrapper around \code{\link[purrr]{map_lgl}(vec, \(i) is_bare_formula(i, ...))}. A boolean vector of the same length as the input is guaranteed.
#' - Calls to \code{is_not_bare_formula}/\code{are_not_bare_formula} negate the output of \code{is_bare_formula}/\code{are_bare_formula}.
#' @md
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
#' @importFrom purrr map_lgl
#' @export
is_bare_formula <- ensure_atomic_boolean('is_bare_formula', 'rlang')
#' 
#' @rdname is_bare_formula
#' @export
is_not_bare_formula <- function(x, scoped, lhs) !is_bare_formula(x, scoped, lhs)
#' 
#' @rdname is_bare_formula
#' @export
are_bare_formula <- function(x, scoped, lhs) {
      purrr::map_lgl(x, \(x) is_bare_formula(x = x, scoped = scoped, lhs = lhs))
    }
#' 
#' @rdname is_bare_formula
#' @export
are_not_bare_formula <- function(x, scoped, lhs) !are_bare_formula(x = x, scoped = scoped, lhs = lhs)


#' @aliases are_scalar_vector, is_not_scalar_vector, are_not_scalar_vector

#' @title Scalar type predicates
#'
#' @description This is a re-export of \code{\link[rlang:scalar-type-predicates]{rlang::is_scalar_vector()}}, modified to have standardised naming and standardised vector handling.
#' Documentation is atuomatically generated from the original package documentation. See the \code{\link[rlang:scalar-type-predicates]{original}} for full details.
#' 
#' These predicates check for a given type and whether the vector is
#' "scalar", that is, of length 1.
#' 
#' In addition to the length check, \code{is_string()} and \code{is_bool()}
#' return \code{FALSE} if their input is missing. This is useful for
#' type-checking arguments, when your function expects a single string
#' or a single \code{TRUE} or \code{FALSE}.
#' 
#' 
#' @returns 
#' - Calls to \code{is_scalar_vector} are guaranteed to return a scalar boolean (ie. a single \code{TRUE} or \code{FALSE} value). If an argument of length > 1 is given, \code{FALSE} is returned.
#' - \code{are_scalar_vector} is a wrapper around \code{\link[purrr]{map_lgl}(vec, \(i) is_scalar_vector(i, ...))}. A boolean vector of the same length as the input is guaranteed.
#' - Calls to \code{is_not_scalar_vector}/\code{are_not_scalar_vector} negate the output of \code{is_scalar_vector}/\code{are_scalar_vector}.
#' @md
#'
#' @param x object to be tested.
#' @name is_scalar_vector
#' @importFrom rlang is_scalar_vector
#' @importFrom purrr map_lgl
#' @export
is_scalar_vector <- ensure_atomic_boolean('is_scalar_vector', 'rlang')
#' 
#' @rdname is_scalar_vector
#' @export
is_not_scalar_vector <- function(x) !is_scalar_vector(x)
#' 
#' @rdname is_scalar_vector
#' @export
are_scalar_vector <- function(x) {
      purrr::map_lgl(x, \(x) is_scalar_vector(x = x))
    }
#' 
#' @rdname is_scalar_vector
#' @export
are_not_scalar_vector <- function(x) !are_scalar_vector(x = x)


#' @aliases are_warning, is_not_warning, are_not_warning

#' @title Is object a condition?
#'
#' @description This is a re-export of \code{\link[rlang:is_condition]{rlang::is_warning()}}, modified to have standardised naming and standardised vector handling.
#' Documentation is atuomatically generated from the original package documentation. See the \code{\link[rlang:is_condition]{original}} for full details.
#' 
#' Is object a condition?.
#' 
#' 
#' @returns 
#' - Calls to \code{is_warning} are guaranteed to return a scalar boolean (ie. a single \code{TRUE} or \code{FALSE} value). If an argument of length > 1 is given, \code{FALSE} is returned.
#' - \code{are_warning} is a wrapper around \code{\link[purrr]{map_lgl}(vec, \(i) is_warning(i, ...))}. A boolean vector of the same length as the input is guaranteed.
#' - Calls to \code{is_not_warning}/\code{are_not_warning} negate the output of \code{is_warning}/\code{are_warning}.
#' @md
#'
#' @param x An object to test.
#' @name is_warning
#' @importFrom rlang is_warning
#' @importFrom purrr map_lgl
#' @export
is_warning <- ensure_atomic_boolean('is_warning', 'rlang')
#' 
#' @rdname is_warning
#' @export
is_not_warning <- function(x) !is_warning(x)
#' 
#' @rdname is_warning
#' @export
are_warning <- function(x) {
      purrr::map_lgl(x, \(x) is_warning(x = x))
    }
#' 
#' @rdname is_warning
#' @export
are_not_warning <- function(x) !are_warning(x = x)


#' @aliases are_error, is_not_error, are_not_error

#' @title Is object a condition?
#'
#' @description This is a re-export of \code{\link[rlang:is_condition]{rlang::is_error()}}, modified to have standardised naming and standardised vector handling.
#' Documentation is atuomatically generated from the original package documentation. See the \code{\link[rlang:is_condition]{original}} for full details.
#' 
#' Is object a condition?.
#' 
#' 
#' @returns 
#' - Calls to \code{is_error} are guaranteed to return a scalar boolean (ie. a single \code{TRUE} or \code{FALSE} value). If an argument of length > 1 is given, \code{FALSE} is returned.
#' - \code{are_error} is a wrapper around \code{\link[purrr]{map_lgl}(vec, \(i) is_error(i, ...))}. A boolean vector of the same length as the input is guaranteed.
#' - Calls to \code{is_not_error}/\code{are_not_error} negate the output of \code{is_error}/\code{are_error}.
#' @md
#'
#' @param x An object to test.
#' @name is_error
#' @importFrom rlang is_error
#' @importFrom purrr map_lgl
#' @export
is_error <- ensure_atomic_boolean('is_error', 'rlang')
#' 
#' @rdname is_error
#' @export
is_not_error <- function(x) !is_error(x)
#' 
#' @rdname is_error
#' @export
are_error <- function(x) {
      purrr::map_lgl(x, \(x) is_error(x = x))
    }
#' 
#' @rdname is_error
#' @export
are_not_error <- function(x) !are_error(x = x)


#' @aliases are_bare_numeric, is_not_bare_numeric, are_not_bare_numeric

#' @title Bare type predicates
#'
#' @description This is a re-export of \code{\link[rlang:bare-type-predicates]{rlang::is_bare_numeric()}}, modified to have standardised naming and standardised vector handling.
#' Documentation is atuomatically generated from the original package documentation. See the \code{\link[rlang:bare-type-predicates]{original}} for full details.
#' 
#' These predicates check for a given type but only return \code{TRUE} for
#' bare R objects. Bare objects have no class attributes. For example,
#' a data frame is a list, but not a bare list.
#' 
#' 
#' @returns 
#' - Calls to \code{is_bare_numeric} are guaranteed to return a scalar boolean (ie. a single \code{TRUE} or \code{FALSE} value). If an argument of length > 1 is given, \code{FALSE} is returned.
#' - \code{are_bare_numeric} is a wrapper around \code{\link[purrr]{map_lgl}(vec, \(i) is_bare_numeric(i, ...))}. A boolean vector of the same length as the input is guaranteed.
#' - Calls to \code{is_not_bare_numeric}/\code{are_not_bare_numeric} negate the output of \code{is_bare_numeric}/\code{are_bare_numeric}.
#' @md
#'
#' @param x Object to be tested.
#' @param n Expected length of a vector.
#' @name is_bare_numeric
#' @importFrom rlang is_bare_numeric
#' @importFrom purrr map_lgl
#' @export
is_bare_numeric <- ensure_atomic_boolean('is_bare_numeric', 'rlang')
#' 
#' @rdname is_bare_numeric
#' @export
is_not_bare_numeric <- function(x, n) !is_bare_numeric(x, n)
#' 
#' @rdname is_bare_numeric
#' @export
are_bare_numeric <- function(x, n) {
      purrr::map_lgl(x, \(x) is_bare_numeric(x = x, n = n))
    }
#' 
#' @rdname is_bare_numeric
#' @export
are_not_bare_numeric <- function(x, n) !are_bare_numeric(x = x, n = n)


#' @aliases are_bare_complex, is_not_bare_complex, are_not_bare_complex

#' @title Bare type predicates
#'
#' @description This is a re-export of \code{\link[rlang:bare-type-predicates]{rlang::is_bare_complex()}}, modified to have standardised naming and standardised vector handling.
#' Documentation is atuomatically generated from the original package documentation. See the \code{\link[rlang:bare-type-predicates]{original}} for full details.
#' 
#' These predicates check for a given type but only return \code{TRUE} for
#' bare R objects. Bare objects have no class attributes. For example,
#' a data frame is a list, but not a bare list.
#' 
#' 
#' @returns 
#' - Calls to \code{is_bare_complex} are guaranteed to return a scalar boolean (ie. a single \code{TRUE} or \code{FALSE} value). If an argument of length > 1 is given, \code{FALSE} is returned.
#' - \code{are_bare_complex} is a wrapper around \code{\link[purrr]{map_lgl}(vec, \(i) is_bare_complex(i, ...))}. A boolean vector of the same length as the input is guaranteed.
#' - Calls to \code{is_not_bare_complex}/\code{are_not_bare_complex} negate the output of \code{is_bare_complex}/\code{are_bare_complex}.
#' @md
#'
#' @param x Object to be tested.
#' @param n Expected length of a vector.
#' @name is_bare_complex
#' @importFrom rlang is_bare_complex
#' @importFrom purrr map_lgl
#' @export
is_bare_complex <- ensure_atomic_boolean('is_bare_complex', 'rlang')
#' 
#' @rdname is_bare_complex
#' @export
is_not_bare_complex <- function(x, n) !is_bare_complex(x, n)
#' 
#' @rdname is_bare_complex
#' @export
are_bare_complex <- function(x, n) {
      purrr::map_lgl(x, \(x) is_bare_complex(x = x, n = n))
    }
#' 
#' @rdname is_bare_complex
#' @export
are_not_bare_complex <- function(x, n) !are_bare_complex(x = x, n = n)


#' @aliases are_lambda, is_not_lambda, are_not_lambda

#' @title Convert to function
#'
#' @description This is a re-export of \code{\link[rlang:as_function]{rlang::is_lambda()}}, modified to have standardised naming and standardised vector handling.
#' Documentation is atuomatically generated from the original package documentation. See the \code{\link[rlang:as_function]{original}} for full details.
#' 
#' \code{as_function()} transforms a one-sided formula into a function.
#' This powers the lambda syntax in packages like purrr.
#' 
#' 
#' @returns 
#' - Calls to \code{is_lambda} are guaranteed to return a scalar boolean (ie. a single \code{TRUE} or \code{FALSE} value). If an argument of length > 1 is given, \code{FALSE} is returned.
#' - \code{are_lambda} is a wrapper around \code{\link[purrr]{map_lgl}(vec, \(i) is_lambda(i, ...))}. A boolean vector of the same length as the input is guaranteed.
#' - Calls to \code{is_not_lambda}/\code{are_not_lambda} negate the output of \code{is_lambda}/\code{are_lambda}.
#' @md
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
#' @importFrom purrr map_lgl
#' @export
is_lambda <- ensure_atomic_boolean('is_lambda', 'rlang')
#' 
#' @rdname is_lambda
#' @export
is_not_lambda <- function(x) !is_lambda(x)
#' 
#' @rdname is_lambda
#' @export
are_lambda <- function(x) {
      purrr::map_lgl(x, \(x) is_lambda(x = x))
    }
#' 
#' @rdname is_lambda
#' @export
are_not_lambda <- function(x) !are_lambda(x = x)


#' @aliases are_bool, is_not_bool, are_not_bool

#' @title Scalar type predicates
#'
#' @description This is a re-export of \code{\link[rlang:scalar-type-predicates]{rlang::is_bool()}}, modified to have standardised naming and standardised vector handling.
#' Documentation is atuomatically generated from the original package documentation. See the \code{\link[rlang:scalar-type-predicates]{original}} for full details.
#' 
#' These predicates check for a given type and whether the vector is
#' "scalar", that is, of length 1.
#' 
#' In addition to the length check, \code{is_string()} and \code{is_bool()}
#' return \code{FALSE} if their input is missing. This is useful for
#' type-checking arguments, when your function expects a single string
#' or a single \code{TRUE} or \code{FALSE}.
#' 
#' 
#' @returns 
#' - Calls to \code{is_bool} are guaranteed to return a scalar boolean (ie. a single \code{TRUE} or \code{FALSE} value). If an argument of length > 1 is given, \code{FALSE} is returned.
#' - \code{are_bool} is a wrapper around \code{\link[purrr]{map_lgl}(vec, \(i) is_bool(i, ...))}. A boolean vector of the same length as the input is guaranteed.
#' - Calls to \code{is_not_bool}/\code{are_not_bool} negate the output of \code{is_bool}/\code{are_bool}.
#' @md
#'
#' @param x object to be tested.
#' @name is_bool
#' @importFrom rlang is_bool
#' @importFrom purrr map_lgl
#' @export
is_bool <- ensure_atomic_boolean('is_bool', 'rlang')
#' 
#' @rdname is_bool
#' @export
is_not_bool <- function(x) !is_bool(x)
#' 
#' @rdname is_bool
#' @export
are_bool <- function(x) {
      purrr::map_lgl(x, \(x) is_bool(x = x))
    }
#' 
#' @rdname is_bool
#' @export
are_not_bool <- function(x) !are_bool(x = x)


#' @aliases are_cpl_na, is_not_cpl_na, are_not_cpl_na

#' @title Test for missing values
#'
#' @description This is a re-export of \code{\link[rlang:are_na]{rlang::is_cpl_na()}}, modified to have standardised naming and standardised vector handling.
#' Documentation is atuomatically generated from the original package documentation. See the \code{\link[rlang:are_na]{original}} for full details.
#' 
#' \ifelse{html}{\href{https://lifecycle.r-lib.org/articles/stages.html#questioning}{\figure{lifecycle-questioning.svg}{options: alt='[Questioning]'}}}{\strong{[Questioning]}}
#' 
#' \code{are_na()} checks for missing values in a vector and is equivalent
#' to \code{\link{base::is.na()}}. It is a vectorised predicate, meaning that its
#' output is always the same length as its input. On the other hand,
#' \code{is_na()} is a scalar predicate and always returns a scalar
#' boolean, \code{TRUE} or \code{FALSE}. If its input is not scalar, it returns
#' \code{FALSE}. Finally, there are typed versions that check for
#' particular \link{missing types}.
#' 
#' 
#' @returns 
#' - Calls to \code{is_cpl_na} are guaranteed to return a scalar boolean (ie. a single \code{TRUE} or \code{FALSE} value). If an argument of length > 1 is given, \code{FALSE} is returned.
#' - \code{are_cpl_na} is a wrapper around \code{\link[purrr]{map_lgl}(vec, \(i) is_cpl_na(i, ...))}. A boolean vector of the same length as the input is guaranteed.
#' - Calls to \code{is_not_cpl_na}/\code{are_not_cpl_na} negate the output of \code{is_cpl_na}/\code{are_cpl_na}.
#' @md
#'
#' @param x An object to test
#' @name is_cpl_na
#' @importFrom rlang is_cpl_na
#' @importFrom purrr map_lgl
#' @export
is_cpl_na <- ensure_atomic_boolean('is_cpl_na', 'rlang')
#' 
#' @rdname is_cpl_na
#' @export
is_not_cpl_na <- function(x) !is_cpl_na(x)
#' 
#' @rdname is_cpl_na
#' @export
are_cpl_na <- function(x) {
      purrr::map_lgl(x, \(x) is_cpl_na(x = x))
    }
#' 
#' @rdname is_cpl_na
#' @export
are_not_cpl_na <- function(x) !are_cpl_na(x = x)


#' @aliases are_node_list, is_not_node_list, are_not_node_list

#' @title Is object a node or pairlist?
#'
#' @description This is a re-export of \code{\link[rlang:is_pairlist]{rlang::is_node_list()}}, modified to have standardised naming and standardised vector handling.
#' Documentation is atuomatically generated from the original package documentation. See the \code{\link[rlang:is_pairlist]{original}} for full details.
#' 
#' \itemize{
#' \item{ \code{is_pairlist()} checks that \code{x} has type \code{pairlist}.
#' }\item{ \code{is_node()} checks that \code{x} has type \code{pairlist} or \code{language}.
#' It tests whether \code{x} is a node that has a CAR and a CDR,
#' including callable nodes (language objects).
#' }\item{ \code{is_node_list()} checks that \code{x} has type \code{pairlist} or \code{NULL}.
#' \code{NULL} is the empty node list.
#' }}.
#' 
#' 
#' @returns 
#' - Calls to \code{is_node_list} are guaranteed to return a scalar boolean (ie. a single \code{TRUE} or \code{FALSE} value). If an argument of length > 1 is given, \code{FALSE} is returned.
#' - \code{are_node_list} is a wrapper around \code{\link[purrr]{map_lgl}(vec, \(i) is_node_list(i, ...))}. A boolean vector of the same length as the input is guaranteed.
#' - Calls to \code{is_not_node_list}/\code{are_not_node_list} negate the output of \code{is_node_list}/\code{are_node_list}.
#' @md
#'
#' @param x Object to test.
#' @name is_node_list
#' @importFrom rlang is_node_list
#' @importFrom purrr map_lgl
#' @export
is_node_list <- ensure_atomic_boolean('is_node_list', 'rlang')
#' 
#' @rdname is_node_list
#' @export
is_not_node_list <- function(x) !is_node_list(x)
#' 
#' @rdname is_node_list
#' @export
are_node_list <- function(x) {
      purrr::map_lgl(x, \(x) is_node_list(x = x))
    }
#' 
#' @rdname is_node_list
#' @export
are_not_node_list <- function(x) !are_node_list(x = x)


#' @aliases are_primitive, is_not_primitive, are_not_primitive

#' @title Is object a function?
#'
#' @description This is a re-export of \code{\link[rlang:is_function]{rlang::is_primitive()}}, modified to have standardised naming and standardised vector handling.
#' Documentation is atuomatically generated from the original package documentation. See the \code{\link[rlang:is_function]{original}} for full details.
#' 
#' The R language defines two different types of functions: primitive
#' functions, which are low-level, and closures, which are the regular
#' kind of functions.
#' 
#' 
#' @returns 
#' - Calls to \code{is_primitive} are guaranteed to return a scalar boolean (ie. a single \code{TRUE} or \code{FALSE} value). If an argument of length > 1 is given, \code{FALSE} is returned.
#' - \code{are_primitive} is a wrapper around \code{\link[purrr]{map_lgl}(vec, \(i) is_primitive(i, ...))}. A boolean vector of the same length as the input is guaranteed.
#' - Calls to \code{is_not_primitive}/\code{are_not_primitive} negate the output of \code{is_primitive}/\code{are_primitive}.
#' @md
#'
#' @param x Object to be tested.
#' @name is_primitive
#' @importFrom rlang is_primitive
#' @importFrom purrr map_lgl
#' @export
is_primitive <- ensure_atomic_boolean('is_primitive', 'rlang')
#' 
#' @rdname is_primitive
#' @export
is_not_primitive <- function(x) !is_primitive(x)
#' 
#' @rdname is_primitive
#' @export
are_primitive <- function(x) {
      purrr::map_lgl(x, \(x) is_primitive(x = x))
    }
#' 
#' @rdname is_primitive
#' @export
are_not_primitive <- function(x) !are_primitive(x = x)


#' @aliases are_callable, is_not_callable, are_not_callable

#' @title Is an object callable?
#'
#' @description This is a re-export of \code{\link[rlang:is_callable]{rlang::is_callable()}}, modified to have standardised naming and standardised vector handling.
#' Documentation is atuomatically generated from the original package documentation. See the \code{\link[rlang:is_callable]{original}} for full details.
#' 
#' A callable object is an object that can appear in the function
#' position of a call (as opposed to argument position). This includes
#' \link{symbolic objects} that evaluate to a function or
#' literal functions embedded in the call.
#' 
#' 
#' @returns 
#' - Calls to \code{is_callable} are guaranteed to return a scalar boolean (ie. a single \code{TRUE} or \code{FALSE} value). If an argument of length > 1 is given, \code{FALSE} is returned.
#' - \code{are_callable} is a wrapper around \code{\link[purrr]{map_lgl}(vec, \(i) is_callable(i, ...))}. A boolean vector of the same length as the input is guaranteed.
#' - Calls to \code{is_not_callable}/\code{are_not_callable} negate the output of \code{is_callable}/\code{are_callable}.
#' @md
#'
#' @param x An object to test.
#' @name is_callable
#' @importFrom rlang is_callable
#' @importFrom purrr map_lgl
#' @export
is_callable <- ensure_atomic_boolean('is_callable', 'rlang')
#' 
#' @rdname is_callable
#' @export
is_not_callable <- function(x) !is_callable(x)
#' 
#' @rdname is_callable
#' @export
are_callable <- function(x) {
      purrr::map_lgl(x, \(x) is_callable(x = x))
    }
#' 
#' @rdname is_callable
#' @export
are_not_callable <- function(x) !are_callable(x = x)


#' @aliases are_scalar_integerish, is_not_scalar_integerish, are_not_scalar_integerish

#' @title Is a vector integer-like?
#'
#' @description This is a re-export of \code{\link[rlang:is_integerish]{rlang::is_scalar_integerish()}}, modified to have standardised naming and standardised vector handling.
#' Documentation is atuomatically generated from the original package documentation. See the \code{\link[rlang:is_integerish]{original}} for full details.
#' 
#' These predicates check whether R considers a number vector to be
#' integer-like, according to its own tolerance check (which is in
#' fact delegated to the C library). This function is not adapted to
#' data analysis, see the help for \code{\link{base::is.integer()}} for examples
#' of how to check for whole numbers.
#' 
#' Things to consider when checking for integer-like doubles:
#' \itemize{
#' \item{ This check can be expensive because the whole double vector has
#' to be traversed and checked.
#' }\item{ Large double values may be integerish but may still not be
#' coercible to integer. This is because integers in R only support
#' values up to \code{2^31 - 1} while numbers stored as double can be
#' much larger.
#' }}.
#' 
#' 
#' @returns 
#' - Calls to \code{is_scalar_integerish} are guaranteed to return a scalar boolean (ie. a single \code{TRUE} or \code{FALSE} value). If an argument of length > 1 is given, \code{FALSE} is returned.
#' - \code{are_scalar_integerish} is a wrapper around \code{\link[purrr]{map_lgl}(vec, \(i) is_scalar_integerish(i, ...))}. A boolean vector of the same length as the input is guaranteed.
#' - Calls to \code{is_not_scalar_integerish}/\code{are_not_scalar_integerish} negate the output of \code{is_scalar_integerish}/\code{are_scalar_integerish}.
#' @md
#'
#' @param x Object to be tested.
#' @param finite Whether all values of the vector are finite. The
#' non-finite values are \code{NA}, \code{Inf}, \code{-Inf} and \code{NaN}. Setting this
#' to something other than \code{NULL} can be expensive because the whole
#' vector needs to be traversed and checked.
#' @name is_scalar_integerish
#' @importFrom rlang is_scalar_integerish
#' @importFrom purrr map_lgl
#' @export
is_scalar_integerish <- ensure_atomic_boolean('is_scalar_integerish', 'rlang')
#' 
#' @rdname is_scalar_integerish
#' @export
is_not_scalar_integerish <- function(x, finite) !is_scalar_integerish(x, finite)
#' 
#' @rdname is_scalar_integerish
#' @export
are_scalar_integerish <- function(x, finite) {
      purrr::map_lgl(x, \(x) is_scalar_integerish(x = x, finite = finite))
    }
#' 
#' @rdname is_scalar_integerish
#' @export
are_not_scalar_integerish <- function(x, finite) !are_scalar_integerish(x = x, finite = finite)


#' @aliases are_symbolic, is_not_symbolic, are_not_symbolic

#' @title Is an object an expression?
#'
#' @description This is a re-export of \code{\link[rlang:is_expression]{rlang::is_symbolic()}}, modified to have standardised naming and standardised vector handling.
#' Documentation is atuomatically generated from the original package documentation. See the \code{\link[rlang:is_expression]{original}} for full details.
#' 
#' In rlang, an \emph{expression} is the return type of \code{\link{parse_expr()}}, the
#' set of objects that can be obtained from parsing R code. Under this
#' definition expressions include numbers, strings, \code{NULL}, symbols,
#' and function calls. These objects can be classified as:
#' \itemize{
#' \item{ Symbolic objects, i.e. symbols and function calls (for which
#' \code{is_symbolic()} returns \code{TRUE})
#' }\item{ Syntactic literals, i.e. scalar atomic objects and \code{NULL}
#' (testable with \code{is_syntactic_literal()})
#' }}
#' 
#' \code{is_expression()} returns \code{TRUE} if the input is either a symbolic
#' object or a syntactic literal. If a call, the elements of the call
#' must all be expressions as well. Unparsable calls are not
#' considered expressions in this narrow definition.
#' 
#' Note that in base R, there exists \code{\link{expression()}} vectors, a data
#' type similar to a list that supports special attributes created by
#' the parser called source references. This data type is not
#' supported in rlang.
#' 
#' 
#' @returns 
#' - Calls to \code{is_symbolic} are guaranteed to return a scalar boolean (ie. a single \code{TRUE} or \code{FALSE} value). If an argument of length > 1 is given, \code{FALSE} is returned.
#' - \code{are_symbolic} is a wrapper around \code{\link[purrr]{map_lgl}(vec, \(i) is_symbolic(i, ...))}. A boolean vector of the same length as the input is guaranteed.
#' - Calls to \code{is_not_symbolic}/\code{are_not_symbolic} negate the output of \code{is_symbolic}/\code{are_symbolic}.
#' @md
#'
#' @param x An object to test.
#' @name is_symbolic
#' @importFrom rlang is_symbolic
#' @importFrom purrr map_lgl
#' @export
is_symbolic <- ensure_atomic_boolean('is_symbolic', 'rlang')
#' 
#' @rdname is_symbolic
#' @export
is_not_symbolic <- function(x) !is_symbolic(x)
#' 
#' @rdname is_symbolic
#' @export
are_symbolic <- function(x) {
      purrr::map_lgl(x, \(x) is_symbolic(x = x))
    }
#' 
#' @rdname is_symbolic
#' @export
are_not_symbolic <- function(x) !are_symbolic(x = x)


#' @aliases are_symbol, is_not_symbol, are_not_symbol

#' @title Is object a symbol?
#'
#' @description This is a re-export of \code{\link[rlang:is_symbol]{rlang::is_symbol()}}, modified to have standardised naming and standardised vector handling.
#' Documentation is atuomatically generated from the original package documentation. See the \code{\link[rlang:is_symbol]{original}} for full details.
#' 
#' Is object a symbol?.
#' 
#' 
#' @returns 
#' - Calls to \code{is_symbol} are guaranteed to return a scalar boolean (ie. a single \code{TRUE} or \code{FALSE} value). If an argument of length > 1 is given, \code{FALSE} is returned.
#' - \code{are_symbol} is a wrapper around \code{\link[purrr]{map_lgl}(vec, \(i) is_symbol(i, ...))}. A boolean vector of the same length as the input is guaranteed.
#' - Calls to \code{is_not_symbol}/\code{are_not_symbol} negate the output of \code{is_symbol}/\code{are_symbol}.
#' @md
#'
#' @param x An object to test.
#' @param name An optional name or vector of names that the symbol
#' should match.
#' @name is_symbol
#' @importFrom rlang is_symbol
#' @importFrom purrr map_lgl
#' @export
is_symbol <- ensure_atomic_boolean('is_symbol', 'rlang')
#' 
#' @rdname is_symbol
#' @export
is_not_symbol <- function(x, name) !is_symbol(x, name)
#' 
#' @rdname is_symbol
#' @export
are_symbol <- function(x, name) {
      purrr::map_lgl(x, \(x) is_symbol(x = x, name = name))
    }
#' 
#' @rdname is_symbol
#' @export
are_not_symbol <- function(x, name) !are_symbol(x = x, name = name)


#' @aliases are_node, is_not_node, are_not_node

#' @title Is object a node or pairlist?
#'
#' @description This is a re-export of \code{\link[rlang:is_pairlist]{rlang::is_node()}}, modified to have standardised naming and standardised vector handling.
#' Documentation is atuomatically generated from the original package documentation. See the \code{\link[rlang:is_pairlist]{original}} for full details.
#' 
#' \itemize{
#' \item{ \code{is_pairlist()} checks that \code{x} has type \code{pairlist}.
#' }\item{ \code{is_node()} checks that \code{x} has type \code{pairlist} or \code{language}.
#' It tests whether \code{x} is a node that has a CAR and a CDR,
#' including callable nodes (language objects).
#' }\item{ \code{is_node_list()} checks that \code{x} has type \code{pairlist} or \code{NULL}.
#' \code{NULL} is the empty node list.
#' }}.
#' 
#' 
#' @returns 
#' - Calls to \code{is_node} are guaranteed to return a scalar boolean (ie. a single \code{TRUE} or \code{FALSE} value). If an argument of length > 1 is given, \code{FALSE} is returned.
#' - \code{are_node} is a wrapper around \code{\link[purrr]{map_lgl}(vec, \(i) is_node(i, ...))}. A boolean vector of the same length as the input is guaranteed.
#' - Calls to \code{is_not_node}/\code{are_not_node} negate the output of \code{is_node}/\code{are_node}.
#' @md
#'
#' @param x Object to test.
#' @name is_node
#' @importFrom rlang is_node
#' @importFrom purrr map_lgl
#' @export
is_node <- ensure_atomic_boolean('is_node', 'rlang')
#' 
#' @rdname is_node
#' @export
is_not_node <- function(x) !is_node(x)
#' 
#' @rdname is_node
#' @export
are_node <- function(x) {
      purrr::map_lgl(x, \(x) is_node(x = x))
    }
#' 
#' @rdname is_node
#' @export
are_not_node <- function(x) !are_node(x = x)


#' @aliases are_scalar_raw, is_not_scalar_raw, are_not_scalar_raw

#' @title Scalar type predicates
#'
#' @description This is a re-export of \code{\link[rlang:scalar-type-predicates]{rlang::is_scalar_raw()}}, modified to have standardised naming and standardised vector handling.
#' Documentation is atuomatically generated from the original package documentation. See the \code{\link[rlang:scalar-type-predicates]{original}} for full details.
#' 
#' These predicates check for a given type and whether the vector is
#' "scalar", that is, of length 1.
#' 
#' In addition to the length check, \code{is_string()} and \code{is_bool()}
#' return \code{FALSE} if their input is missing. This is useful for
#' type-checking arguments, when your function expects a single string
#' or a single \code{TRUE} or \code{FALSE}.
#' 
#' 
#' @returns 
#' - Calls to \code{is_scalar_raw} are guaranteed to return a scalar boolean (ie. a single \code{TRUE} or \code{FALSE} value). If an argument of length > 1 is given, \code{FALSE} is returned.
#' - \code{are_scalar_raw} is a wrapper around \code{\link[purrr]{map_lgl}(vec, \(i) is_scalar_raw(i, ...))}. A boolean vector of the same length as the input is guaranteed.
#' - Calls to \code{is_not_scalar_raw}/\code{are_not_scalar_raw} negate the output of \code{is_scalar_raw}/\code{are_scalar_raw}.
#' @md
#'
#' @param x object to be tested.
#' @name is_scalar_raw
#' @importFrom rlang is_scalar_raw
#' @importFrom purrr map_lgl
#' @export
is_scalar_raw <- ensure_atomic_boolean('is_scalar_raw', 'rlang')
#' 
#' @rdname is_scalar_raw
#' @export
is_not_scalar_raw <- function(x) !is_scalar_raw(x)
#' 
#' @rdname is_scalar_raw
#' @export
are_scalar_raw <- function(x) {
      purrr::map_lgl(x, \(x) is_scalar_raw(x = x))
    }
#' 
#' @rdname is_scalar_raw
#' @export
are_not_scalar_raw <- function(x) !are_scalar_raw(x = x)


#' @aliases are_expression, is_not_expression, are_not_expression

#' @title Is an object an expression?
#'
#' @description This is a re-export of \code{\link[rlang:is_expression]{rlang::is_expression()}}, modified to have standardised naming and standardised vector handling.
#' Documentation is atuomatically generated from the original package documentation. See the \code{\link[rlang:is_expression]{original}} for full details.
#' 
#' In rlang, an \emph{expression} is the return type of \code{\link{parse_expr()}}, the
#' set of objects that can be obtained from parsing R code. Under this
#' definition expressions include numbers, strings, \code{NULL}, symbols,
#' and function calls. These objects can be classified as:
#' \itemize{
#' \item{ Symbolic objects, i.e. symbols and function calls (for which
#' \code{is_symbolic()} returns \code{TRUE})
#' }\item{ Syntactic literals, i.e. scalar atomic objects and \code{NULL}
#' (testable with \code{is_syntactic_literal()})
#' }}
#' 
#' \code{is_expression()} returns \code{TRUE} if the input is either a symbolic
#' object or a syntactic literal. If a call, the elements of the call
#' must all be expressions as well. Unparsable calls are not
#' considered expressions in this narrow definition.
#' 
#' Note that in base R, there exists \code{\link{expression()}} vectors, a data
#' type similar to a list that supports special attributes created by
#' the parser called source references. This data type is not
#' supported in rlang.
#' 
#' 
#' @returns 
#' - Calls to \code{is_expression} are guaranteed to return a scalar boolean (ie. a single \code{TRUE} or \code{FALSE} value). If an argument of length > 1 is given, \code{FALSE} is returned.
#' - \code{are_expression} is a wrapper around \code{\link[purrr]{map_lgl}(vec, \(i) is_expression(i, ...))}. A boolean vector of the same length as the input is guaranteed.
#' - Calls to \code{is_not_expression}/\code{are_not_expression} negate the output of \code{is_expression}/\code{are_expression}.
#' @md
#'
#' @param x An object to test.
#' @name is_expression
#' @importFrom rlang is_expression
#' @importFrom purrr map_lgl
#' @export
is_expression <- ensure_atomic_boolean('is_expression', 'rlang')
#' 
#' @rdname is_expression
#' @export
is_not_expression <- function(x) !is_expression(x)
#' 
#' @rdname is_expression
#' @export
are_expression <- function(x) {
      purrr::map_lgl(x, \(x) is_expression(x = x))
    }
#' 
#' @rdname is_expression
#' @export
are_not_expression <- function(x) !are_expression(x = x)


#' @aliases are_bare_raw, is_not_bare_raw, are_not_bare_raw

#' @title Bare type predicates
#'
#' @description This is a re-export of \code{\link[rlang:bare-type-predicates]{rlang::is_bare_raw()}}, modified to have standardised naming and standardised vector handling.
#' Documentation is atuomatically generated from the original package documentation. See the \code{\link[rlang:bare-type-predicates]{original}} for full details.
#' 
#' These predicates check for a given type but only return \code{TRUE} for
#' bare R objects. Bare objects have no class attributes. For example,
#' a data frame is a list, but not a bare list.
#' 
#' 
#' @returns 
#' - Calls to \code{is_bare_raw} are guaranteed to return a scalar boolean (ie. a single \code{TRUE} or \code{FALSE} value). If an argument of length > 1 is given, \code{FALSE} is returned.
#' - \code{are_bare_raw} is a wrapper around \code{\link[purrr]{map_lgl}(vec, \(i) is_bare_raw(i, ...))}. A boolean vector of the same length as the input is guaranteed.
#' - Calls to \code{is_not_bare_raw}/\code{are_not_bare_raw} negate the output of \code{is_bare_raw}/\code{are_bare_raw}.
#' @md
#'
#' @param x Object to be tested.
#' @param n Expected length of a vector.
#' @name is_bare_raw
#' @importFrom rlang is_bare_raw
#' @importFrom purrr map_lgl
#' @export
is_bare_raw <- ensure_atomic_boolean('is_bare_raw', 'rlang')
#' 
#' @rdname is_bare_raw
#' @export
is_not_bare_raw <- function(x, n) !is_bare_raw(x, n)
#' 
#' @rdname is_bare_raw
#' @export
are_bare_raw <- function(x, n) {
      purrr::map_lgl(x, \(x) is_bare_raw(x = x, n = n))
    }
#' 
#' @rdname is_bare_raw
#' @export
are_not_bare_raw <- function(x, n) !are_bare_raw(x = x, n = n)


#' @aliases are_scalar_bytes, is_not_scalar_bytes, are_not_scalar_bytes

#' @title Scalar type predicates
#'
#' @description This is a re-export of \code{\link[rlang:scalar-type-predicates]{rlang::is_scalar_bytes()}}, modified to have standardised naming and standardised vector handling.
#' Documentation is atuomatically generated from the original package documentation. See the \code{\link[rlang:scalar-type-predicates]{original}} for full details.
#' 
#' These predicates check for a given type and whether the vector is
#' "scalar", that is, of length 1.
#' 
#' In addition to the length check, \code{is_string()} and \code{is_bool()}
#' return \code{FALSE} if their input is missing. This is useful for
#' type-checking arguments, when your function expects a single string
#' or a single \code{TRUE} or \code{FALSE}.
#' 
#' 
#' @returns 
#' - Calls to \code{is_scalar_bytes} are guaranteed to return a scalar boolean (ie. a single \code{TRUE} or \code{FALSE} value). If an argument of length > 1 is given, \code{FALSE} is returned.
#' - \code{are_scalar_bytes} is a wrapper around \code{\link[purrr]{map_lgl}(vec, \(i) is_scalar_bytes(i, ...))}. A boolean vector of the same length as the input is guaranteed.
#' - Calls to \code{is_not_scalar_bytes}/\code{are_not_scalar_bytes} negate the output of \code{is_scalar_bytes}/\code{are_scalar_bytes}.
#' @md
#'
#' @param x object to be tested.
#' @name is_scalar_bytes
#' @importFrom rlang is_scalar_bytes
#' @importFrom purrr map_lgl
#' @export
is_scalar_bytes <- ensure_atomic_boolean('is_scalar_bytes', 'rlang')
#' 
#' @rdname is_scalar_bytes
#' @export
is_not_scalar_bytes <- function(x) !is_scalar_bytes(x)
#' 
#' @rdname is_scalar_bytes
#' @export
are_scalar_bytes <- function(x) {
      purrr::map_lgl(x, \(x) is_scalar_bytes(x = x))
    }
#' 
#' @rdname is_scalar_bytes
#' @export
are_not_scalar_bytes <- function(x) !are_scalar_bytes(x = x)


#' @aliases are_bare_list, is_not_bare_list, are_not_bare_list

#' @title Bare type predicates
#'
#' @description This is a re-export of \code{\link[rlang:bare-type-predicates]{rlang::is_bare_list()}}, modified to have standardised naming and standardised vector handling.
#' Documentation is atuomatically generated from the original package documentation. See the \code{\link[rlang:bare-type-predicates]{original}} for full details.
#' 
#' These predicates check for a given type but only return \code{TRUE} for
#' bare R objects. Bare objects have no class attributes. For example,
#' a data frame is a list, but not a bare list.
#' 
#' 
#' @returns 
#' - Calls to \code{is_bare_list} are guaranteed to return a scalar boolean (ie. a single \code{TRUE} or \code{FALSE} value). If an argument of length > 1 is given, \code{FALSE} is returned.
#' - \code{are_bare_list} is a wrapper around \code{\link[purrr]{map_lgl}(vec, \(i) is_bare_list(i, ...))}. A boolean vector of the same length as the input is guaranteed.
#' - Calls to \code{is_not_bare_list}/\code{are_not_bare_list} negate the output of \code{is_bare_list}/\code{are_bare_list}.
#' @md
#'
#' @param x Object to be tested.
#' @param n Expected length of a vector.
#' @name is_bare_list
#' @importFrom rlang is_bare_list
#' @importFrom purrr map_lgl
#' @export
is_bare_list <- ensure_atomic_boolean('is_bare_list', 'rlang')
#' 
#' @rdname is_bare_list
#' @export
is_not_bare_list <- function(x, n) !is_bare_list(x, n)
#' 
#' @rdname is_bare_list
#' @export
are_bare_list <- function(x, n) {
      purrr::map_lgl(x, \(x) is_bare_list(x = x, n = n))
    }
#' 
#' @rdname is_bare_list
#' @export
are_not_bare_list <- function(x, n) !are_bare_list(x = x, n = n)


#' @aliases are_bare_environment, is_not_bare_environment, are_not_bare_environment

#' @title Is object an environment?
#'
#' @description This is a re-export of \code{\link[rlang:is_environment]{rlang::is_bare_environment()}}, modified to have standardised naming and standardised vector handling.
#' Documentation is atuomatically generated from the original package documentation. See the \code{\link[rlang:is_environment]{original}} for full details.
#' 
#' \code{is_bare_environment()} tests whether \code{x} is an environment without a s3 or
#' s4 class.
#' 
#' 
#' @returns 
#' - Calls to \code{is_bare_environment} are guaranteed to return a scalar boolean (ie. a single \code{TRUE} or \code{FALSE} value). If an argument of length > 1 is given, \code{FALSE} is returned.
#' - \code{are_bare_environment} is a wrapper around \code{\link[purrr]{map_lgl}(vec, \(i) is_bare_environment(i, ...))}. A boolean vector of the same length as the input is guaranteed.
#' - Calls to \code{is_not_bare_environment}/\code{are_not_bare_environment} negate the output of \code{is_bare_environment}/\code{are_bare_environment}.
#' @md
#'
#' @param x object to test
#' @name is_bare_environment
#' @importFrom rlang is_bare_environment
#' @importFrom purrr map_lgl
#' @export
is_bare_environment <- ensure_atomic_boolean('is_bare_environment', 'rlang')
#' 
#' @rdname is_bare_environment
#' @export
is_not_bare_environment <- function(x) !is_bare_environment(x)
#' 
#' @rdname is_bare_environment
#' @export
are_bare_environment <- function(x) {
      purrr::map_lgl(x, \(x) is_bare_environment(x = x))
    }
#' 
#' @rdname is_bare_environment
#' @export
are_not_bare_environment <- function(x) !are_bare_environment(x = x)


#' @aliases are_weakref, is_not_weakref, are_not_weakref

#' @title Is object a weak reference?
#'
#' @description This is a re-export of \code{\link[rlang:is_weakref]{rlang::is_weakref()}}, modified to have standardised naming and standardised vector handling.
#' Documentation is atuomatically generated from the original package documentation. See the \code{\link[rlang:is_weakref]{original}} for full details.
#' 
#' Is object a weak reference?.
#' 
#' 
#' @returns 
#' - Calls to \code{is_weakref} are guaranteed to return a scalar boolean (ie. a single \code{TRUE} or \code{FALSE} value). If an argument of length > 1 is given, \code{FALSE} is returned.
#' - \code{are_weakref} is a wrapper around \code{\link[purrr]{map_lgl}(vec, \(i) is_weakref(i, ...))}. A boolean vector of the same length as the input is guaranteed.
#' - Calls to \code{is_not_weakref}/\code{are_not_weakref} negate the output of \code{is_weakref}/\code{are_weakref}.
#' @md
#'
#' @param x An object to test.
#' @name is_weakref
#' @importFrom rlang is_weakref
#' @importFrom purrr map_lgl
#' @export
is_weakref <- ensure_atomic_boolean('is_weakref', 'rlang')
#' 
#' @rdname is_weakref
#' @export
is_not_weakref <- function(x) !is_weakref(x)
#' 
#' @rdname is_weakref
#' @export
are_weakref <- function(x) {
      purrr::map_lgl(x, \(x) is_weakref(x = x))
    }
#' 
#' @rdname is_weakref
#' @export
are_not_weakref <- function(x) !are_weakref(x = x)


#' @aliases are_file_empty, is_not_file_empty, are_not_file_empty

#' @title Functions to test for file types
#'
#' @description This is a re-export of \code{\link[fs:is_file]{fs::is_file_empty()}}, modified to have standardised naming and standardised vector handling.
#' Documentation is atuomatically generated from the original package documentation. See the \code{\link[fs:is_file]{original}} for full details.
#' 
#' Functions to test for file types.
#' 
#' 
#' @returns 
#' - Calls to \code{is_file_empty} are guaranteed to return a scalar boolean (ie. a single \code{TRUE} or \code{FALSE} value). If an argument of length > 1 is given, \code{FALSE} is returned.
#' - \code{are_file_empty} is a wrapper around \code{\link[purrr]{map_lgl}(vec, \(i) is_file_empty(i, ...))}. A boolean vector of the same length as the input is guaranteed.
#' - Calls to \code{is_not_file_empty}/\code{are_not_file_empty} negate the output of \code{is_file_empty}/\code{are_file_empty}.
#' @md
#'
#' @param path A character vector of one or more paths.
#' @param follow If \code{TRUE}, symbolic links will be followed (recursively) and
#' the results will be that of the final file rather than the link.
#' @name is_file_empty
#' @importFrom fs is_file_empty
#' @importFrom purrr map_lgl
#' @export
is_file_empty <- ensure_atomic_boolean('is_file_empty', 'fs')
#' 
#' @rdname is_file_empty
#' @export
is_not_file_empty <- function(path, follow) !is_file_empty(path, follow)
#' 
#' @rdname is_file_empty
#' @export
are_file_empty <- function(path, follow) {
      purrr::map_lgl(path, \(path) is_file_empty(path = path, follow = follow))
    }
#' 
#' @rdname is_file_empty
#' @export
are_not_file_empty <- function(path, follow) !are_file_empty(path = path, follow = follow)


#' @aliases are_link, is_not_link, are_not_link

#' @title Functions to test for file types
#'
#' @description This is a re-export of \code{\link[fs:is_file]{fs::is_link()}}, modified to have standardised naming and standardised vector handling.
#' Documentation is atuomatically generated from the original package documentation. See the \code{\link[fs:is_file]{original}} for full details.
#' 
#' Functions to test for file types.
#' 
#' 
#' @returns 
#' - Calls to \code{is_link} are guaranteed to return a scalar boolean (ie. a single \code{TRUE} or \code{FALSE} value). If an argument of length > 1 is given, \code{FALSE} is returned.
#' - \code{are_link} is a wrapper around \code{\link[purrr]{map_lgl}(vec, \(i) is_link(i, ...))}. A boolean vector of the same length as the input is guaranteed.
#' - Calls to \code{is_not_link}/\code{are_not_link} negate the output of \code{is_link}/\code{are_link}.
#' @md
#'
#' @param path A character vector of one or more paths.
#' @name is_link
#' @importFrom fs is_link
#' @importFrom purrr map_lgl
#' @export
is_link <- ensure_atomic_boolean('is_link', 'fs')
#' 
#' @rdname is_link
#' @export
is_not_link <- function(path) !is_link(path)
#' 
#' @rdname is_link
#' @export
are_link <- function(path) {
      purrr::map_lgl(path, \(path) is_link(path = path))
    }
#' 
#' @rdname is_link
#' @export
are_not_link <- function(path) !are_link(path = path)


#' @aliases are_dir, is_not_dir, are_not_dir

#' @title Functions to test for file types
#'
#' @description This is a re-export of \code{\link[fs:is_file]{fs::is_dir()}}, modified to have standardised naming and standardised vector handling.
#' Documentation is atuomatically generated from the original package documentation. See the \code{\link[fs:is_file]{original}} for full details.
#' 
#' Functions to test for file types.
#' 
#' 
#' @returns 
#' - Calls to \code{is_dir} are guaranteed to return a scalar boolean (ie. a single \code{TRUE} or \code{FALSE} value). If an argument of length > 1 is given, \code{FALSE} is returned.
#' - \code{are_dir} is a wrapper around \code{\link[purrr]{map_lgl}(vec, \(i) is_dir(i, ...))}. A boolean vector of the same length as the input is guaranteed.
#' - Calls to \code{is_not_dir}/\code{are_not_dir} negate the output of \code{is_dir}/\code{are_dir}.
#' @md
#'
#' @param path A character vector of one or more paths.
#' @param follow If \code{TRUE}, symbolic links will be followed (recursively) and
#' the results will be that of the final file rather than the link.
#' @name is_dir
#' @importFrom fs is_dir
#' @importFrom purrr map_lgl
#' @export
is_dir <- ensure_atomic_boolean('is_dir', 'fs')
#' 
#' @rdname is_dir
#' @export
is_not_dir <- function(path, follow) !is_dir(path, follow)
#' 
#' @rdname is_dir
#' @export
are_dir <- function(path, follow) {
      purrr::map_lgl(path, \(path) is_dir(path = path, follow = follow))
    }
#' 
#' @rdname is_dir
#' @export
are_not_dir <- function(path, follow) !are_dir(path = path, follow = follow)


#' @aliases are_file, is_not_file, are_not_file

#' @title Functions to test for file types
#'
#' @description This is a re-export of \code{\link[fs:is_file]{fs::is_file()}}, modified to have standardised naming and standardised vector handling.
#' Documentation is atuomatically generated from the original package documentation. See the \code{\link[fs:is_file]{original}} for full details.
#' 
#' Functions to test for file types.
#' 
#' 
#' @returns 
#' - Calls to \code{is_file} are guaranteed to return a scalar boolean (ie. a single \code{TRUE} or \code{FALSE} value). If an argument of length > 1 is given, \code{FALSE} is returned.
#' - \code{are_file} is a wrapper around \code{\link[purrr]{map_lgl}(vec, \(i) is_file(i, ...))}. A boolean vector of the same length as the input is guaranteed.
#' - Calls to \code{is_not_file}/\code{are_not_file} negate the output of \code{is_file}/\code{are_file}.
#' @md
#'
#' @param path A character vector of one or more paths.
#' @param follow If \code{TRUE}, symbolic links will be followed (recursively) and
#' the results will be that of the final file rather than the link.
#' @name is_file
#' @importFrom fs is_file
#' @importFrom purrr map_lgl
#' @export
is_file <- ensure_atomic_boolean('is_file', 'fs')
#' 
#' @rdname is_file
#' @export
is_not_file <- function(path, follow) !is_file(path, follow)
#' 
#' @rdname is_file
#' @export
are_file <- function(path, follow) {
      purrr::map_lgl(path, \(path) is_file(path = path, follow = follow))
    }
#' 
#' @rdname is_file
#' @export
are_not_file <- function(path, follow) !are_file(path = path, follow = follow)


#' @aliases are_absolute_path, is_not_absolute_path, are_not_absolute_path

#' @title Test if a path is an absolute path
#'
#' @description This is a re-export of \code{\link[fs:is_absolute_path]{fs::is_absolute_path()}}, modified to have standardised naming and standardised vector handling.
#' Documentation is atuomatically generated from the original package documentation. See the \code{\link[fs:is_absolute_path]{original}} for full details.
#' 
#' Test if a path is an absolute path.
#' 
#' 
#' @returns 
#' - Calls to \code{is_absolute_path} are guaranteed to return a scalar boolean (ie. a single \code{TRUE} or \code{FALSE} value). If an argument of length > 1 is given, \code{FALSE} is returned.
#' - \code{are_absolute_path} is a wrapper around \code{\link[purrr]{map_lgl}(vec, \(i) is_absolute_path(i, ...))}. A boolean vector of the same length as the input is guaranteed.
#' - Calls to \code{is_not_absolute_path}/\code{are_not_absolute_path} negate the output of \code{is_absolute_path}/\code{are_absolute_path}.
#' @md
#'
#' @param path A character vector of one or more paths.
#' @name is_absolute_path
#' @importFrom fs is_absolute_path
#' @importFrom purrr map_lgl
#' @export
is_absolute_path <- ensure_atomic_boolean('is_absolute_path', 'fs')
#' 
#' @rdname is_absolute_path
#' @export
is_not_absolute_path <- function(path) !is_absolute_path(path)
#' 
#' @rdname is_absolute_path
#' @export
are_absolute_path <- function(path) {
      purrr::map_lgl(path, \(path) is_absolute_path(path = path))
    }
#' 
#' @rdname is_absolute_path
#' @export
are_not_absolute_path <- function(path) !are_absolute_path(path = path)


#' @aliases are_existing_file, is_not_existing_file, are_not_existing_file

#' @title Query for existence and access permissions
#'
#' @description This is a re-export of \code{\link[fs:file_access]{fs::file_exists()}}, modified to have standardised naming and standardised vector handling.
#' Documentation is atuomatically generated from the original package documentation. See the \code{\link[fs:file_access]{original}} for full details.
#' 
#' \code{file_exists(path)} is a shortcut for \code{file_access(x, "exists")};
#' \code{dir_exists(path)} and \code{link_exists(path)} are similar but also check that
#' the path is a directory or link, respectively. (\code{file_exists(path)} returns
#' \code{TRUE} if \code{path} exists and it is a directory.).
#' 
#' 
#' @returns 
#' - Calls to \code{is_existing_file} are guaranteed to return a scalar boolean (ie. a single \code{TRUE} or \code{FALSE} value). If an argument of length > 1 is given, \code{FALSE} is returned.
#' - \code{are_existing_file} is a wrapper around \code{\link[purrr]{map_lgl}(vec, \(i) is_existing_file(i, ...))}. A boolean vector of the same length as the input is guaranteed.
#' - Calls to \code{is_not_existing_file}/\code{are_not_existing_file} negate the output of \code{is_existing_file}/\code{are_existing_file}.
#' @md
#'
#' @param path A character vector of one or more paths.
#' @name is_existing_file
#' @importFrom fs file_exists
#' @importFrom purrr map_lgl
#' @export
is_existing_file <- ensure_atomic_boolean('file_exists', 'fs')
#' 
#' @rdname is_existing_file
#' @export
is_not_existing_file <- function(path) !is_existing_file(path)
#' 
#' @rdname is_existing_file
#' @export
are_existing_file <- function(path) {
      purrr::map_lgl(path, \(path) is_existing_file(path = path))
    }
#' 
#' @rdname is_existing_file
#' @export
are_not_existing_file <- function(path) !are_existing_file(path = path)


#' @aliases are_existing_dir, is_not_existing_dir, are_not_existing_dir

#' @title Query for existence and access permissions
#'
#' @description This is a re-export of \code{\link[fs:file_access]{fs::dir_exists()}}, modified to have standardised naming and standardised vector handling.
#' Documentation is atuomatically generated from the original package documentation. See the \code{\link[fs:file_access]{original}} for full details.
#' 
#' \code{file_exists(path)} is a shortcut for \code{file_access(x, "exists")};
#' \code{dir_exists(path)} and \code{link_exists(path)} are similar but also check that
#' the path is a directory or link, respectively. (\code{file_exists(path)} returns
#' \code{TRUE} if \code{path} exists and it is a directory.).
#' 
#' 
#' @returns 
#' - Calls to \code{is_existing_dir} are guaranteed to return a scalar boolean (ie. a single \code{TRUE} or \code{FALSE} value). If an argument of length > 1 is given, \code{FALSE} is returned.
#' - \code{are_existing_dir} is a wrapper around \code{\link[purrr]{map_lgl}(vec, \(i) is_existing_dir(i, ...))}. A boolean vector of the same length as the input is guaranteed.
#' - Calls to \code{is_not_existing_dir}/\code{are_not_existing_dir} negate the output of \code{is_existing_dir}/\code{are_existing_dir}.
#' @md
#'
#' @param path A character vector of one or more paths.
#' @name is_existing_dir
#' @importFrom fs dir_exists
#' @importFrom purrr map_lgl
#' @export
is_existing_dir <- ensure_atomic_boolean('dir_exists', 'fs')
#' 
#' @rdname is_existing_dir
#' @export
is_not_existing_dir <- function(path) !is_existing_dir(path)
#' 
#' @rdname is_existing_dir
#' @export
are_existing_dir <- function(path) {
      purrr::map_lgl(path, \(path) is_existing_dir(path = path))
    }
#' 
#' @rdname is_existing_dir
#' @export
are_not_existing_dir <- function(path) !are_existing_dir(path = path)


#' @aliases are_existing_link, is_not_existing_link, are_not_existing_link

#' @title Query for existence and access permissions
#'
#' @description This is a re-export of \code{\link[fs:file_access]{fs::link_exists()}}, modified to have standardised naming and standardised vector handling.
#' Documentation is atuomatically generated from the original package documentation. See the \code{\link[fs:file_access]{original}} for full details.
#' 
#' \code{file_exists(path)} is a shortcut for \code{file_access(x, "exists")};
#' \code{dir_exists(path)} and \code{link_exists(path)} are similar but also check that
#' the path is a directory or link, respectively. (\code{file_exists(path)} returns
#' \code{TRUE} if \code{path} exists and it is a directory.).
#' 
#' 
#' @returns 
#' - Calls to \code{is_existing_link} are guaranteed to return a scalar boolean (ie. a single \code{TRUE} or \code{FALSE} value). If an argument of length > 1 is given, \code{FALSE} is returned.
#' - \code{are_existing_link} is a wrapper around \code{\link[purrr]{map_lgl}(vec, \(i) is_existing_link(i, ...))}. A boolean vector of the same length as the input is guaranteed.
#' - Calls to \code{is_not_existing_link}/\code{are_not_existing_link} negate the output of \code{is_existing_link}/\code{are_existing_link}.
#' @md
#'
#' @param path A character vector of one or more paths.
#' @name is_existing_link
#' @importFrom fs link_exists
#' @importFrom purrr map_lgl
#' @export
is_existing_link <- ensure_atomic_boolean('link_exists', 'fs')
#' 
#' @rdname is_existing_link
#' @export
is_not_existing_link <- function(path) !is_existing_link(path)
#' 
#' @rdname is_existing_link
#' @export
are_existing_link <- function(path) {
      purrr::map_lgl(path, \(path) is_existing_link(path = path))
    }
#' 
#' @rdname is_existing_link
#' @export
are_not_existing_link <- function(path) !are_existing_link(path = path)


#' @aliases are_POSIXlt, is_not_POSIXlt, are_not_POSIXlt

#' @title Various POSIX utilities
#'
#' @description This is a re-export of \code{\link[lubridate:posix_utils]{lubridate::is.POSIXlt()}}, modified to have standardised naming and standardised vector handling.
#' Documentation is atuomatically generated from the original package documentation. See the \code{\link[lubridate:posix_utils]{original}} for full details.
#' 
#' \code{\link{POSIXct()}} mirrors primitive contructors in base R (\code{\link{double()}},
#' \code{\link{character()}} etc.).
#' 
#' 
#' @returns 
#' - Calls to \code{is_POSIXlt} are guaranteed to return a scalar boolean (ie. a single \code{TRUE} or \code{FALSE} value). If an argument of length > 1 is given, \code{FALSE} is returned.
#' - \code{are_POSIXlt} is a wrapper around \code{\link[purrr]{map_lgl}(vec, \(i) is_POSIXlt(i, ...))}. A boolean vector of the same length as the input is guaranteed.
#' - Calls to \code{is_not_POSIXlt}/\code{are_not_POSIXlt} negate the output of \code{is_POSIXlt}/\code{are_POSIXlt}.
#' @md
#'
#' @param x an R object
#' @name is_POSIXlt
#' @importFrom lubridate is.POSIXlt
#' @importFrom purrr map_lgl
#' @export
is_POSIXlt <- ensure_atomic_boolean('is.POSIXlt', 'lubridate')
#' 
#' @rdname is_POSIXlt
#' @export
is_not_POSIXlt <- function(x) !is_POSIXlt(x)
#' 
#' @rdname is_POSIXlt
#' @export
are_POSIXlt <- function(x) {
      purrr::map_lgl(x, \(x) is_POSIXlt(x = x))
    }
#' 
#' @rdname is_POSIXlt
#' @export
are_not_POSIXlt <- function(x) !are_POSIXlt(x = x)


#' @aliases are_POSIXt, is_not_POSIXt, are_not_POSIXt

#' @title Various POSIX utilities
#'
#' @description This is a re-export of \code{\link[lubridate:posix_utils]{lubridate::is.POSIXt()}}, modified to have standardised naming and standardised vector handling.
#' Documentation is atuomatically generated from the original package documentation. See the \code{\link[lubridate:posix_utils]{original}} for full details.
#' 
#' \code{\link{POSIXct()}} mirrors primitive contructors in base R (\code{\link{double()}},
#' \code{\link{character()}} etc.).
#' 
#' 
#' @returns 
#' - Calls to \code{is_POSIXt} are guaranteed to return a scalar boolean (ie. a single \code{TRUE} or \code{FALSE} value). If an argument of length > 1 is given, \code{FALSE} is returned.
#' - \code{are_POSIXt} is a wrapper around \code{\link[purrr]{map_lgl}(vec, \(i) is_POSIXt(i, ...))}. A boolean vector of the same length as the input is guaranteed.
#' - Calls to \code{is_not_POSIXt}/\code{are_not_POSIXt} negate the output of \code{is_POSIXt}/\code{are_POSIXt}.
#' @md
#'
#' @param x an R object
#' @name is_POSIXt
#' @importFrom lubridate is.POSIXt
#' @importFrom purrr map_lgl
#' @export
is_POSIXt <- ensure_atomic_boolean('is.POSIXt', 'lubridate')
#' 
#' @rdname is_POSIXt
#' @export
is_not_POSIXt <- function(x) !is_POSIXt(x)
#' 
#' @rdname is_POSIXt
#' @export
are_POSIXt <- function(x) {
      purrr::map_lgl(x, \(x) is_POSIXt(x = x))
    }
#' 
#' @rdname is_POSIXt
#' @export
are_not_POSIXt <- function(x) !are_POSIXt(x = x)


#' @aliases are_timepoint, is_not_timepoint, are_not_timepoint

#' @title Is x a date-time object?
#'
#' @description This is a re-export of \code{\link[lubridate:is.instant]{lubridate::is.timepoint()}}, modified to have standardised naming and standardised vector handling.
#' Documentation is atuomatically generated from the original package documentation. See the \code{\link[lubridate:is.instant]{original}} for full details.
#' 
#' An instant is a specific moment in time. Most common date-time
#' objects (e.g, POSIXct, POSIXlt, and Date objects) are instants.
#' 
#' 
#' @returns 
#' - Calls to \code{is_timepoint} are guaranteed to return a scalar boolean (ie. a single \code{TRUE} or \code{FALSE} value). If an argument of length > 1 is given, \code{FALSE} is returned.
#' - \code{are_timepoint} is a wrapper around \code{\link[purrr]{map_lgl}(vec, \(i) is_timepoint(i, ...))}. A boolean vector of the same length as the input is guaranteed.
#' - Calls to \code{is_not_timepoint}/\code{are_not_timepoint} negate the output of \code{is_timepoint}/\code{are_timepoint}.
#' @md
#'
#' @param x an R object
#' @name is_timepoint
#' @importFrom lubridate is.timepoint
#' @importFrom purrr map_lgl
#' @export
is_timepoint <- ensure_atomic_boolean('is.timepoint', 'lubridate')
#' 
#' @rdname is_timepoint
#' @export
is_not_timepoint <- function(x) !is_timepoint(x)
#' 
#' @rdname is_timepoint
#' @export
are_timepoint <- function(x) {
      purrr::map_lgl(x, \(x) is_timepoint(x = x))
    }
#' 
#' @rdname is_timepoint
#' @export
are_not_timepoint <- function(x) !are_timepoint(x = x)


#' @aliases are_timespan, is_not_timespan, are_not_timespan

#' @title Is x a length of time?
#'
#' @description This is a re-export of \code{\link[lubridate:is.timespan]{lubridate::is.timespan()}}, modified to have standardised naming and standardised vector handling.
#' Documentation is atuomatically generated from the original package documentation. See the \code{\link[lubridate:is.timespan]{original}} for full details.
#' 
#' Is x a length of time?.
#' 
#' 
#' @returns 
#' - Calls to \code{is_timespan} are guaranteed to return a scalar boolean (ie. a single \code{TRUE} or \code{FALSE} value). If an argument of length > 1 is given, \code{FALSE} is returned.
#' - \code{are_timespan} is a wrapper around \code{\link[purrr]{map_lgl}(vec, \(i) is_timespan(i, ...))}. A boolean vector of the same length as the input is guaranteed.
#' - Calls to \code{is_not_timespan}/\code{are_not_timespan} negate the output of \code{is_timespan}/\code{are_timespan}.
#' @md
#'
#' @param x an R object
#' @name is_timespan
#' @importFrom lubridate is.timespan
#' @importFrom purrr map_lgl
#' @export
is_timespan <- ensure_atomic_boolean('is.timespan', 'lubridate')
#' 
#' @rdname is_timespan
#' @export
is_not_timespan <- function(x) !is_timespan(x)
#' 
#' @rdname is_timespan
#' @export
are_timespan <- function(x) {
      purrr::map_lgl(x, \(x) is_timespan(x = x))
    }
#' 
#' @rdname is_timespan
#' @export
are_not_timespan <- function(x) !are_timespan(x = x)


#' @aliases are_difftime, is_not_difftime, are_not_difftime

#' @title Is x a difftime object?
#'
#' @description This is a re-export of \code{\link[lubridate:is.difftime]{lubridate::is.difftime()}}, modified to have standardised naming and standardised vector handling.
#' Documentation is atuomatically generated from the original package documentation. See the \code{\link[lubridate:is.difftime]{original}} for full details.
#' 
#' Is x a difftime object?.
#' 
#' 
#' @returns 
#' - Calls to \code{is_difftime} are guaranteed to return a scalar boolean (ie. a single \code{TRUE} or \code{FALSE} value). If an argument of length > 1 is given, \code{FALSE} is returned.
#' - \code{are_difftime} is a wrapper around \code{\link[purrr]{map_lgl}(vec, \(i) is_difftime(i, ...))}. A boolean vector of the same length as the input is guaranteed.
#' - Calls to \code{is_not_difftime}/\code{are_not_difftime} negate the output of \code{is_difftime}/\code{are_difftime}.
#' @md
#'
#' @param x an R object
#' @name is_difftime
#' @importFrom lubridate is.difftime
#' @importFrom purrr map_lgl
#' @export
is_difftime <- ensure_atomic_boolean('is.difftime', 'lubridate')
#' 
#' @rdname is_difftime
#' @export
is_not_difftime <- function(x) !is_difftime(x)
#' 
#' @rdname is_difftime
#' @export
are_difftime <- function(x) {
      purrr::map_lgl(x, \(x) is_difftime(x = x))
    }
#' 
#' @rdname is_difftime
#' @export
are_not_difftime <- function(x) !are_difftime(x = x)


#' @aliases are_duration, is_not_duration, are_not_duration

#' @title Create a duration object.
#'
#' @description This is a re-export of \code{\link[lubridate:duration]{lubridate::is.duration()}}, modified to have standardised naming and standardised vector handling.
#' Documentation is atuomatically generated from the original package documentation. See the \code{\link[lubridate:duration]{original}} for full details.
#' 
#' \code{duration()} creates a duration object with the specified values. Entries
#' for different units are cumulative. durations display as the number of
#' seconds in a time span. When this number is large, durations also display an
#' estimate in larger units, however, the underlying object is always recorded
#' as a fixed number of seconds. For display and creation purposes, units are
#' converted to seconds using their most common lengths in seconds. Minutes = 60
#' seconds, hours = 3600 seconds, days = 86400 seconds, weeks = 604800. Units
#' larger than weeks are not used due to their variability.
#' 
#' 
#' @returns 
#' - Calls to \code{is_duration} are guaranteed to return a scalar boolean (ie. a single \code{TRUE} or \code{FALSE} value). If an argument of length > 1 is given, \code{FALSE} is returned.
#' - \code{are_duration} is a wrapper around \code{\link[purrr]{map_lgl}(vec, \(i) is_duration(i, ...))}. A boolean vector of the same length as the input is guaranteed.
#' - Calls to \code{is_not_duration}/\code{are_not_duration} negate the output of \code{is_duration}/\code{are_duration}.
#' @md
#'
#' @param x numeric value of the number of units to be contained in the
#' duration.
#' @name is_duration
#' @importFrom lubridate is.duration
#' @importFrom purrr map_lgl
#' @export
is_duration <- ensure_atomic_boolean('is.duration', 'lubridate')
#' 
#' @rdname is_duration
#' @export
is_not_duration <- function(x) !is_duration(x)
#' 
#' @rdname is_duration
#' @export
are_duration <- function(x) {
      purrr::map_lgl(x, \(x) is_duration(x = x))
    }
#' 
#' @rdname is_duration
#' @export
are_not_duration <- function(x) !are_duration(x = x)


#' @aliases are_period, is_not_period, are_not_period

#' @title Create or parse period objects
#'
#' @description This is a re-export of \code{\link[lubridate:period]{lubridate::is.period()}}, modified to have standardised naming and standardised vector handling.
#' Documentation is atuomatically generated from the original package documentation. See the \code{\link[lubridate:period]{original}} for full details.
#' 
#' \code{period()} creates or parses a period object with the specified values.
#' 
#' 
#' @returns 
#' - Calls to \code{is_period} are guaranteed to return a scalar boolean (ie. a single \code{TRUE} or \code{FALSE} value). If an argument of length > 1 is given, \code{FALSE} is returned.
#' - \code{are_period} is a wrapper around \code{\link[purrr]{map_lgl}(vec, \(i) is_period(i, ...))}. A boolean vector of the same length as the input is guaranteed.
#' - Calls to \code{is_not_period}/\code{are_not_period} negate the output of \code{is_period}/\code{are_period}.
#' @md
#'
#' @param x Any R object for \code{is.periods} and a numeric value of the number of
#' units for elementary constructors. With the exception of seconds(), x must
#' be an integer.
#' @name is_period
#' @importFrom lubridate is.period
#' @importFrom purrr map_lgl
#' @export
is_period <- ensure_atomic_boolean('is.period', 'lubridate')
#' 
#' @rdname is_period
#' @export
is_not_period <- function(x) !is_period(x)
#' 
#' @rdname is_period
#' @export
are_period <- function(x) {
      purrr::map_lgl(x, \(x) is_period(x = x))
    }
#' 
#' @rdname is_period
#' @export
are_not_period <- function(x) !are_period(x = x)


#' @aliases are_instant, is_not_instant, are_not_instant

#' @title Is x a date-time object?
#'
#' @description This is a re-export of \code{\link[lubridate:is.instant]{lubridate::is.instant()}}, modified to have standardised naming and standardised vector handling.
#' Documentation is atuomatically generated from the original package documentation. See the \code{\link[lubridate:is.instant]{original}} for full details.
#' 
#' An instant is a specific moment in time. Most common date-time
#' objects (e.g, POSIXct, POSIXlt, and Date objects) are instants.
#' 
#' 
#' @returns 
#' - Calls to \code{is_instant} are guaranteed to return a scalar boolean (ie. a single \code{TRUE} or \code{FALSE} value). If an argument of length > 1 is given, \code{FALSE} is returned.
#' - \code{are_instant} is a wrapper around \code{\link[purrr]{map_lgl}(vec, \(i) is_instant(i, ...))}. A boolean vector of the same length as the input is guaranteed.
#' - Calls to \code{is_not_instant}/\code{are_not_instant} negate the output of \code{is_instant}/\code{are_instant}.
#' @md
#'
#' @param x an R object
#' @name is_instant
#' @importFrom lubridate is.instant
#' @importFrom purrr map_lgl
#' @export
is_instant <- ensure_atomic_boolean('is.instant', 'lubridate')
#' 
#' @rdname is_instant
#' @export
is_not_instant <- function(x) !is_instant(x)
#' 
#' @rdname is_instant
#' @export
are_instant <- function(x) {
      purrr::map_lgl(x, \(x) is_instant(x = x))
    }
#' 
#' @rdname is_instant
#' @export
are_not_instant <- function(x) !are_instant(x = x)


#' @aliases are_interval, is_not_interval, are_not_interval

#' @title Utilities for creation and manipulation of \code{Interval} objects
#'
#' @description This is a re-export of \code{\link[lubridate:interval]{lubridate::is.interval()}}, modified to have standardised naming and standardised vector handling.
#' Documentation is atuomatically generated from the original package documentation. See the \code{\link[lubridate:interval]{original}} for full details.
#' 
#' \code{interval()} creates an \linkS4class{Interval} object with the specified start and
#' end dates. If the start date occurs before the end date, the interval will be
#' positive. Otherwise, it will be negative. Character vectors in ISO 8601
#' format are supported from v1.7.2.
#' 
#' \code{int_start()}/\code{int_end()} and \verb{int_start<-()}/\verb{int_end<-()} are
#' "accessors" and "setters" respectively of the start/end date of an
#' interval.
#' 
#' \code{int_flip()} reverses the order of the start date and end date in an
#' interval. The new interval takes place during the same timespan as the
#' original interval, but has the opposite direction.
#' 
#' \code{int_shift()} shifts the start and end dates of an interval up or down the
#' timeline by a specified amount. Note that this may change the exact length of
#' the interval if the interval is shifted by a Period object. Intervals shifted
#' by a Duration or difftime object will retain their exact length in seconds.
#' 
#' \code{int_overlaps()} tests if two intervals overlap.
#' 
#' \code{int_standardize()} ensures all intervals in an interval object are
#' positive. If an interval is not positive, flip it so that it retains its
#' endpoints but becomes positive.
#' 
#' \code{int_aligns()} tests if two intervals share an endpoint. The direction of
#' each interval is ignored. int_align tests whether the earliest or latest
#' moments of each interval occur at the same time.
#' 
#' \code{int_diff()} returns the intervals that occur between the elements of a
#' vector of date-times. \code{int_diff()} is similar to the POSIXt and Date
#' methods of \code{\link{diff()}}, but returns an \linkS4class{Interval} object instead
#' of a difftime object.
#' 
#' 
#' @returns 
#' - Calls to \code{is_interval} are guaranteed to return a scalar boolean (ie. a single \code{TRUE} or \code{FALSE} value). If an argument of length > 1 is given, \code{FALSE} is returned.
#' - \code{are_interval} is a wrapper around \code{\link[purrr]{map_lgl}(vec, \(i) is_interval(i, ...))}. A boolean vector of the same length as the input is guaranteed.
#' - Calls to \code{is_not_interval}/\code{are_not_interval} negate the output of \code{is_interval}/\code{are_interval}.
#' @md
#'
#' @param x an R object
#' @name is_interval
#' @importFrom lubridate is.interval
#' @importFrom purrr map_lgl
#' @export
is_interval <- ensure_atomic_boolean('is.interval', 'lubridate')
#' 
#' @rdname is_interval
#' @export
is_not_interval <- function(x) !is_interval(x)
#' 
#' @rdname is_interval
#' @export
are_interval <- function(x) {
      purrr::map_lgl(x, \(x) is_interval(x = x))
    }
#' 
#' @rdname is_interval
#' @export
are_not_interval <- function(x) !are_interval(x = x)


