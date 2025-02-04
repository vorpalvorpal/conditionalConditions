#' Conditional CLI Message Functions
#'
#' These functions provide convenient wrappers around cli message functions that
#' only execute when a condition is TRUE (for `*_if`) or FALSE (for `*_if_not`).
#' They maintain all the functionality of the original cli functions while adding
#' conditional execution.
#'
#' @param condition Logical. For `*_if` functions, the message is displayed when TRUE.
#'   For `*_if_not` functions, the message is displayed when FALSE.
#' @param message The message to display
#' @param ... Additional arguments passed to the corresponding cli function
#' @param .envir The environment to use for message interpolation
#' @param call The execution environment for error messages
#' @param .frame The environment to use for error messages
#'
#' @return Nothing is returned; these functions are called for their side effects
#'
#' @examples
#' \dontrun{
#' x <- -5
#' abort_if(x < 0, "Value must be positive")
#' warn_if_not(x >= 0, "Value should be non-negative")
#' inform_if(x > 100, "Large value detected: {x}")
#' }
#'
#' @name cli_conditional
NULL

#' @rdname cli_conditional
#' @export
abort_if <- function(condition,
                     message,
                     ...,
                     call = .envir,
                     .envir = parent.frame(),
                     .frame = .envir) {
  checkmate::assert_logical(condition, len = 1, any.missing = FALSE)
  if (condition) {
    cli::cli_abort(message,
      ...,
      call = call,
      .envir = .envir,
      .frame = .frame)
  }
}

#' @rdname cli_conditional
#' @export
abort_if_not <- function(condition,
                         message,
                         ...,
                         call = .envir,
                         .envir = parent.frame(),
                         .frame = .envir) {
  checkmate::assert_logical(condition, len = 1, any.missing = FALSE)
  if (!condition) {
    cli::cli_abort(message,
      ...,
      call = call,
      .envir = .envir,
      .frame = .frame)
  }
}

#' @rdname cli_conditional
#' @export
warn_if <- function(condition,
                    message,
                    ...,
                    .envir = parent.frame()) {
  checkmate::assert_logical(condition, len = 1, any.missing = FALSE)
  if (condition) {
    cli::cli_warn(message, ..., .envir = .envir)
  }
}

#' @rdname cli_conditional
#' @export
warn_if_not <- function(condition,
                        message,
                        ...,
                        .envir = parent.frame()) {
  checkmate::assert_logical(condition, len = 1, any.missing = FALSE)
  if (!condition) {
    cli::cli_warn(message, ..., .envir = .envir)
  }
}

#' @rdname cli_conditional
#' @export
inform_if <- function(condition,
                      message,
                      ...,
                      .envir = parent.frame()) {
  checkmate::assert_logical(condition, len = 1, any.missing = FALSE)
  if (condition) {
    cli::cli_inform(message, ..., .envir = .envir)
  }
}

#' @rdname cli_conditional
#' @export
inform_if_not <- function(condition,
                          message,
                          ...,
                          .envir = parent.frame()) {
  checkmate::assert_logical(condition, len = 1, any.missing = FALSE)
  if (!condition) {
    cli::cli_inform(message, ..., .envir = .envir)
  }
}
