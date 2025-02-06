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
#' @param expr An expression to be run if condition is TRUE (or FALSE for `*_if_not`).
#'   This is run before calling `cli::cli_abort`/`cli::cli_warn`/`cli::cli_inform`.
#'   This is useful for the common case of cleanup tasks before exit or returning early
#'   with a warning message.
#' @param ... Additional arguments passed by `cli::cli_abort`/`cli::cli_warn`/`cli::cli_inform` to
#'   `rlang::abort`/`rlang::warn`/`rlang::inform`
#' @param .envir The environment to use for message interpolation
#' @param call The execution environment for error messages
#' @param .frame The environment to use for error messagesi
#'
#' @return Nothing is returned; these functions are called for their side effects
#'
#' @examples
#' \dontrun{
#' x <- -5
#' abort_if(x < 0, "Value must be positive")
#' # The use of pipes with `is_*` family functions allows English-like composition
#' warn_if(x |> is_not_number(), "x must be a number")
#' # All the features of `cli` messages are available.
#' abort_if(x |> is_not_integerish(),
#'   c("{.arg x} must be integerish."
#'   "x" = case_when(is_number(x)  ~ "Was {x} instead",
#'                   is_numeric(x) ~ "Was length {length(x)} vector instead",
#'                   TRUE          ~ "Was class {.cls class(x)} instead")
#' inform_if(x > 100, "Large value detected: {x}")
#' # The `expr` argument is useful for early return or cleanup
#' warn_if_not(x >= 0, "Value should be non-negative", return(x))
#' do_cleanup <- function(){} # cleanup tasks
#' abort_if(x |> is_null(),
#'          "x can't be NULL",
#'          {do_cleanup()
#'           return(x)})
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
