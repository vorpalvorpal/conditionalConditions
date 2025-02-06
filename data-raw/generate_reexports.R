# data-raw/generate_reexports.R

# Function to find and process exported functions from a package matching patterns
get_matching_exports <- function(package, patterns) {
  ns <- asNamespace(package)
  exports <- getNamespaceExports(ns)

  matches <- unique(unlist(lapply(patterns, function(pattern) {
    grep(pattern, exports, value = TRUE)
  })))

  if (length(matches) == 0) {
    warning("No matching functions found in package: ", package)
  }

  matches
}

enlist <- function(l) {
  idx <-
    map_lgl(l,
      \(i) (rlang::is_list(i) &&
        (rlang::is_empty(i) &&
          stringr::str_starts(attributes(i)$Rd_tag, "\\\\")))) |>
    which()
  if (length(idx) == 0) return(l)
  idx <- c(idx, length(l) + 1)
  j <- purrr::map2(idx[1:(length(idx) - 1)],
    dplyr::lead(idx)[1:(length(idx) - 1)],
    \(x, y) {
      i <- l[(x + 1):(y - 1)]
      attributes(i) <- attributes(l[[x]])
      return(i)
    })
  j <- c(l[1:(idx[1] - 1)], j)
  attributes(j) <- attributes(l)
  return(j)
}

# Add extract Rd tags from list and put them in the correct spots.
add_tags <- function(x) {
  tag <- attr(x, "Rd_tag")
  if (purrr::is_null(tag)) {
    if (purrr::is_empty(x)) return(NULL)
    else if (is.list(x)) return(paste0(purrr::map_chr(enlist(x), add_tags), collapse = ""))
    else return(paste0(as.character(x), collapse = ""))
  } else if (stringr::str_starts(tag, "\\\\")) {
    if (purrr::is_empty(x)) {
      return(tag)
    } else if (tag %in% c("\\preformatted")) {
      # TODO work out how to put the line breaks in 'collapse = "\n"' doesn't work.
      out <- paste0(tag,
        "{",
        paste0(x, collapse = ""),
        "}",
        collapse = "")
      return(out)
      # multi-argument tags
    } else if (purrr::every(x, \(i) purrr::is_null(attributes(i)))) {
      out <- paste0(
        tag,
        paste0(purrr::map_chr(x, \(i) paste0("{", add_tags(i), "}")), collapse = ""),
        collapse = ""
      )
      return(out)
    } else {
      out <- paste0(
        tag,
        "{",
        paste0(purrr::map_chr(enlist(x), add_tags), collapse = ""),
        "}",
        collapse = "")
      return(out)
    }
  } else return(paste0(as.character(x), collapse = ""))
}

get_title <- function(fun, package) {
  # Get rd file
  tryCatch(
    {
      rd <- utils:::.getHelpFile(
        as.character(help(fun, package = eval(package), help_type = "text"))
      )
    },
    error = function(e) {
      warning("Could not find help file for ", fun, " in package ", package)
      return(character())
    })

  # Extract items
  title <- rd[which(tools:::RdTags(rd) == "\\title")][[1]] |>
    purrr::map_chr(add_tags) |>
    paste0(collapse = "") |>
    stringr::str_remove("^\\n") |>
    stringr::str_replace_all("\\n", "\n#' ")

  if (length(title) == 0) return(character())
  else return(title)
}

get_description <- function(fun, package) {
  # Get rd file
  tryCatch(
    {
      rd <- utils:::.getHelpFile(
        as.character(help(fun, package = eval(package), help_type = "text"))
      )
    },
    error = function(e) {
      warning("Could not find help file for ", fun, " in package ", package)
      return(character())
    })

  # Extract items
  description <-
    rd[which(tools:::RdTags(rd) == "\\description")][[1]] |>
    purrr::map_chr(add_tags) |>
    paste0(collapse = "") |>
    stringr::str_trim() |>
    # Fix up potentially missing final full stops
    stringr::str_remove("\\.$") |>
    paste0(".\n") |>
    stringr::str_replace_all("\\n", "\n#' ")

  if (length(description) == 0) return(character())
  else return(description)
}

# Function to extract parameter documentation from a package
get_param_docs <- function(fun, package) {
  # Get rd file
  tryCatch(
    {
      rd <- utils:::.getHelpFile(
        as.character(help(fun, package = eval(package), help_type = "text"))
      )
    },
    error = function(e) {
      warning("Could not find help file for ", fun, " in package ", package)
      return(character())
    })

  # Extract items
  args <- purrr::map(rd[which(tools:::RdTags(rd) == "\\arguments")][[1]],
    function(arg) {
      if ("Rd_tag" %in% names(attributes(arg))) {
        if (attributes(arg)$Rd_tag == "\\item") {
          return(arg)
        }
      }
      return(NULL)
    }) |> purrr::compact()

  if (length(args) == 0) return(character())

  # Get argument names and descriptions
  arg_names <- purrr::map_chr(args, function(arg) arg[[1]][[1]][[1]][1])
  arg_desc <- purrr::map_chr(args, function(arg) {
    purrr::map_chr(arg[[2]], add_tags) |>
      paste0(collapse = "")
  }) |>
    stringr::str_replace_all("\\n", "\n#' ") |>
    stringr::str_replace_all("^\\[", "") |>
    stringr::str_replace_all("\\]\\\\", "\\\\")

  # Check if arguments are actually used in the function
  fun_args <- names(formals(get(fun, envir = asNamespace(package))))
  valid_args <- arg_names %in% fun_args
  multi_args <- grep(",", arg_names)
  if (length(multi_args > 0)) {
    valid_multis <-
      purrr::map(multi_args, \(i){
        arg_names[[i]] |>
          stringr::str_split_1(",") |>
          stringr::str_trim() |>
          purrr::map(\(s) if (s %in% fun_args) s) |>
          unlist()
      })
    multi_args <- multi_args[!purrr::map_lgl(valid_multis, is.null)]

    valid_args[multi_args] <- TRUE
    arg_names[multi_args] <-
      valid_multis[!purrr::map_lgl(valid_multis, is.null)] |>
      purrr::map_chr(\(x) stringr::str_flatten(x, collapse = ","))
  }


  # Create roxygen param tags
  param_docs <- sprintf("#' @param %s %s",
    arg_names[valid_args],
    arg_desc[valid_args])

  if (length(param_docs) == 0) return(character())

  return(param_docs)
}

# Function to standardise function names to is_ format
standardise_name <- function(fun) {
  # Handle lubridate's is.* format
  fun <- sub("^is\\.", "is_", fun)
  # Already in is_ format
  if (grepl("^is_", fun)) return(fun)
  # Transform *_exists to is_existing_*
  if (grepl("_exists$", fun)) {
    base_name <- sub("_exists$", "", fun)
    return(paste0("is_existing_", base_name))
  }
  # For checkmate test_* functions
  if (grepl("^test_", fun)) {
    return(sub("^test_", "is_", fun))
  }
  # Shouldn't reach here given our patterns, but just in case
  paste0("is_", fun)
}

# Function to generate roxygen block for a re-export
generate_roxygen <- function(fun, is_name, package) {
  # Get parameter documentation
  param_docs <- get_param_docs(fun, package)
  description <- get_description(fun, package)
  title <- get_title(fun, package)

  link_location <- help(fun, package = eval(package)) |> fs::path_file()

  is_not_name <- stringr::str_replace(is_name, "^is", "is_not")
  are_name <- stringr::str_replace(is_name, "^is", "are")
  are_not_name <- stringr::str_replace(are_name, "^are", "are_not")

  c(
    sprintf("#' @aliases %s, %s, %s", are_name, is_not_name, are_not_name),
    sprintf("\n#' @title %s", title),
    "#'",
    sprintf("#' @description This is a re-export of \\code{\\link[%s:%s]{%s::%s()}}, modified to have standardised naming and standardised vector handling.", package, link_location, package, fun),
    sprintf("#' Documentation is atuomatically generated from the original package documentation. See the \\code{\\link[%s:%s]{original}} for full details.", package, link_location),
    "#' ",
    sprintf("#' %s", description),
    "#' ",
    "#' @returns ",
    sprintf("#' - Calls to \\code{%s} are guaranteed to return a scalar boolean (ie. a single \\code{TRUE} or \\code{FALSE} value). If an argument of length > 1 is given, \\code{FALSE} is returned.", is_name),
    sprintf("#' - \\code{%s} is a wrapper around \\code{\\link[purrr]{map_lgl}(vec, \\(i) %s(i, ...))}. A boolean vector of the same length as the input is guaranteed.", are_name, is_name),
    sprintf("#' - Calls to \\code{%s}/\\code{%s} negate the output of \\code{%s}/\\code{%s}.", is_not_name, are_not_name, is_name, are_name),
    "#' @md",
    "#'",
    param_docs,
    sprintf("#' @name %s", is_name),
    sprintf("#' @importFrom %s %s", package, fun),
    "#' @importFrom purrr map_lgl",
    generate_variant_functions(is_name, fun, package),
    ""
  )
}

# Function to generate the re-export file
generate_reexports <- function() {
  # Define packages and their patterns to match, in order of precedence. If a
  # function is re-exported from an earlier package it is not over-written by
  # functions of the same name from a later package.
  packages_config <- list(
    checkmate = "^test_",  # First priority
    rlang = "^is_",
    fs = c("^is_", "_exists$"),
    lubridate = "^is\\." # ,
    # base = "^is\\."        # Last priority
  )

  # Generate the exports
  all_exports <- character()
  exported_names <- character()  # Track what we've exported
  conflicts <- list()  # Track conflicts for reporting

  for (pkg in names(packages_config)) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      warning("Package '", pkg, "' not available")
      next
    }

    matching_funs <- get_matching_exports(pkg, packages_config[[pkg]])

    for (fun in matching_funs) {
      is_name <- standardise_name(fun)

      # Check if we already have this name (case insensitive)
      existing_match <- grep(paste0("^", is_name, "$"),
        exported_names,
        ignore.case = TRUE,
        value = TRUE)

      if (length(existing_match) > 0) {
        # Record the conflict
        conflicts[[is_name]] <- list(
          existing = existing_match,
          skipped = sprintf("%s::%s", pkg, fun)
        )
        next
      }

      roxygen_block <- generate_roxygen(fun, is_name, pkg)
      all_exports <- c(all_exports, roxygen_block)
      exported_names <- c(exported_names, is_name)
    }
  }

  # Create the file header
  header <- c(
    "#' Predicate Functions",
    "#'",
    "#' These functions are re-exports of predicate functions from various packages,",
    "#' standardised to use is_ prefix naming convention. All functionality is",
    "#' preserved from the original functions.",
    "#'",
    "#' Re-exports are included from:",
    "#' * checkmate: test_* functions renamed to is_*",
    "#' * rlang: is_* functions",
    "#' * fs: is_* functions and *_exists functions (latter renamed to is_existing_*)",
    "#' * lubridate: is.* functions renamed to is_*",
    "#'",
    "#' Note that in cases where multiple packages provide similar functionality",
    "#' (e.g., is_date), precedence is given to earlier packages in the list above.",
    "#'",
    "#' @name predicates",
    "#' @keywords internal",
    "NULL",
    "",
    "#' @noRd",
    "ensure_atomic_boolean <- function(fun, package) {
      args <- formals(get(fun, envir = asNamespace(package)))
      args_str <- paste(purrr::map2_chr(args,
        names(args),
        \\(arg, arg_name) {
          if (rlang::is_missing(arg)) return(arg_name)
          else if (purrr::is_null(arg)) return(paste0(arg_name, ' = NULL', collapse = ''))
          else if (rlang::is_call(arg)) return(paste0(arg_name, ' = ', rlang::expr_text(arg), collapse = ''))
          #else if (!is.na(arg) && arg == '') return(paste0(arg_name, ' = \"\"', collapse = ''))
          else if (rlang::is_character(arg)) return(paste0(arg_name, ' = \"', arg, '\"', collapse = ''))
          else paste0(arg_name, ' = ', as.character(arg), collapse = '')
        }), collapse = ', ')
      args_pass <- paste(sprintf('%s = %s', names(args), names(args)), collapse = ', ')

      eval(parse(text = sprintf(
        'function(%s) {
          result <- get(\"%s\", envir = asNamespace(\"%s\"))(%s)
          if (length(result) != 1) return(FALSE)
          result
        }', args_str, fun, package, args_pass)))
    }",
    ""
  )

  # Write to R directory
  writeLines(
    c(header, all_exports),
    "R/predicates.R"
  )

  # Report on what happened
  message("Generated re-exports in R/predicates.R")
  message(sprintf("Exported %d functions", length(exported_names)))

  if (length(conflicts) > 0) {
    message("\nThe following conflicts were found (later packages were skipped):")
    for (name in names(conflicts)) {
      conflict <- conflicts[[name]]
      message(sprintf("- %s: kept %s, skipped %s",
        name, conflict$existing, conflict$skipped))
    }
  }
}

# Function to generate the actual function definitions for variants
generate_variant_functions <- function(is_name, fun, package) {
  is_not_name <- stringr::str_replace(is_name, "^is", "is_not")
  are_name <- stringr::str_replace(is_name, "^is", "are")
  are_not_name <- stringr::str_replace(are_name, "^are", "are_not")

  # Get formal arguments of original function
  args <- formals(get(fun, envir = asNamespace(package)))
  arg_names <- names(args)

  # Create argument string for function definitions
  args_str    <- paste(arg_names, collapse = ", ")
  args_pass   <- paste(sprintf("%s = %s", arg_names, arg_names), collapse = ", ")

  # Create the function definitions
  funs <- c(
    "#' @export",
    sprintf("%s <- ensure_atomic_boolean('%s', '%s')", is_name, fun, package),
    "#' ",
    sprintf("#' @rdname %s", is_name),
    "#' @export",
    sprintf("%s <- function(%s) !%s(%s)",
      is_not_name, args_str, is_name, args_str))

  if (length(args) > 0) {
    funs <- c(
      funs,
      "#' ",
      sprintf("#' @rdname %s", is_name),
      "#' @export",
      sprintf("%s <- function(%s) {
      purrr::map_lgl(x, \\(%s) %s(%s))
    }", are_name, args_str, arg_names[[1]], is_name, args_pass),
      "#' ",
      sprintf("#' @rdname %s", is_name),
      "#' @export",
      sprintf("%s <- function(%s) !%s(%s)",
        are_not_name, args_str, are_name, args_pass),
      ""
    )
  }

  return(funs)
}

# Run the generation
generate_reexports()
