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
  description <- rd[which(tools:::RdTags(rd) == "\\description")][[1]] |>
    as.character() |>
    paste0(collapse = "") |>
    stringr::str_replace_all("\\n", "\n#' ") |>
    stringr::str_remove("^\\n")

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

  add_tags <- function(x) {
    tag <- attributes(x)$Rd_tag
    if (purrr::is_null(tag)) {
      if (purrr::is_empty(x)) return(NULL)
      else return(paste0(as.character(x), collapse = ""))
    } else if (stringr::str_starts(tag, "\\\\")) {
      if (purrr::is_empty(x)) return(tag)
      else if (tag == "\\item") {
        out <- paste0(tag,
          "{",
          paste0(purrr::map_chr(x[[1]], add_tags), collapse = ""),
          "}{",
          paste0(purrr::map_chr(x[[2]], add_tags), collapse = ""),
          "}",
          collapse = "")
      } else {
        out <- paste0(tag,
          "{",
          paste0(purrr::map_chr(x, add_tags), collapse = ""),
          "}",
          collapse = "")
        return(out)
      }
    } else return(paste0(as.character(x), collapse = ""))
  }
  # Get argument names and descriptions
  arg_names <- purrr::map_chr(args, function(arg) arg[[1]][[1]][[1]][1])
  arg_desc <- purrr::map_chr(args, function(arg) {
    purrr::map_chr(arg[[2]], add_tags) |>
      paste0(collapse = "")
  }) |>
    stringr::str_replace_all("\\n", "\n#' ")

  # Check if arguments are actually used in the function
  fun_args <- names(formals(get(fun, envir = asNamespace(package))))
  valid_args <- arg_names %in% fun_args

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

  c(
    sprintf("\n#' @title %s function from %s", is_name, package),
    "#'",
    sprintf("#' @description Re-export of \\code{%s::%s}.", package, fun),
    "#' See the original package documentation for full details.", description,
    "#'",
    param_docs,
    sprintf("#' @name %s", is_name),
    sprintf("#' @importFrom %s %s", package, fun),
    "#' @export",
    sprintf("%s <- %s::%s", is_name, package, fun),
    ""
  )
}

# Function to generate the re-export file
generate_reexports <- function() {
  # Define packages and their patterns to match, in order of precedence
  packages_config <- list(
    checkmate = "^test_",  # First priority
    rlang = "^is_",
    fs = c("^is_", "_exists$"),
    lubridate = "^is\\."   # Last priority
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
    "#' standardised to use is_ prefix naming convention. All functionality and",
    "#' documentation is preserved from the original functions.",
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

# Run the generation
generate_reexports()
