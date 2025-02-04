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

# Function to generate roxygen block for a re-export
generate_roxygen <- function(fun, is_name, package) {
  # Get the original documentation
  help_text <- help(fun, package)
  title <- utils:::.getHelpTitle(help_text)

  c(
    sprintf("\n#' %s", title),
    "#'",
    sprintf("#' This is a re-export of \\code{%s::%s}.", package, fun),
    "#' See the original package documentation for full details.",
    "#'",
    sprintf("#' @name %s", is_name),
    sprintf("#' @importFrom %s %s", package, fun),
    "#' @export",
    sprintf("%s <- %s::%s", is_name, package, fun),
    ""
  )
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
  # Shouldn't reach here given our patterns, but just in case
  paste0("is_", fun)
}

# Function to generate the re-export file
generate_reexports <- function() {
  # Define packages and their patterns to match
  packages_config <- list(
    checkmate = "^test_",
    rlang = "^is_",
    fs = c("^is_", "_exists$"),
    lubridate = "^is\\."
  )

  # Generate the exports
  all_exports <- character()

  for (pkg in names(packages_config)) {
    matching_funs <- get_matching_exports(pkg, packages_config[[pkg]])

    for (fun in matching_funs) {
      is_name <- standardise_name(fun)
      roxygen_block <- generate_roxygen(fun, is_name, pkg)
      all_exports <- c(all_exports, roxygen_block)
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
}

# Run the generation
generate_reexports()
