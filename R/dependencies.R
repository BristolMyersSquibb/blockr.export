# retrieves block dependencies
# returns a named list that comprises of
# the `internal` functions
# the `packages`
# out code block depends on.
get_block_dependencies <- function(
  block,
  code,
  to_copy = c()
) {
  if (missing(code))
    stop("missing block")

  if (missing(code))
    stop("missing code")

  # find function calls within expression
  fns <- stringr::str_extract_all(
    code,
    "(?<=\\s|^)[^(\\s]+(?=\\()"
  )

  # no function calls in the expression
  # no need to go further
  if (!length(fns))
    return()

  block_desc <- find_block_desc(block)

  core_package <- attr(block_desc, "package")

  # we assume it's in a package
  # otherwise we'd have to get from parent(?) env
  # scan for library() or require(), etc.
  if(!length(core_package))
    return()

  ext_packages <- get_package_deps(core_package)
  ext_packages <- ext_packages[!ext_packages %in% to_copy]

  # if we find function calls we look them up
  # in the list of dependencies we have found
  # if they are found we return the package where it comes from
  # so we can library(<package>) in our code.
  find_functions(fns, core_package, ext_packages, to_copy = to_copy)
}

# recursively go through expressions and functions bodies
# to retrieve any dependencies required to run them
find_functions <- function(
  fns,
  core_package,
  ext_packages = list(),
  stack = list(internals = list(), ext_packages = c()),
  to_copy = c()
) {
  # find external packages used by any of the function calls
  ext_packages_used <- find_packages(fns, ext_packages)

  # add used pacakges to stack
  stack$ext_packages <- c(stack$ext_packages, ext_packages_used)

  # retrieve all functions: exported and internals
  pkg_functions <- safe_get_functions(core_package, unexported = TRUE)

  # filter down to those that match our calls present
  # in the code/expression
  fns <- gsub("^.*::", "", fns)
  fns2 <- unlist(fns)
  fns <- pkg_functions[pkg_functions %in% fns]

  # call and deparse the functions to obtain source code
  functions <- sapply(seq_along(fns), \(i) {
    # note that we import the source code of all functions
    # within the package that includes the blocks
    call <- paste0(core_package, ":::", fns[i])
    code <- tryCatch(
      eval(parse(text = call)),
      error = \(e) e
    )

    if(inherits(code, "error")){
      cat("Error generating", fns[i], ":", code$message, "\n")
      return("")
    }

    code <- code |>
      deparse() |>
      paste0(collapse = "\n")

    paste(fns[i], "<-", code)
  })

  # we force copy functions from to_copy packages
  to_copy_functions <- sapply(to_copy, \(pkg) {
    pkg_functions <- safe_get_functions(pkg, unexported = FALSE)

    fns <- pkg_functions[pkg_functions %in% fns2]

    sapply(seq_along(fns), \(i) {
      # note that we import the source code of all functions
      # within the package that includes the blocks
      call <- paste0(pkg, ":::", fns[i])
      code <- tryCatch(
        eval(parse(text = call)),
        error = \(e) e
      )

      if(inherits(code, "error")){
        cat("Error generating", fns[i], ":", code$message, "\n")
        return("")
      }

      code <- code |>
        deparse() |>
        paste0(collapse = "\n")

      paste(fns[i], "<-", code)
    }) |>
      unlist()
  }) |>
    unlist()

  functions <- c(functions, to_copy_functions)

  # add to stack
  stack$internals <- list(stack$internals, functions)

  # find function calls within expression
  fns <- stringr::str_extract_all(
    functions,
    "(?<=\\s|^)[^(\\s]+(?=\\()",
  ) |>
    unlist()

  # if we have found function calls we go back to the top
  if(length(fns))
    return(find_functions(fns, core_package, ext_packages, stack, to_copy))

  list(
    internal = paste0(unlist(stack$internals), collapse = "\n\n"),
    packages = stack$ext_packages |> unname() |> unlist() |> unique()
  )
}

# finds packages our code depends on
# scans dependencies (`package`) for any `fns`
# returns package names where `fns` found
find_packages <- function(fns, packages = c()) {
  if(!length(packages))
    return()

  pkg_functions <- lapply(packages, safe_get_functions, unexported = FALSE)
  names(pkg_functions) <- packages

  packages <- c()
  for(i in seq_along(pkg_functions)) {
    fns_used <- pkg_functions[[i]][fns %in% pkg_functions[[i]]]

    if(!length(fns_used))
      next

    packages <- c(packages, names(pkg_functions)[i])
  }

  unique(packages)
}

# safely retrieve a function from namespace
safe_get_functions <- function(package, unexported = FALSE) {
  if(!length(package))
    return()

  ns <- tryCatch(
    do.call(getNamespace, list(package)),
    err = \(e) e
  )

  if(inherits(ns, "error")) {
    cat("error retrieving namespace from", package, ":", ns$message, "\n")
    return()
  }

  fns <- tryCatch(
    ls(ns, all.names = unexported),
    err = \(e) e
  )

  if (inherits(fns, "error")) {
    cat("error retrieving namespace from", package, ":", fns$message, "\n")
    return()
  }

  return(fns)
}

find_block_desc <- function(block) {
  block_classes <- attr(block, "class")
  blocks <- blockr::available_blocks()

  for(block in blocks) {
    classes <- attr(block, "classes")
    if(all(classes %in% block_classes))
      return(block)
  }

  return()
}

get_package_deps <- function(package) {
  pkgs <- utils::installed.packages() |>
    as.data.frame()

  package_entry <- pkgs[pkgs$Package == package, ]

  if(!nrow(package_entry))
    return()

  imports <- clean_package(package_entry$Imports)
  suggests <- clean_package(package_entry$Suggests)
  depends <- clean_package(package_entry$Depends)

  packages <- c(imports, suggests, depends) |>
    unique()

  packages <- packages[!is.na(packages)]
  packages[packages != ""]
}

clean_package <- function(function_string) {
  packages <- strsplit(function_string, ",")[[1]]
  packages <- gsub("\\n", "", packages)
  packages <- strsplit(packages, " ") |>
    sapply(\(pkg) trimws(pkg[[1]]))
  packages[!"R" %in% packages]
}
