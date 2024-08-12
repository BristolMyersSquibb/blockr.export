make_file <- function(
  workspace = blockr::get_workspace(),
  to_copy = c()
){
  stacks <- workspace |>
    ls() |>
    lapply(\(stack) {
      stack <- get(stack, envir = workspace)

      blocks <- lapply(stack, \(block) {
        code <- blockr::generate_code(block) |>
          deparse() |>
          remove_to_copy_ns(to_copy)

        deps <- get_block_dependencies(block, code, to_copy)

        list(
          code = code,
          deps = deps
        )
      })

      codes <- sapply(blocks, \(block) {
        block$code
      })

      code <- blockr::generate_code(stack) |>
        deparse() |>
        remove_to_copy_ns(to_copy) |>
        unlist()

      code <- paste0(code, collapse = "\n") |> replace_data()

      title <- attr(stack, "title")
      code <- paste0("```{r ", attr(stack, "name"), "}\n", code, "\n```")

      list(
        content = code,
        internals = lapply(blocks, \(block) block$deps$internal),
        packages = lapply(blocks, \(block) block$deps$packages)
      )
    })

  content <- sapply(stacks, \(stack) stack$content)
  content <- paste0(content, collapse = "\n")

  internals <- lapply(stacks, \(stack) stack$internal)
  imports <- lapply(stacks, \(stack) stack$packages)

  content |>
    add_dependencies(internals, imports)
}

# adds dependencies to a file.
add_dependencies <- function(content, internals, packages) {
  internals <- unlist(internals)
  internals <- paste0(internals, collapse = "\n")

  packages <- unlist(packages)
  packages <- packages[packages != ""]

  library <- ""
  for(package in packages) {
    library <- paste0(
      library,
      "\n",
      "libary(",
      package,
      ")"
    )
  }

  paste0(
    "```{r packages}\nlibrary(magrittr)\n",
    library,
    "\n```\n\n",
    "```{r internals}\n",
    internals,
    "\n```\n\n",
    content
  )
}

write_file <- function(
  file,
  writer,
  workspace = blockr::get_workspace(),
  to_copy = c()
){
  if(missing(writer)) stop("Missing `writer`")
  if(!is.function(writer)) stop("`writer` must be a function")
  if(missing(file)) stop("No file provided")

  generated <- make_file(workspace, to_copy)

  writer(generated, file)
}

replace_data <- function(code) {
  code <- stringr::str_replace_all(code, "(?<=[\\s\\(\\[\\{=])data(?=[\\s\\)\\]\\}=])", ".")
  code <- stringr::str_replace_all(code, " data ", " . ")
  stringr::str_replace_all(code, " data,", " .,")
}
