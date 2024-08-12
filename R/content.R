#' Content
#' 
#' Generate content for a [new_file()].
#' 
#' @inheritParams methodParams
#' 
#' @export
content <- function(x, ...) UseMethod("content")

#' @export
content.export_file <- function(x, ...) {
  generate_content(x, ...)
}

safe_content <- function(x, ...) {
  content <- safe_eval(content(x, ...)) 

  if(is_error(content)) return(content)
  warn_if(content)

  invisible(content)
}

generate_content <- function(x, ...){
  stacks <- attr(x, "workspace") |>
    ls() |>
    lapply(\(stack) {
      stack <- get(stack, envir = attr(x, "workspace"))

      blocks <- lapply(stack, \(block) {
        code <- blockr::generate_code(block) |>
          deparse() |>
          remove_to_copy_ns(attr(x, "to_copy"))

        deps <- get_block_dependencies(block, code, attr(x, "to_copy"))

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
        remove_to_copy_ns(attr(x, "to_copy")) |>
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

replace_data <- function(code) {
  code <- stringr::str_replace_all(code, "(?<=[\\s\\(\\[\\{=])data(?=[\\s\\)\\]\\}=])", ".")
  code <- stringr::str_replace_all(code, " data ", " . ")
  stringr::str_replace_all(code, " data,", " .,")
}
