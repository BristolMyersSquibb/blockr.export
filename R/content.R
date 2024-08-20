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
  stacks <- attr(x, "workspace") |> ls()

  o <- attr(x, "order")

  if(!is.null(o))
    stacks <- stacks[order(o)]

  stacks <- stacks |> 
    lapply(\(stack) {
      stack <- get(stack, envir = attr(x, "workspace"))

      blocks <- lapply(stack, \(block) {
        code <- safe_code(block, x) |>
          remove_to_copy_ns(attr(x, "to_copy")) |>
          replace_data()

        deps <- get_block_dependencies(block, code, attr(x, "to_copy"))

        list(
          code = paste0(code, collapse = "\n"),
          deps = deps
        )
      })

      code <- sapply(blocks, \(block) {
        block$code
      }) |>
        paste0(collapse = "%>%\n")

      code <- code_fence(stack, x, attr(stack, "name"), code)

      if(!stack_has_markdown_block(stack))
        code <- paste0(
          "\n## ", attr(stack, "title"),
          "\n\n",
          code
        )

      list(
        title = attr(stack, "title"),
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
    add_dependencies(internals, imports, x)
}

# adds dependencies to a file.
add_dependencies <- function(content, internals, packages, file) {
  echo <- !inherits(file, "export_rmarkdown_output")
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

  output <- paste0(
    "```{r packages, message=FALSE, echo=", echo, "}\nlibrary(magrittr)\n",
    library,
    "\n```\n\n"
  )

  if(length(internals)) {
    output <- paste0(
      output,
      "```{r internals, message=FALSE, echo=", echo, "}\n",
      internals,
      "\n```\n\n"
    )
  }

  paste0(
    output,
    content
  )
}

replace_data <- function(code) {
  code <- stringr::str_replace_all(code, "(?<=[\\s\\(\\[\\{=])data(?=[\\s\\)\\]\\}=])", ".")
  code <- stringr::str_replace_all(code, " data ", " . ")
  stringr::str_replace_all(code, " data,", " .,")
}
