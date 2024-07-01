make_file <- function(chunks = c("stack", "block"), workspace = blockr::get_workspace()){
  chunks <- match.arg(chunks)

  chunks <- switch(chunks,
    stack = make_stack(workspace),
    block = make_block(workspace)
  )

  chunks <- paste(chunks, collapse = "\n\n")

  cat(chunks)

  chunks
}

make_stack <- function(workspace){
  workspace |>
    ls() |>
    lapply(\(stack) {
      stack <- get(stack, envir = workspace)
      code <- blockr::generate_code(stack) |>
        deparse()

      code <- paste0("```{r ", attr(stack, "name"), "}\n", code, "\n```")

      title <- attr(stack, "title")

      paste0("##", title, "\n\n", code)
    })
}

make_block <- function(workspace){
  lapply(workspace, \(stack) {
    stack <- get(stack, envir = workspace)

    blocks <- lapply(stack, \(block) {
      code <- blockr::generate_code(block) |>
        deparse()

      code <- paste0("```{r ", attr(block, "name"), "}\n", code, "\n```")

      name <- attr(stack, "name")

      paste0("###", name, "\n\n", code)
    })

    chunks <- paste(blocks, collapse = "\n\n")

    title <- attr(stack, "title")

    paste0("##", title, "\n\n", chunks)
  })
}

write_file <- function(
  file,
  writer,
  chunks = c("stack", "block"),
  workspace = blockr::get_workspace()
){
  if(missing(writer)) stop("Missing `writer`")
  if(!is.function(writer)) stop("`writer` must be a function")
  if(missing(file)) stop("No file provided")
  chunks <- match.arg(chunks)

  generated <- make_file(chunks, workspace)

  writer(generated, file)
}
