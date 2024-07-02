make_file <- function(chunks = c("stack", "block"), workspace = blockr::get_workspace()){
  chunks <- match.arg(chunks)

  switch(chunks,
    stack = make_stack(workspace),
    block = make_block(workspace)
  )
}

make_stack <- function(workspace){
  stacks <- workspace |>
    ls() |>
    lapply(\(stack) {
      stack <- get(stack, envir = workspace)

      blocks <- lapply(stack, \(block) {
        code <- blockr::generate_code(block) |>
          deparse()

        deps <- get_block_dependencies(block, code)

        list(
          code = code,
          deps = deps
        )
      })

      codes <- sapply(blocks, \(block) {
        block$code
      })

      codes <- paste0(unlist(codes), collapse = "%>%\n\t")
      code <- paste0("```{r ", attr(stack, "name"), "}\n", codes, "\n```")

      title <- attr(stack, "title")

      list(
        content = paste0("## ", title, "\n\n", code),
        deps = blocks$deps
      )
    })

  content <- sapply(stacks, \(stack) stack$content)
  content <- paste0(content, collapse = "\n")

  deps <- sapply(stacks, \(stack) stack$deps)
  print(deps)

  return(content)
}

make_block <- function(workspace){
  lapply(workspace, \(stack) {
    stack <- get(stack, envir = workspace)

    blocks <- lapply(stack, \(block) {
      code <- blockr::generate_code(block) |>
        deparse()

      deps <- get_block_dependencies(block, code)

      code <- paste0("```{r ", attr(block, "name"), "}\n", code, "\n```")

      name <- attr(stack, "name")

      paste0("###", name, "\n\n", code)

      list(
        code = code,
        deps = deps
      )
    })

    chunks <- paste(unlist(blocks$code), collapse = "\n\n")

    title <- attr(stack, "title")

    list(
      content = paste0("##", title, "\n\n", chunks),
      deps = blocks$deps
    )
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
