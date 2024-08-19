#' Code
#' 
#' Generate code for a blockr block
#' 
#' @param x Block object.
#' @param ... Ignored.
#' 
#' @export
code <- function(x, ...) UseMethod("code")

#' @export
code.block <- function(x, ...) {
  blockr::generate_code(x) |>
    deparse()
}

#' @export
code.markdown_block <- function(x, ...) {
  prog <- blockr::generate_code(x)
  output <- eval(prog)

  if(length(output$original))
    return(output$original)

  return(output$text)
}

safe_code <- function(x, ...) {
  ok <- safe_eval(code(x, ...))

  if(is_error(ok)) return(ok)
  warn_if(ok)

  return(ok)
}

#' Code Fence
#' 
#' Generate code fence for a blockr stack
#' 
#' @param x Stack object.
#' @param name Name of the stack.
#' @param code Code generated for the stack.
#' @param ... Ignored.
#' 
#' @export
code_fence <- function(x, name, code, ...) UseMethod("code_fence")

#' @export
code_fence.stack <- function(x, name, code, ...) {
  has_md <- stack_has_markdown_block(x)

  if(!has_md) {
    return(paste0("```{r ", name, "}\n", code, "\n```"))
  }

  return(code)
}

safe_code_fence <- function(x, ...) {
  ok <- safe_eval(code_fence(x, ...))

  if(is_error(ok)) return(ok)
  warn_if(ok)

  return(ok)
}

stack_has_markdown_block <- function(stack) {
  for (block in stack) {
    if(inherits(block, "markdown_block")) {
      return(TRUE)
    }
  }

  return(FALSE)
}
