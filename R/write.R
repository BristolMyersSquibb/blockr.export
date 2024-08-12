#' Write 
#' 
#' Write generated content ([content]) for a [new_file()].
#' 
#' @inheritParams methodParams
#' 
#' @export
write <- function(x, ...) UseMethod("write")

#' @export
write.export_file <- function(x, ...) {
  writeLines(attr(x, "content"), attr(x, "file"))
}

safe_write <- function(x, ...) {
  ok <- safe_eval(write(x, ...))

  if(is_error(ok)) return(ok)
  warn_if(ok)

  return(ok)
}
