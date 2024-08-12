#' Post Write 
#' 
#' Run callback after the ([content]) for a [new_file()] as been written.
#' 
#' @inheritParams methodParams
#' 
#' @export
post_write <- function(x, ...) UseMethod("post_write")

#' @export
post_write.export_file <- function(x, ...) {
  return()
}

safe_post_write <- function(x, ...) {
  ok <- safe_eval(post_write(x, ...))

  if(is_error(ok)) return(ok)
  warn_if(ok)

  return(ok)
}
