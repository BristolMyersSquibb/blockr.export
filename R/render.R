#' Render
#' 
#' Render a [new_file()], used to render Rmarkdown files.
#' 
#' @inheritParams methodParams
#' 
#' @export
render <- function(x, ...) UseMethod("render")

#' @export
render.export_file <- function(x, ...) {
  return()
}

safe_render <- function(x, ...) {
  ok <- safe_eval(render(x, ...))

  if(is_error(ok)) return(ok)

  warn_if(ok)
  invisible(ok)
}
