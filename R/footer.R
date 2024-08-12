#' Footer 
#' 
#' Generate footer for a [new_file()].
#' 
#' @inheritParams methodParams
#' 
#' @export
footer <- function(x, ...) UseMethod("footer")

#' @export
footer.export_file <- function(x, ...) {
  return("")
}

safe_footer <- function(x, ...) {
  ok <- safe_eval(footer(x, ...))

  if(is_error(ok)) return(ok)
  warn_if(ok)

  return(ok)
}
