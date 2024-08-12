#' Front matter 
#' 
#' Generate the front matter for a [new_file()].
#' 
#' @inheritParams methodParams
#' 
#' @export
front_matter <- function(x, ...) UseMethod("front_matter")

#' @export
front_matter.export_file <- function(x, ...) {
  return("")
}

safe_front_matter <- function(x, ...) {
  fm <- safe_eval(front_matter(x, ...))

  if(is_error(fm)) return(fm)
  warn_if(fm)

  if (is.null(fm)) fm <- ""

  if(fm == "") return(fm)

  paste0(fm, "\n\n")
}
