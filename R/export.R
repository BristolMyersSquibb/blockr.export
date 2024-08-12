#' Export 
#' 
#' Export a file.
#' 
#' @inheritParams methodParams
#' 
#' @export
export <- function(x, ...) UseMethod("export")

#' @export
export.export_file <- function(x, ...) {
  fm <- safe_front_matter(x)
  if(is_error(fm)) return(fm)

  foot <- safe_footer(x)
  if(is_error(foot)) return(foot)

  content <- safe_content(x)
  if(is_error(content)) return(content)

  attr(x, "content") <- c(fm, content, foot)

  ok <- safe_write(x)
  if(is_error(ok)) return(ok)

  ok <- safe_post_write(x)
  if(is_error(ok)) return(ok)

  invisible(x)
}
