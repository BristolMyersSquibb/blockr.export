#' New file
#' 
#' Create a new file
#' 
#' @param file Path to file to generate.
#' @param title Title of the file.
#' @param workspace Blockr workspace as returned by [blockr::get_workspace()].
#' @param ... Any additional attribute.
#' @param class Class of the new file.
#' 
#' @name new_file
#' 
#' @export
new_file <- function(file, ..., title = "Blockr workspace", workspace = blockr::get_workspace(), class){
  stopifnot(!missing(file), !missing(class))

  structure(
    list(),
    extension = tools::file_ext(file),
    file = file,
    workspace = workspace,
    to_copy = c(),
    content = c(),
    title = title,
    ...,
    class = c(class, "export_file")
  )
}

#' New file params
#' @param x Output of [new_file()]
#' @param ... Ignored.
#' @name methodParams
NULL
