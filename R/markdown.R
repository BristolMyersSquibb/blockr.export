#' Export to Markdown
#'
#' Export a workspace to markdown.
#'
#' @param workspace The workspace to export
#' @param file The file to export to
#' @param to_copy A vector of package names to copy the functions from (if found).
#' @export
export_markdown <- function(
  file,
  workspace = blockr::get_workspace(),
  to_copy = c()
){
  f <- new_file(
    workspace = workspace,
    file = file,
    to_copy = to_copy,
    class = "export_markdown"
  )

  err <- export(f)

  if(is_error(err)){
    cat("Error exporting file:", err$message, "\n")
  }

  return(f)
}

#' @export
front_matter.export_markdown <- function(x, ...) {
  paste0(
    "---\n",
    sprintf(
      "title: %s\n",
      attr(x, "title")
    ),
    "---"
  )
}
