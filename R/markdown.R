#' Export to Markdown
#'
#' Export a workspace to markdown.
#'
#' @param workspace The workspace to export
#' @param file The file to export to
#' @param copy_deps Whether to copy the source code of first level dependencies.
#' @param to_copy A vector of package names to copy the functions from (if found).
#' @export
export_markdown <- function(
  file,
  workspace = blockr::get_workspace(),
  copy_deps = FALSE,
  to_copy = c()
){
  write_file(file, write_md(), workspace, to_copy)
}

write_md <- function() {
  \(content, file) {
    content <- paste0(
      "---\n",
      "title: blockr\n",
      "---\n\n",
      "\n\n",
      content
    )

    writeLines(content, file)
  }
}
