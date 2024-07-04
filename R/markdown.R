#' Export to Markdown
#'
#' Export a workspace to markdown.
#'
#' @param workspace The workspace to export
#' @param chunks Whether code chunks represent entire stacks or individual blocks.
#' @param file The file to export to
#' @param to_copy A vector of package names to copy the functions from (if found).
#' @export
export_markdown <- function(
  file,
  chunks = c("stack", "block"),
  workspace = blockr::get_workspace(),
  to_copy = c()
){
  write_file(file, write_md, chunks, workspace, to_copy)
}

write_md <- function(content, file) {
  content <- paste0(
    "---\n",
    "title: blockr\n",
    "---\n\n",
    "\n\n",
    content
  )
  writeLines(content, file)
}
