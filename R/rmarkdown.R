#' Export to RMarkdown
#'
#' Export a workspace to Rmarkdown.
#'
#' @param workspace The workspace to export
#' @param chunks Whether code chunks represent entire stacks or individual blocks.
#' @param file The file to export to
#' @param to_copy A vector of package names to copy the functions from (if found).
#' @export
export_rmarkdown <- function(
  file,
  chunks = c("stack", "block"),
  workspace = blockr::get_workspace(),
  to_copy = c()
){
  write_file(file, write_rmd, chunks, workspace, to_copy = to_copy)
}

write_rmd <- function(content, file) {
  content <- paste0(
    "---\n",
    "title: blockr\n",
    "output: html_document\n",
    "---\n\n",
    "# Workdspace\n\n",
    content
  )
  writeLines(content, file)
}
