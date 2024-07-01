#' Export a RMarkdown file
#'
#' @param workspace The workspace to export
#' @param chunks Whether code chunks represent entire stacks or individual blocks.
#' @param file The file to export to
#' @export
export_rmarkdown <- function(
  file,
  chunks = c("stack", "block"),
  workspace = blockr::get_workspace()
){
  write_file(file, write_rmd, chunks, workspace)
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
