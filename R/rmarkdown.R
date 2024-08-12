#' Export to RMarkdown
#'
#' Export a workspace to Rmarkdown.
#'
#' @param workspace The workspace to export
#' @param file The file to export to
#' @param copy_deps Whether to copy the source code of first level dependencies.
#' @param to_copy A vector of package names to copy the functions from (if found).
#' @param style Whether to run [styler::style_file()] on the generated code.
#' @export
export_rmarkdown <- function(
  file,
  workspace = blockr::get_workspace(),
  copy_deps = FALSE,
  to_copy = c(),
  style = FALSE
){
  write_file(file, write_rmd(style), workspace, to_copy = to_copy)
}

write_rmd <- function(style = FALSE) {
  \(content, file){
    content <- paste0(
      "---\n",
      "title: blockr\n",
      "output: html_document\n",
      "---\n\n",
      "# Workdspace\n\n",
      content
    )

    writeLines(content, file)

    if(!style)
      return()

    if(!requireNamespace("styler", quietly = TRUE))
      cat("{styler} required when style = TRUE")

    styler::style_file(file)
  }
}
