#' Export to HTML
#'
#' Export a workspace to HTML via Rmarkdown.
#'
#' @param workspace The workspace to export
#' @param chunks Whether code chunks represent entire stacks or individual blocks.
#' @param file The file to export to
#' @export
export_markdown <- function(
  file,
  chunks = c("stack", "block"),
  workspace = blockr::get_workspace()
){
  write_file(file, write_html, chunks, workspace)
}

write_html <- function(content, file) {
  if (!requireNamespace("rmarkdown", quietly = TRUE)) {
    stop("This functions requires the rmarkdown package")
  }

  temp_file <- tempfile(fileext = ".Rmd")

  on.exit({
    unlink(temp_file)
  })

  content <- paste0(
    "---\n",
    "title: blockr\n",
    "output: html_document\n",
    "---\n\n",
    "\n\n",
    content
  )

  writeLines(content, temp_file)

  success <- tryCatch(
    rmarkdown::render(
      temp_file,
      file
    )
  )

  if (inherits(success, "error"))
    stop("Error kniting Rmarkdown to HTML")

  invisible()
}
