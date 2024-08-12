#' Export to HTML
#'
#' Export a workspace to HTML via Rmarkdown.
#'
#' @param workspace The workspace to export
#' @param file The file to export to
#' @param copy_deps Whether to copy the source code of first level dependencies.
#' @param to_copy A vector of package names to copy the functions from (if found).
#' @param style Whether to run [styler::style_file()] on the generated code.
#' @export
export_html <- function(
  file,
  workspace = blockr::get_workspace(),
  copy_deps = FALSE,
  to_copy = c(),
  style = FALSE
){
  write_file(file, write_html(style), workspace, to_copy)
}

write_html <- function(style = FALSE) {
  \(content, file){
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

    if(style) {
      if(!requireNamespace("styler", quietly = TRUE))
        cat("{styler} required when style = TRUE")

      styler::style_file(temp_file)
    }

    success <- tryCatch(
      rmarkdown::render(
        temp_file,
        file
      ),
      error = \(e) e
    )

    if (inherits(success, "error"))
      stop("Error kniting Rmarkdown to HTML")

    invisible()
  }
}
