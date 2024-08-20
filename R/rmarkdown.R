#' Export to RMarkdown
#'
#' Export a workspace to Rmarkdown.
#'
#' @param workspace The workspace to export
#' @param file The file to export to
#' @param to_copy A vector of package names to copy the functions from (if found).
#' @param style Whether to run [styler::style_file()] on the generated code.
#' @param output Output file to use in the front matter.
#' @param ... Passed to [new_file()]
#' @export
export_rmarkdown <- function(
  file,
  workspace = blockr::get_workspace(),
  to_copy = c(),
  style = FALSE,
  ...,
  output = "html_document"
){
  f <- new_file(
    workspace = workspace,
    file = file,
    style = style,
    output = output,
    to_copy = to_copy,
    ...,
    class = "export_rmarkdown"
  )

  err <- export(f)

  if(is_error(err)){
    cat("Error exporting file:", err$message, "\n")
    return()
  }

  err <- post_write(f)

  if(is_error(err)){
    cat("Error exporting file:", err$message, "\n")
    return()
  }

  return(f)
}

#' @export
front_matter.export_rmarkdown <- function(x, ...) {
  output <- attr(x, "output")

  paste0(
    "---\n",
    sprintf(
      "title: %s\n",
      attr(x, "title")
    ),
    sprintf(
      "output: %s\n",
      output
    ),
    "---"
  )
}

#' @export
post_write.export_rmarkdown <- function(x, ...) {
  style <- attr(x, "style")

  if(!style)
    return()

  if(!requireNamespace("styler", quietly = TRUE))
    cat("{styler} required when style = TRUE")

  styler::style_file(file)
}

#' Export to RMarkdown Output
#'
#' Export a workspace to Rmarkdown.
#'
#' @inheritParams export_rmarkdown
#'
#' @export
export_rmarkdown_output <- function(
  file,
  workspace = blockr::get_workspace(),
  to_copy = c(),
  style = FALSE,
  ...,
  output = " html_document"
){
  tmp <- tempfile(fileext = ".Rmd")
  on.exit(unlink(tmp))

  f <- new_file(
    workspace = workspace,
    file = tmp,
    style = style,
    output = output,
    output_file = file,
    to_copy = to_copy,
    ...,
    class = "export_rmarkdown_output"
  )

  f <- export(f)

  if(is_error(f)){
    cat("Error exporting file:", f$message, "\n")
    return()
  }

  err <- post_write(f)

  if(is_error(err)){
    cat("Error exporting file:", err$message, "\n")
    return()
  }

  err <- render(f)

  if(is_error(err)){
    cat("Error exporting file:", err$message, "\n")
    return()
  }

  return(f)
}

#' @export
front_matter.export_rmarkdown_output <- function(x, ...) {
  output <- attr(x, "output")

  paste0(
    "---\n",
    sprintf(
      "title: %s\n",
      attr(x, "title")
    ),
    sprintf(
      "output: %s\n",
      output
    ),
    "---"
  )
}

#' @export
post_write.export_rmarkdown_output <- function(x, ...) {
  style <- attr(x, "style")

  if(!style)
    return()

  if(!requireNamespace("styler", quietly = TRUE))
    cat("{styler} required when style = TRUE")

  styler::style_file(file)
}

#' @export
render.export_rmarkdown_output <- function(x, ...) {
  rmarkdown::render(attr(x, "file"), output_file = attr(x, "output_file"))
}
