#' Export to set of R Scripts
#'
#' Export a workspace to a collection R scripts.
#'
#' @param workspace The workspace to export
#' @param file The file to export to
#' @param to_copy A vector of package names to copy the functions from (if found).
#' @param style Whether to run [styler::style_file()] on the generated code.
#' @export
export_gzip <- function(
  file,
  workspace = blockr::get_workspace(),
  to_copy = c(),
  style = FALSE
){
  make_gzip(workspace, to_copy, file, style)
}

make_gzip <- function(workspace, to_copy, out, style = FALSE){
  dir <- tempdir()
  files <- c()

  stacks <- workspace |>
    ls() |>
    lapply(\(stack) {
      stack <- get(stack, envir = workspace)

      blocks <- lapply(stack, \(block) {
        code <- blockr::generate_code(block) |>
          deparse() |>
          remove_to_copy_ns(to_copy)

        deps <- get_block_dependencies(block, code, to_copy)

        list(
          code = code,
          deps = deps
        )
      })

      code <- blockr::generate_code(stack) |>
        deparse() |>
        remove_to_copy_ns(to_copy) |>
        unlist()

      code <- paste0(code, collapse = "\n") |> replace_data()

      code <- paste0("```{r ", attr(stack, "name"), "}\n", code, "\n```")

      internal <- sapply(blocks, \(block) block$deps$internal)
      packages <- sapply(blocks, \(block) block$deps$packages) |>
        unlist()

      internal <- internal[!is.null(internal)]
      packages <- packages[!is.null(packages)]

      internal <- internal[internal != ""]
      packages <- packages[packages != ""]

      packages <- sapply(packages, \(pkg) {
        paste0("library(", pkg, ")")
      })

      filename <- sprintf("%s.R", attr(stack, "name"))
      path <- file.path(dir, filename)
      files <<- c(files, path)

      internal <- paste0(internal, collapse = "\n")
      packages <- paste0(packages, collapse = "\n")

      codes <- paste(
        "# Packages\n",
        packages,
        "\n\n",
        "# Functions copied\n",
        internal,
        "\n\n",
        codes,
        collapse = "\n\n"
      )

      writeLines(
        codes,
        path
      )

      if(!style)
        return()

      if(!requireNamespace("styler", quietly = TRUE))
        cat("{styler} required when style = TRUE")

      styler::style_dir(path)
    })

  utils::zip(out, files)
}

remove_to_copy_ns <- function(code, to_copy = c()) {
  if(!length(to_copy))
    return(code)

  # create regular expression
  pattern <- paste0(to_copy, "::", collapse = "|")

  gsub(pattern, "", code)
}
