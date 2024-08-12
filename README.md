<!-- badges: start -->
<!-- badges: end -->

# blockr.export

Export blockr workspaces as documents.

## Installation

You can install the development version of blockr.export from [GitHub](https://github.com/) with:

``` r
# install.packages("remotes")
remotes::install_github("blockr-org/blockr.export")
```

## Exports

Export to a file, use `export_rmarkdown_output` to render the 
output with Rmarkdown.

- Rmarkdown: `export_rmarkdown`
- Rmarkdown Output: `export_rmarkdown_output`
- Markdown: `export_markdown`

## Extend

Create a custom file with `new_file`, then, optionally,
customise the methods.

Methods:

- `front_matter`: returns front matter
- `content`: returns the content to write in the file.
- `footer`: returns the footer to place at the bottom of the file.
- `write`: write the file
- `post_write`: callback to run after the file has been written
- `render`: render the generated file, used with Rmarkdown

## Example

``` r
library(shiny)
library(blockr)
library(blockr.export)

stack <- new_stack(
  new_dataset_block,
  new_select_block
)

ui <- fluidPage(
  theme = bslib::bs_theme(5L),
  downloadButton(
    "download",
    "Download"
  ),
  generate_ui(stack)
)

server <- function(input, output, session) {
  output$download <- shiny::downloadHandler(
    filename = \(x){
      "file.Rmd"
    },
    content = \(file){
      export_markdown(file)
    }
  )

  generate_server(stack)
}

shinyApp(ui, server)
```
