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

- Rmarkdown
- Markdown
- HTML

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
      "file.md"
    },
    content = \(file){
      export_markdown(file)
    }
  )

  generate_server(stack)
}

shinyApp(ui, server)
```

