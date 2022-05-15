
<!-- README.md is generated from README.Rmd. Please edit that file -->

# ShinyEditor : HTML Editor in Shiny App

<!-- badges: start -->

<!-- badges: end -->

In this package we use [tinyMCE.js](https://www.tiny.cloud/) which is
considered as world’s most advanced rich text editor. It allows various
styling options to style text. You can simply copy your content from MS
Word / Excel and can generate HTML.

## Installation

You can install the released version of ShinyEditor from
[GitHub](https://github.com/) with:

``` r
# install.packages("remotes")
remotes::install_github("deepanshu88/ShinyEditor")
```

## Steps

You need `API` key from [tiny](https://www.tiny.cloud/) to generate HTML
editor. API key is absolutely free, just Sign Up required. Once done, add
the domains you wish to assign to your API key and click `Add domain`.
This will get your domain approved. See the
[instructions](https://www.tiny.cloud/blog/how-to-get-tinymce-cloud-up-in-less-than-5-minutes/)

## Example

This is a basic example which shows you how to use this package. To know more about options to customise editor and update on client, check out the complete documentation of [ShinyEditor](https://www.listendata.com/2021/03/shinyeditor-rich-text-editor-in-shiny.html)

``` r
library(shiny)
library(ShinyEditor)

# UI
ui <- fluidPage(

  # Setup
  use_editor("Enter-API-Key"),
  titlePanel("HTML Generator"),

  # Text Input 1
fluidRow(
  column(
    width = 6,
    editor('textcontent'),
    br(),
    actionButton(
      "generatehtml",
      "Generate HTML Code",
      icon = icon("code"),
      class = "btn-primary"
    )),

  column(
    width = 6,
    tags$pre(textOutput("rawText"))
  )
)

)

# Server
server <- function(input, output, session) {

  # Generate HTML
  observeEvent(input$generatehtml, {

    editorText(session, editorid = 'textcontent', outputid = 'mytext')

    output$rawText <- renderText({
      req(input$mytext)
      enc2utf8(input$mytext)
    })

  })

}

# Run App
shinyApp(ui = ui, server = server)
```

