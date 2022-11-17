library(shiny)
library(ShinyEditor)

# UI
ui <- fluidPage(

  # Setup
  use_editor("API-Key"),
  titlePanel("HTML Generator"),

  # Text Input 1
  fluidRow(
    column(
      width = 6,
      editor('textcontent', text = "Sample Text",
             options = "branding: false,
             height: 300,
             plugins: ['lists', 'table', 'link', 'image', 'code'],
             toolbar1: 'bold italic forecolor backcolor | formatselect fontselect fontsizeselect | alignleft aligncenter alignright alignjustify',
             toolbar2: 'undo redo removeformat bullist numlist table blockquote code superscript  subscript strikethrough link image'"),
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
