library(ShinyEditor)

# UI
ui <- fluidPage(

  # Setup
  use_editor(),
  titlePanel("HTML Generator"),

  # Text Input 1
  fluidRow(
    column(
      width = 6,
      editor_ui('textcontent', init_text = "Sample Text"),
      br(),
      actionButton(
        "generatehtml",
        "Generate HTML Code",
        icon = icon("code"),
        class = "btn-primary"
      )),

    column(
      width = 6,
      uiOutput("rawText")
    )
  )

)

# Server
server <- function(input, output, session) {

  # Generate HTML
  content <- reactiveVal()
  editor_server("textcontent", inputId = "mytext", options = list(
    branding = FALSE,
    plugins = c('lists', 'table', 'link', 'image', 'code'),
    toolbar1 = 'bold italic forecolor backcolor | formatselect fontselect fontsizeselect | alignleft aligncenter alignright alignjustify',
    toolbar2 = 'undo redo removeformat bullist numlist table blockquote code superscript  subscript strikethrough link image'
  ))
  output$rawText <- renderUI({
    req(content())
    shiny::HTML(content())
  })
  observeEvent(input$generatehtml, {
    content(input$mytext)
  })

}

# Run App
shinyApp(ui = ui, server = server)
