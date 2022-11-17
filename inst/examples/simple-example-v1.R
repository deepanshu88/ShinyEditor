library(ShinyEditor)
# UI
ui <- fluidPage(# Setup
  use_editor(Sys.getenv("TINYMCE_KEY")),
  titlePanel("HTML Generator"),

  # Text Input 1
  fluidRow(
    column(
      width = 6,
      editor_ui('textcontent'),
      br(),
      actionButton(
        "generatehtml",
        "Generate HTML Code",
        icon = icon("code"),
        class = "btn-primary"
      )
    ),

    column(width = 6,
           uiOutput("rawText"))
  ))

# Server
server <- function(input, output, session) {
  # Generate HTML
  content <- reactiveVal()
  editor_server("textcontent", inputId = "mytext")
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
