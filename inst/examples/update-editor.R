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
      editor('textcontent'),
      br(),
      actionButton(
        "generatehtml",
        "Generate HTML Code",
        icon = icon("code"),
        class = "btn-primary"
      ), actionButton("updatedata", "Update Editor", icon = icon("edit"))),

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

  observeEvent(input$updatedata, {
    UpdateEditor(session,
                 id = "textcontent",
                 text = "<b>Sample Text</b>")

  })

}

# Run App
shinyApp(ui = ui, server = server)
