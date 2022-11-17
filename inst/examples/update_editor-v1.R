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
       editor_ui('textcontent'),
       br(),
       actionButton(
         "generatehtml",
         "Generate HTML Code",
         icon = icon("code"),
         class = "btn-primary"
       ), actionButton("updatedata", "Update Editor", icon = icon("edit"))),

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
   editor_server("textcontent", inputId = "mytext")
   output$rawText <- renderUI({
     req(content())
     shiny::HTML(content())
   })
   observeEvent(input$generatehtml, {
     content(input$mytext)
   })

   observeEvent(input$updatedata, {
    update_editor(id = "textcontent",
                  text = "<b>Sample Text</b>")

  })

 }

 # Run App
 shinyApp(ui = ui, server = server)
