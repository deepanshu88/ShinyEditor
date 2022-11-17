#' HTML Editor to be shown in Shiny Apps
#'
#' It shows HTML editor to style text in text area using tinyMCE
#'
#' @param id HTML ID to use for editor
#' @param text Prepopulated Text to be shown in editor.
#' @param options Options to customize editor.
#'
#' @return HTML Editor
#' @export
#' @author Deepanshu Bhalla
#'
#' @examples
#' library(shiny)
#' library(ShinyEditor)
#'
#' # UI
#' ui <- fluidPage(
#'
#'   # Setup
#'   use_editor("API-KEY"),
#'   titlePanel("HTML Generator"),
#'
#'   # Text Input 1
#'   fluidRow(
#'     column(
#'       width = 6,
#'       editor('textcontent'),
#'       br(),
#'       actionButton(
#'         "generatehtml",
#'         "Generate HTML Code",
#'         icon = icon("code"),
#'         class = "btn-primary"
#'       )),
#'
#'     column(
#'       width = 6,
#'       tags$pre(textOutput("rawText"))
#'     )
#'   )
#'
#' )
#'
#' # Server
#' server <- function(input, output, session) {
#'
#'   # Generate HTML
#'   observeEvent(input$generatehtml, {
#'
#'    editorText(session, editorid = 'textcontent', outputid = 'mytext')
#'
#'     output$rawText <- renderText({
#'       req(input$mytext)
#'       enc2utf8(input$mytext)
#'     })
#'
#'   })
#'
#' }
#'
#' # Run App
#' shinyApp(ui = ui, server = server)

editor <- function(id, text = NULL, options = NULL) {
  shiny::tagList(
    shiny::tags$form(method="post", shiny::tags$textarea(id=id, text)),
    shiny::tags$script(paste0("tinymce.init({ selector: '#", id, "' ,", options, "});"))
  )
}


#' Update HTML Editor on the client
#'
#' It allows to update HTML Editor in server side.
#'
#' @param id HTML ID to use for editor
#' @param text Prepopulated Text to be shown in editor.
#'
#' @return HTML Editor
#' @export
#' @author Deepanshu Bhalla
#'
#' @examples
#' library(shiny)
#' library(ShinyEditor)
#'
#' ui <- fluidPage(
#'
#'   # Setup
#'   use_editor("API-KEY"),
#'   titlePanel("HTML Generator"),
#'
#'   # Text Input 1
#'   fluidRow(
#'     column(
#'       width = 6,
#'       editor('textcontent'),
#'       br(),
#'       actionButton(
#'         "generatehtml",
#'         "Generate HTML Code",
#'         icon = icon("code"),
#'         class = "btn-primary"
#'       ), actionButton("updatedata", "Update Editor", icon = icon("edit"))),
#'
#'     column(
#'       width = 6,
#'       tags$pre(textOutput("rawText"))
#'     )
#'   )
#'
#' )
#'
#' # Server
#' server <- function(input, output, session) {
#'
#'   # Generate HTML
#'   observeEvent(input$generatehtml, {
#'
#'    editorText(session, editorid = 'textcontent', outputid = 'mytext')
#'
#'     output$rawText <- renderText({
#'       req(input$mytext)
#'       enc2utf8(input$mytext)
#'     })
#'
#'   })
#'
#'   observeEvent(input$updatedata, {
#'     UpdateEditor(session,
#'                  id = "textcontent",
#'                  text = "<b>Sample Text</b>")
#'
#'   })
#'
#' }
#'
#' # Run App
#' shinyApp(ui = ui, server = server)


UpdateEditor <- function(session,
                         id = "textcontent",
                         text = "Sample Text") {

  session$sendCustomMessage(type = "UpdateshinyEditor",
                            list(id = id,
                                 content = text
                            ))

}

#' Extract HTML generated
#'
#' It extracts HTML generated based on text written in editor.
#'
#' @param session session object passed to function given to shinyServer
#' @param editorid ID passed in editor function in UI
#' @param outputid output ID to store HTML
#'
#' @return HTML
#' @export
#' @author Deepanshu Bhalla
#'
#' @examples
#' library(shiny)
#' library(ShinyEditor)
#'
#' # UI
#' ui <- fluidPage(
#'
#'   # Setup
#'   use_editor("API-KEY"),
#'   titlePanel("HTML Generator"),
#'
#'   # Text Input 1
#'   fluidRow(
#'     column(
#'       width = 6,
#'       editor('textcontent'),
#'       br(),
#'       actionButton(
#'         "generatehtml",
#'         "Generate HTML Code",
#'         icon = icon("code"),
#'         class = "btn-primary"
#'       )),
#'
#'     column(
#'       width = 6,
#'       tags$pre(textOutput("rawText"))
#'     )
#'   )
#'
#' )
#'
#' # Server
#' server <- function(input, output, session) {
#'
#'   # Generate HTML
#'   observeEvent(input$generatehtml, {
#'
#'    editorText(session, editorid = 'textcontent', outputid = 'mytext')
#'
#'     output$rawText <- renderText({
#'       req(input$mytext)
#'       enc2utf8(input$mytext)
#'     })
#'
#'   })
#'
#' }
#'
#' # Run App
#' shinyApp(ui = ui, server = server)

editorText <- function (session, editorid, outputid) {
  session$sendCustomMessage(type = "HTMLText", list(jscode = paste0("Shiny.setInputValue(", "'", outputid, "', tinyMCE.get(", "'", editorid, "'",
                                                                    ").getContent(), {priority: 'event'});")))
}
