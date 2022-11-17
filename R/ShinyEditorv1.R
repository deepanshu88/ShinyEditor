#' HTML Editor to be shown in Shiny Apps
#'
#' It shows HTML editor to style text in text area using tinyMCE
#'
#' @param id HTML ID to use for editor
#' @param init_text Prepopulated Text to be shown in editor.
#'
#' @return \code{shiny.tag}˝
#' @export
#' @author Deepanshu Bhalla & Stephen Holsenbeck
#'
#' @examples
#' library(shiny)
#' library(ShinyEditor)
#'
#' # UI
#' ui <- fluidPage(# Setup
#'   use_editor(),
#'   titlePanel("HTML Generator"),
#'
#'   # Text Input 1
#'   fluidRow(
#'     column(
#'       width = 6,
#'       editor_ui('textcontent'),
#'       br(),
#'       actionButton(
#'         "generatehtml",
#'         "Generate HTML Code",
#'         icon = icon("code"),
#'         class = "btn-primary"
#'       )
#'     ),
#'
#'     column(width = 6,
#'            uiOutput("rawText"))
#'   ))
#'
#' # Server
#' server <- function(input, output, session) {
#'   # Generate HTML
#'   content <- reactiveVal()
#'   editor_server("textcontent", inputId = "mytext")
#'   output$rawText <- renderUI({
#'     req(content())
#'     shiny::HTML(content())
#'   })
#'   observeEvent(input$generatehtml, {
#'     content(input$mytext)
#'   })
#'
#' }
#'
#' # Run App
#' shinyApp(ui = ui, server = server)

editor_ui <- function(inputId, init_text = NULL) {
  shiny::tagList(shiny::tags$form(method = "post", shiny::tags$textarea(id = inputId, init_text)))
}
#' An environment for storing TinyMCE Selectors
#' @description An environment to store selectors that are already instantiated to prevent re-init (and thus breaking) already rendered elements
se_env <- new.env()

#' An object for storing TinyMCE selectors
#' @description A vector of already initialized selectors
#' @name se_env$init_ids
se_env$init_ids <- c()


#' The server module for the TinyMCE editor
#'
#' @param selector \code{chr} The inputId used for `editor_ui`. This is namespaced and `#` is prepended unless `asis = TRUE` in which case this is taken verbatim as the selector.
#' @inheritParams shinyjs::addClass
#' @param options \code{list} of options to pass to the `TinyMCE.init` method. See the [TinyMCE Configuration options reference](https://www.tiny.cloud/docs/configure/)
#' @param inputId \code{chr} The inputId to save the content of the TinyMCE editor to. This is namespaced unless `asis = TRUE` in which case this is taken verbatim as the inputId.
#' @inheritParams shiny::onSessionEnded
#'
#' @export
#'
#' @inherit editor_ui examples
editor_server <- function(selector, asis = FALSE, options = NULL, inputId, session = shiny::getDefaultReactiveDomain()) {
  if (asis) {
    .selector <- selector
    .inputId <- inputId
  } else {
    .selector <- paste0("#", session$ns(selector))
    .inputId <- session$ns(inputId)
  }
  if (!missing(inputId)) {
    out <- editor_text(selector, inputId = inputId, as_fn = TRUE)
    .setup <- paste0(
      "
                    editor.on('keyup', function(e) {
                      Shiny.setInputValue('",.inputId,"', this.getContent(), {priority: 'event'})
                    });
                    editor.on('change', function(e) {
                      Shiny.setInputValue('",.inputId,"', this.getContent(), {priority: 'event'})
                    })
      "
    )

    if (!is.null(options$setup)) {
      UU::gwarn("You have included a {.code setup} option. This will overwrite logic intended to write the input object.\n
                consider adding the following to your setup callbac function: {.pre { .setup}}")


    } else {
      options$setup =  ShinyEditor::json_verbatim(paste0(
        "function(editor) {
        ",.setup,"
                    }"
      ))
    }

  } else
    out <- NULL

  init <-
    append(list(selector = .selector), options)

  init_chr <- jsonlite::toJSON(init, auto_unbox = TRUE, pretty = TRUE, json_verbatim = TRUE)

  shiny::isolate({
    if (!selector %in% se_env$init_ids) {
      #TODO Add an event listener that watches for compositionend and calls the custom code to save the input constantly
      shinyjs::runjs(paste0("tinymce.init(",init_chr,");"))
      assign("init_ids", c(se_env$init_ids, init$selector), envir = se_env)
    }
  })


}
#' Update HTML Editor on the client
#'
#' It allows to update HTML Editor in server side.
#'
#' @inheritParams editor_server
#' @param text Prepopulated Text to be shown in editor.
#'
#' @return \code{NULL} Updates the text in the TinyMCE Editor
#' @export
#'
#' @examples
#' library(shiny)
#' library(ShinyEditor)
#' # UI
#' ui <- fluidPage(
#'
#'   # Setup
#'   use_editor(),
#'   titlePanel("HTML Generator"),
#'
#'   # Text Input 1
#'   fluidRow(
#'     column(
#'       width = 6,
#'       editor_ui('textcontent'),
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
#'       uiOutput("rawText")
#'     )
#'   )
#'
#' )
#'
#' # Server
#' server <- function(input, output, session) {
#'
#'   # Generate HTML
#'   content <- reactiveVal()
#'   editor_server("textcontent", inputId = "mytext")
#'   output$rawText <- renderUI({
#'     req(content())
#'     shiny::HTML(content())
#'   })
#'   observeEvent(input$generatehtml, {
#'     content(input$mytext)
#'   })
#'
#'   observeEvent(input$updatedata, {
#'     update_editor(selector = "textcontent",
#'                   text = "<b>Sample Text</b>")
#'
#'   })
#'
#' }
#'
#' # Run App
#' shinyApp(ui = ui, server = server)



update_editor <- function(selector,
                         text = "Sample Text",
                         session = shiny::getDefaultReactiveDomain()) {

  session$sendCustomMessage(type = "UpdateshinyEditor",
                            list(id = id,
                                 content = text
                            ))

}

#' Extract HTML generated
#'
#' It extracts HTML generated based on text written in editor.
#'
#' @inheritParams editor_server
#'
#' @return \code{NULL/fun} If `as_fn = FALSE` *Default* the `input[[inputId]]` is updated with the generated HTML.
#'
#' @export
#'
#' @examples
#' devtools::load_all()
#'
#' # UI
#' ui <- fluidPage(# Setup
#'   use_editor(Sys.getenv("TINYMCE_KEY")),
#'   titlePanel("HTML Generator"),
#'
#'   # Text Input 1
#'   fluidRow(
#'     column(
#'       width = 6,
#'       editor_ui('textcontent'),
#'       br(),
#'       actionButton(
#'         "generatehtml",
#'         "Generate HTML Code",
#'         icon = icon("code"),
#'         class = "btn-primary"
#'       )
#'     ),
#'
#'     column(width = 6,
#'            uiOutput("rawText"))
#'   ))
#'
#' # Server
#' server <- function(input, output, session) {
#'   # Generate HTML
#'   content <- reactiveVal()
#'   editor_server("textcontent")
#'
#'   observeEvent(input$generatehtml, {
#'     editor_text("textcontent", inputId = "mytext")
#'     output$rawText <- renderUI({
#'       req(input$mytext)
#'       shiny::HTML(input$mytext)
#'     })
#'   })
#'
#' }
#'
#' # Run App
#' shinyApp(ui = ui, server = server)


editor_text <- function (selector, asis = FALSE, inputId, as_fn = FALSE, session = shiny::getDefaultReactiveDomain()) {
  browser()
  .jscode <- paste0(
    "debugger;
    var editor = tinyMCE.get(",
    "'",
    ifelse(asis, selector, session$ns(selector)),
    "'",
    ");
    if (typeof editor != null) {
      Shiny.setInputValue(",
    "'",
    session$ns(inputId),
    "', editor.getContent(), {priority: 'event'});
    }
    "
  )
  ex <- rlang::expr({
    session$sendCustomMessage(type = "HTMLText", list(
      jscode = !!.jscode
    ))
  })

  if (as_fn)
    rlang::new_function(args = rlang::pairlist2(), body = ex)
  else
    eval(ex)
}
