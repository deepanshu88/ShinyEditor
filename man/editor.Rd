% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ShinyEditor.R
\name{editor}
\alias{editor}
\title{HTML Editor to be shown in Shiny Apps}
\usage{
editor(id, text = NULL, options = NULL)
}
\arguments{
\item{id}{HTML ID to use for editor}

\item{text}{Prepopulated Text to be shown in editor.}

\item{options}{Options to customize editor.}
}
\value{
HTML Editor
}
\description{
It shows HTML editor to style text in text area using tinyMCE
}
\examples{
library(shiny)
library(ShinyEditor)

# UI
ui <- fluidPage(

  # Setup
  use_editor("API-KEY"),
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
}
\author{
Deepanshu Bhalla
}
