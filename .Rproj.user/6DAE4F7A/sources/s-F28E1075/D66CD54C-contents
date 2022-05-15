#' Loads libraries for this package
#'
#' Set up for the package. Required in UI
#'
#' @return JS Files
#' @export

use_editor <- function(API) {

  tagList(shiny::singleton(
    shiny::tags$head(
      shiny::tags$script(
        src = paste0(
          "https://cdn.tiny.cloud/1/", API, "/tinymce/5/tinymce.min.js"
        ),
        referrerpolicy = "origin"
      ),
      shiny::tags$script(src = "ShinyEditor-assets/ShinyEditor.js"),
      shiny::tags$script(
        "Shiny.addCustomMessageHandler('HTMLText', function(data) {
       eval(data.jscode)
      });"
      )
    )
  ))

}
