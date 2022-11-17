#' Marks input as verbatim JSON.
#' @description Useful when supplying a JS function as an option
#' @param x \code{chr} String to be interpreted as literal JSON
#'
#' @return \code{json, chr} The string as class `json`
#' @export
#'
#' @examples
#' jsonlite::toJSON(
#'   list(
#'     a = 1:3,
#'     b = "character",
#'     callback = json_verbatim("function(e) { //do some thing }")
#'   ),
#'   auto_unbox = TRUE,
#'   pretty = TRUE,
#'   json_verbatim = TRUE
#' )
json_verbatim <- function(x) {
  class(x) <- "json"
  x
}

#' Write the TINYMCE_KEY variable to _.Renviron_
#' @description Click [here](https://www.tiny.cloud/my-account/dashboard/) to view the dashboard or make an account
#' @param key \code{chr} The TinyMCE API Key
#' @inheritParams UU::creds_to_renviron
#'
#' @return \code{msg} informing the user that the key was written
#' @export
#'
#' @examples
#' tinymce_key("a 48 chr hex string api key")
tinymce_key <- function(key, scope = c("user", "project")[1]) {
  UU::creds_to_renviron(TINYMCE_KEY = key, scope = scope, overwrite = TRUE)
}
