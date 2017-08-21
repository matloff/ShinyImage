#' runShiny function to start shiny app
#' 
#' @description A function to start shiny app
#' @name runShiny-function
#' @export
#' @import shiny 
#' @importFrom EBImage display imageData Image colorMode readImage 
#' writeImage gblur combine channel rotate
#' @importFrom shinyjs hidden hide show
runShiny <- function() {
  appDir <- system.file("shinyapp", "app19.R", package = "ShinyImage")
  if (appDir == "") {
    stop("Could not find Shiny directory. Try re-installing `ShinyImage`.", call. = FALSE)
  }
  
  shiny::runApp(appDir, display.mode = "normal")
}