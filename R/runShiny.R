#' runShiny function to start shiny app
#' 
#' @description A function to start shiny app
#' @name runShiny-function
#' @export
#' @import shiny 
#' @importFrom EBImage display imageData Image colorMode readImage 
#' writeImage gblur combine channel rotate
#' @importFrom shinyjs hidden hide show

# function to run the Shiny app
# optional parameter to give the current shinyimg e.g. tiger to the shiny app to work on

runShiny <- function(current_shinyimg) {
  if(missing(current_shinyimg))
  {
    #do nothing
  }
  else
  {
    #create a global variable for the current shinyimg 
    current <<- current_shinyimg
  }
  
  appDir <- system.file("shinyapp", "app19.R", package = "ShinyImage")
  if (appDir == "") {
    stop("Could not find Shiny directory. Try re-installing `ShinyImage`.", call. = FALSE)
  }
  
  shiny::runApp(appDir, display.mode = "normal")
  
}