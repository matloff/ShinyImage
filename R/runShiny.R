#' runShiny function to start shiny app
#' 
#' @description A function to start shiny app
#' @name runShiny-function
#' @export
#' @param current_shinyimg takes in an optional parameter of a image user is 
#'    currently editing in the command line
#' @import shiny 
#' @importFrom EBImage display imageData Image colorMode readImage 
#' writeImage gblur combine channel rotate
#' @importFrom shinyjs hidden hide show
#' @examples
#' \dontrun{
#'    runShiny()
#' }

# function to run the Shiny app
# optional parameter to give the current shinyimg e.g. tiger to the shiny app to work on

runShiny <- function(current_shinyimg) {
  #intiial declaration of current
  current <- NULL

  if(missing(current_shinyimg))
  {
    #global declaration of current so 
    #shiny app can access it
    current <<- NULL
  }
  else
  {
    #assign current_shinyimg to current
    #current is used in the shinyapp if current exists
    current <<- current_shinyimg
  }
  
  appDir <- system.file("shinyapp", "app19.R", package = "ShinyImage")
  if (appDir == "") {
    stop("Could not find Shiny directory. Try re-installing `ShinyImage`.", call. = FALSE)
  }
  
  shiny::runApp(appDir, display.mode = "normal")
  
}