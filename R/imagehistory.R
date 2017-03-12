library(EBImage)                             #Include EBImage Lib
library(R6)

#' Class providing object describing one action.
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords data
#' @return Object of \code{\link{R6Class}} representing a single ShinyImage action.
#' @format \code{\link{R6Class}} object.
#' @examples
#' crop = c(c(0, 0), c(1200, 1400))
#' siaction$new(0.1, 1, 0, crop)
#' @field brightness Stores address of your lightning server.
#' @field contrast Stores id of your current session on the server.
#' @field gamma Stores url of the last visualization created by this object.
#' @field crop A double nested sequence of crops c\(c\(x1, y1\), c\(x2, y2\)\).
#' #' @section Methods:
#' \describe{
#'   \item{Documentation}{The user should not need to create an action object. This is a class used exclusively by a shinyimg to keep track of a set of changes.}
#'   \item{\code{new(brightness, contrast, gamma, crop)}}{This method is used to create object of this class with the appropriate parameters.}
#'
#'   \item{\code{get_action()}}{This method returns a c()'d list of the input parameters.}
#' }
siaction <- R6Class("siaction",
                    lock_objects = FALSE,
                    public = list(
                      brightness = 0,
                      contrast = 0,
                      gamma = 0,
                      crop = NULL,
                      initialize = function(brightness, contrast, gamma, crop) {
                        self$brightness <- brightness
                        self$contrast <- contrast
                        self$gamma <- gamma
                        self$crop <- crop
                      },
                      get_action = function() {
                        return (c(self$brightness, self$contrast, self$gamma, self$crop))
                      }
                    )
)
#' An EBImage wrapper with integrated history tracking.
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords data
#' @return Object of \code{\link{R6Class}} with manipulation functions.
#' @format \code{\link{R6Class}} object.
#' @examples
#' local_tiger = shinyimg$new\(\"Tigerwater_edit2.jpg\"\)
#' web_tiger = shinyimg$new("https://upload.wikimedia.org/wikipedia/commons/1/1c/Tigerwater_edit2.jpg")
#'
#' local_tiger$add_brightness()
#' local_tiger$undo() # Undoes the brightness addition
#'
#' local_tiger$redo() # Redoes the brightness addition
#' 
#' shiny_tiger = local_tiger$getimg() # Now usable by Shiny
#'
#' local_tiger$add_brightness() # Adds brightness to the image
#' 
#' local_tiger$remove_brightness() # Removes brightness
#' 
#' local_tiger$add_contrast() # Adds contrast
#' 
#' local_tiger$remove_contrast() # Removes Contrast
#' 
#' local_tiger$crop() # Allows the user to select two points to crop to.
#' 
#' local_tiger$save("save.ri") # Saves the current state. The filename is optional.
#' 
#' local_tiger$reload("save.ri") # Loads from a previously saved state. The filename is optional.
#' 
#' #' @section Methods:
#' \describe{
#'   \item{Documentation}{The user should not need to create an action object. This is a class used exclusively by a shinyimg to keep track of a set of changes.}
#'   \item{\code{new(img)}}{Default constructor. \code{img} can be either a URL or a location of a local image.}
#'
#'   \item{\code{undo()}}{Undoes the last change done to this image. When the original image state is reached, no more undos are possible.}
#'   \item{\code{redo()}}{Redos the next action after an undo has been performed. Will no longer redo if there are no more undos to redo.}
#'   \item{\code{getimg()}}{Returns a Shiny compatible image.}
#'   \item{\code{add_brightness()}}{Adds brightness to the image.}
#'   \item{\code{remove_brightness()}}{Removes brightness (darkens) to the image.}
#'   \item{\code{add_contrast()}}{Adds contrast to the image.}
#'   \item{\code{remove_contrast()}}{Removes contrast from the image.}
#'   \item{\code{crop()}}{Uses locator to get corners of an image. Automatically finds min and max coordinates. After two points are selected, a cropping selection can be create in order to crop the image to the desired size.}
#'   \item{\code{save(filepath)}}{Saves the current state to be resumed later. \code{filepath} has a default value of "workspace.si"}
#'   \item{\code{load(filepath)}}{Loads a previously saved state. \code{filepath} has a default value of "workspace.si"}
#'   }
shinyimg <- R6Class("shinyimg",
                    lock_objects = FALSE,
                    public = list(
                      brightness = 0,
                      contrast = 1,
                      actions = 0,
                      xy1 = c(0, 0),
                      xy2 = NULL,
                      img_history = c(),
                      local_img = NULL,
                      current_image = NULL,
                      autosave_filename = "workspace.si",
                      initialize = function(inputImage = NULL, autosave_filename = NULL) {
                        if (!is.null(inputImage)) {
                          self$local_img <- readImage(inputImage)
                          
                          self$current_image <- self$local_img * 1
                          
                          self$xy2 <- c(dim(self$local_img)[1], dim(self$local_img)[2])
                          if (!is.null(autosave_filename))
                            self$autosave_filename <- autosave_filename
                          self$add_action()
                        } else {
                          if (!is.null(autosave_filename)) {
                            self$autosave_filename <- autosave_filename  
                          }
                          self$reload()
                        }
                      },
                      add_action = function(bright = self$brightness, cont = self$contrast, gam = 0, 
                                            crop1x = self$xy1[1],crop1y = self$xy1[2], crop2x = self$xy2[1], 
                                            crop2y = self$xy2[2]) {
                        if (self$actions < length(self$img_history))
                          self$img_history <- self$img_history[1:self$actions]
                        
                        self$img_history <-
                          c(self$img_history, siaction$new(bright, cont, gam, c(c(crop1x,crop1y), c(crop2x, crop2y))))
                        self$actions <- self$actions + 1
                        self$render()
                      },
                      getimg = function() {
                        return (renderPlot({
                          display(self$current_image, method = "raster")
                        }))
                      },
                      applyAction = function(action) {
                     
                        dataframe = action[[1]]
                 
                        args = dataframe$get_action()
              
                        self$current_image <- self$local_img * args[2]
                        self$current_image <- self$local_img + args[1]
                        self$current_image <- self$local_img[args[4]:args[6], args[5]:args[7], ]
                      },
                      
                      add_brightness = function() {
                        self$current_image <- self$current_image + 0.1
                        self$brightness <- self$brightness + 0.1
                        self$add_action()
                      },
                      
                      remove_brightness = function() {
                        self$current_image <- self$current_image - 0.1
                        self$contrast <- self$contrast - 0.1
                        self$add_action()
                      },
                      
                      add_contrast = function() {
                        self$current_image <- self$current_image * 1.1
                        self$contrast <- self$contrast * 1.1
                        self$add_action()
                      },
                      
                      remove_contrast = function() {
                        self$current_image <- self$current_image * 0.9
                        self$contrast <- self$contrast * 0.9
                        self$add_action()
                      },
                      set_brightness = function(brightness) {
                        self$current_image <- self$local_img + brightness
                        self$brightness <- brightness
                        self$add_action()
                      },
                      set_contrast = function(contrast) {
                        self$current_image <- self$local_img * contrast
                        self$contrast <- contrast
                        self$add_action()
                      },
                      crop = function() {
                        print("Select the two opposite corners of a rectangle on the plot.")
                        location = locator(2)
                        x1 = min(location$x[1], location$x[2])
                        y1 = min(location$y[1], location$y[2])
                        x2 = max(location$x[1], location$x[2])
                        y2 = max(location$y[1], location$y[2])
                        self$xy1 = c(x1, y1)
                        self$xy2 = c(x2, y2)
                        self$current_image <<- self$current_image[x1:x2, y1:y2, ]
                        self$add_action()
                      },
                      
                      undo = function() {
                        if (self$actions != 1) {
                          self$actions <- self$actions - 1
                          self$applyAction(self$img_history[self$actions])
                          self$render()
                        } else {
                          print("No action to undo")
                        }
                      },
                      
                      redo = function() {
                        if (self$actions < length(self$img_history)) {
                          self$actions <- self$actions + 1
                          self$applyAction(self$img_history[self$actions])
                          self$render()
                        } else {
                          print("No action to redo")
                        }
                      },
                      
                      # plot output of image
                      render = function() {
                        display(self$current_image, method = "raster")
                      },
                      save = function(file = self$autosave_filename) {
                        action_matrix <- matrix(NA, nrow=length(self$img_history), ncol=7)
                        i = 1
                        
                        for (item in self$img_history) {
                          history <- item$get_action()
                          action_matrix[i, ] <- c(history[1], history[2], history[3], 
                                                  history[4], history[5], 
                                                  history[6], history[7])
                          i = i + 1
                        }
                        actions <- self$actions
                        img <- imageData(self$local_img)
                        base::save(action_matrix, actions, img, file=file)
                      },
                      load = function(file = self$autosave_filename) {
                        base::load(file)
                        self$img_history = c()
                        
                        self$local_img <- Image(img)
                        colorMode(self$local_img) = Color
                        
                        for (i in 1:dim(action_matrix)[1]) {
                          self$add_action(action_matrix[i, 1],
                                     action_matrix[i, 2],
                                     action_matrix[i, 3],
                                     action_matrix[i, 4],
                                     action_matrix[i, 5],
                                     action_matrix[i, 6],
                                     action_matrix[i, 7]
                                     )
                        }
                        self$actions <- actions
                        self$applyAction(self$img_history[self$actions])
                      }
                    )
)

#' A Shiny application that requires a ShinyImg to edit
#' (most likely will be changed in the future)
#' 
#' @param img A ShinyImg to edit using the GUI editor
#' @examples
#' local_tiger = shinyimg$new("Tigerwater_edit2.jpg")
#' start_gui(local_tiger)
#'
start_gui = function(img) {
  if (!require("shiny")) {
    install.packages("shiny")
  }
  
  if (!require("shinydashboard")) {
    install.packages("shinydashboard")
  }
  library("shinydashboard")
  library("shiny")
  
  server <- function(input, output, session) {
    
    current_image <- reactive({
      current_image <- img$current_image
    })
    
    updateSliderInput(session, "brightness", value = img$brightness * 10)
    updateSliderInput(session, "contrast", value = (1 - img$contrast) * 10)
    
    observe({
      cat("Startup succeeded")
    })
    
    observeEvent(input$close, {
      stopApp()
    })
    
    output$plot2 <- renderPlot({
      display(current_image(), method = "raster")
    })
    
    observeEvent(input$brightness, {
      img$set_brightness(input$brightness / 10)
      output$plot2 <- renderPlot({
        display(img$current_image, method = "raster")
      })
    })
    observeEvent(input$contrast, {
      actualContrast = (1 + input$contrast / 10)
      img$set_contrast(actualContrast)
      output$plot2 <- renderPlot({
        display(img$current_image, method = "raster")
      })
    })
    
    output$img <- img$getimg()
  }
  
  ui <- dashboardPage(
    dashboardHeader(title = "ShinyImg GUI"),
    dashboardSidebar(
 
        sliderInput("brightness", "Image Brightness", -10, 10, 0),
        sliderInput("contrast", "Image Contrast", -10, 10, 0)

      
    ),
    dashboardBody(
      # Boxes need to be put in a row (or column)
      fluidRow(
        box(plotOutput("plot2", height = 250))
      )
    )
  )
    #fluidPage(plotOutput("img", click = "plot_click"),
     #             verbatimTextOutput("info"))
  cat("ShinyImg GUI will now start running.\n")
  cat("Please use the close button in the GUI to stop the server.")
  shinyApp(ui = ui, server = server)
}

