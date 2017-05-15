library(EBImage)                             #Include EBImage Lib
library(R6)

#' Class providing object describing one action.
#'
#' @docType class
#' @importFrom R6 R6Class
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
#'   \item{\code{get_action()}}{This method returns a c() list of the input parameters.}
#'   
siaction <- R6Class("siaction",
                    # Make this action mutable. TODO: Make it so that
                    # it doesn't need to be
                    lock_objects = FALSE,
                    public = list(
                      # Properties of this action
                      brightness = 0,
                      contrast = 0,
                      gamma = 0,
                      crop = NULL,
                      # Initialize all the values of this action
                      initialize = function(brightness, contrast, gamma, crop) {
                        self$brightness <- brightness
                        self$contrast <- contrast
                        self$gamma <- gamma
                        self$crop <- crop
                      },
                      # Get the c()'d properties of this particular action
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
#' 
#' 
#' local_tiger = shinyimg$new('Tigerwater_edit2.jpg')
#' web_tiger = shinyimg$new('https://upload.wikimedia.org/wikipedia/commons/1/1c/Tigerwater_edit2.jpg')
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
#' local_tiger$save('save.ri') # Saves the current state. The filename is optional.
#' 
#' local_tiger$load('save.ri') # Loads from a previously saved state. The filename is optional. Requires a previously instantiated shinyimg instance (argument provided to new can be null).
#' 
#' @section Methods:
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
#'   \item{\code{save(filepath)}}{Saves the current state to be resumed later. \code{filepath} has a default value of 'workspace.si'}
#'   \item{\code{load(filepath)}}{Loads a previously saved state. \code{filepath} has a default value of 'workspace.si'}
#'   \item{\code{dim()}}{Returns the current image dimentions.}
#'   \item{\code{render()}}{Renders the current image.}
#'   \item{\code{toggle_render()}}{Toggles the automatic rendering after making a change. By default, this option is off.}
#'   }
#'   
shinyimg <- R6Class("shinyimg",
                    lock_objects = FALSE,
                    public = list(
                      # Default Brightness
                      brightness = 0,
                      # Default Contrast
                      contrast = 1,
                      # CURRENT Number of actions. Can be less than the
                      # Actual number of actions due to undos.
                      actions = 0,
                      # Crop coordinates
                      xy1 = c(0, 0),
                      xy2 = NULL,
                      # Crop offsets (relative to top left, which is 0, 0)
                      xoffset = 0,
                      yoffset = 0,
                      # List of image histories
                      img_history = c(),
                      # Variable to store the source image
                      local_img = NULL,
                      # Variable to store the current image to display
                      current_image = NULL,
                      # Option to have the output automatically rendered
                      autodisplay = 0,
                      # The filename used for the autosave in case of crashes
                      autosave_filename = "workspace.si",
                      # Startup function. Can take a image, and/or the autosave
                      # filename.
                      initialize = function(inputImage = NULL, 
                                            autosave_filename = NULL) {
                        
                        # Set the autosave filename if it is not null
                        if (!is.null(autosave_filename))
                          self$autosave_filename <- autosave_filename
                        
                        if (!is.null(inputImage)) {
                          # Here the user passed in an argument for inputImage
                          # We use the readImage functionality to read in the
                          # image to form an EBImage. This may be changed at 
                          # a later time. 
                          self$local_img <- readImage(inputImage)
                          # Here we set the current image to the original 
                          # image. Multiplying by one essentially copies the 
                          # image. The reason this works is that the
                          # multiplication function when applied to an image 
                          # changes contrast. In this case, 1 is the default 
                          # contrast, and thus we are essentially making 
                          # a copy by not changing the contrast but telling 
                          # it to make another image. 
                          self$current_image <- self$local_img * 1
                          
                          # Here we set the xy2 coordinate, which is the lower 
                          # right coordinate of the image. 
                          self$xy2 <- c(dim(self$local_img)[1], 
                                        dim(self$local_img)[2])
                          
                          # Add the "base" action, which is the original image. 
                          self$add_action()
                        } else {
                          # TODO: Maybe some sort of error message?
                          result = tryCatch({
                            self$load()
                          }, warning = function(w) {
                            
                          }, error = function(e) {
                            
                          }, finally = {
                            
                          });
                        }
                      },
                      # The main workhorse function for scribing an action.
                      # crop parameters refer to the top left x, top left y,
                      # bottom right x, bottom right y respectively. 
                      add_action = function(bright = self$brightness, 
                                            cont = self$contrast, 
                                            gam = 0, 
                                            crop1x = self$xy1[1],
                                            crop1y = self$xy1[2], 
                                            crop2x = self$xy2[1], 
                                            crop2y = self$xy2[2]) {
                        
                        # If we are not at the most recent image, we need to 
                        # prune the extra actions. 
                        if (self$actions < length(self$img_history))
                          self$img_history <- self$img_history[1:self$actions]
                        
                        # Use the siaction constructor to create a new action 
                        # and add it to the img_history list.
                        self$img_history <-
                          c(self$img_history, siaction$new(bright, 
                                                           cont, 
                                                           gam, 
                                                           c(
                                                             c(crop1x,crop1y), 
                                                             c(crop2x, crop2y)
                                                           )))
                        # Add one to the action counter because we just added 
                        # an action to the action list
                        self$actions <- self$actions + 1
                        
                        # If the autodisplay flag is on, render the changes.
                        if (self$autodisplay) {
                          self$render()
                        }
                      },
                      # Rendering function used to get a plot that can be
                      # used by Shiny
                      getimg = function() {
                        return (renderPlot({
                          display(self$current_image, method = "raster")
                        }))
                      },
                      # This function essentially generates a modified image
                      # from the original (local_img)
                      applyAction = function(action) {
                        # Unpack the action variable
                        dataframe = action[[1]]
                        # Use the action's getter to return the c()'d args
                        args = dataframe$get_action()
                        
                        # args[2] is contrast, but we use local_img as
                        # the source image.
                        self$current_image <- self$local_img * args[2]
                        # args[1] is brightness
                        self$current_image <- self$current_image + args[1]
                        
                        # args[4] through args[7] are ABSOLUTE crop locations.
                        self$current_image <- self$current_image[
                          args[4]:args[6], args[5]:args[7], 
                          ]
                      },
                      # Adjusts brightness by 0.1. This is a good increment
                      # but a variable brightness function should be added.
                      add_brightness = function() {
                        self$current_image <- self$current_image + 0.1
                        self$brightness <- self$brightness + 0.1
                        self$add_action()
                      },
                      
                      # Adjusts brightness by -0.1. This is a good decrement
                      # but a variable brightness function should be added.
                      remove_brightness = function() {
                        self$current_image <- self$current_image - 0.1
                        self$brightness <- self$brightness - 0.1
                        self$add_action()
                      },
                      
                      # Adjusts contrast by 0.1. This is a good increment
                      # but a variable contrast function should be added.
                      add_contrast = function() {
                        self$current_image <- self$current_image * 1.1
                        self$contrast <- self$contrast * 1.1
                        self$add_action()
                      },
                      
                      # Adjusts contrast by -0.1. This is a good increment
                      # but a variable contrast function should be added.
                      remove_contrast = function() {
                        self$current_image <- self$current_image * 0.9
                        self$contrast <- self$contrast * 0.9
                        self$add_action()
                      },
                      
                      # Adjusts brightness by the argument. Mainly used
                      # by the Shiny app.
                      # TODO: Document the usage of this function.
                      set_brightness = function(brightness) {
                        self$current_image <- self$local_img + brightness
                        self$brightness <- brightness
                        self$add_action()
                      },
                      
                      # Adjusts contrast by the argument. Mainly used
                      # by the Shiny app.
                      # TODO: Document the usage of this function.
                      set_contrast = function(contrast) {
                        self$current_image <- self$local_img * contrast
                        self$contrast <- contrast
                        self$add_action()
                      },
                      # The command line cropper uses locator to have the
                      # user locate the two corners of the subimage. 
                      crop = function() {
                        print("Select the two opposite corners 
                              of a rectangle on the plot.")
                        location = locator(2)
                        x1 = min(location$x[1], location$x[2])
                        y1 = min(location$y[1], location$y[2])
                        x2 = max(location$x[1], location$x[2])
                        y2 = max(location$y[1], location$y[2])
                        self$current_image <<- self$current_image[x1:x2, 
                                                                  y1:y2,]
                        
                        # In order to maintain a correct cropping, we need to
                        # know how much of
                        # the original image has already been cropped.
                        xdiff = x2 - x1
                        ydiff = y2 - y1
                        
                        # The offset is needed to maintain the ABSOLUTE crop
                        # data.
                        self$xoffset = self$xoffset + x1
                        self$yoffset = self$yoffset + y1
                        
                        # Create the absolute crop data using the offsets and
                        # new area.
                        self$xy1 = c(self$xoffset, self$yoffset)
                        self$xy2 = c(self$xoffset + xdiff, 
                                     self$yoffset + ydiff)
                        self$add_action()
                      },
                      
                      # The function used by Shiny to crop using absolute 
                      # coordinates. 
                      cropxy = function(x1, x2, y1, y2) {
                        # TODO: Temporary workaround to prevent an image with a
                        # Height or a width from being created. Should be 
                        # removed when the image return method is fixed below. 
                        if (abs(x1-x2) == 0 || abs(y1-y2) == 0)
                          return
                        
                        # TODO: Make sure the ABSOLUTE coordinates are not 
                        self$xy1 = c(min(x1, x2), min(y1, y2))
                        self$xy2 = c(max(x1,x2), max(y1,y2))
                        # We use the add action in order to apply that action.
                        self$add_action()
                        # TODO: Temporary workaround to return the correct 
                        # image. Is not efficient because it generates the 
                        # image from scratch every time. 
                        self$applyAction(self$img_history[self$actions])
                      },
                      # Uses the actions list (img_history) to undo the last
                      # done action. DOES NOT PRUNE THE LIST AT THIS POINT. 
                      undo = function() {
                        # If there are more actions to undo besides the 
                        # original
                        # image (aka action #1)
                        if (self$actions != 1) {
                          # Step back by one action
                          self$actions <- self$actions - 1
                          
                          # Apply the action.
                          self$applyAction(self$img_history[self$actions])
                          
                          # TODO: IDEA. Lazy loading. Don't actually apply the
                          # action UNTIL we're done undoing.
                          
                          # TODO: See if this autodisplay should be applied to
                          # the applyAction function instead.
                          if (self$autodisplay) {
                            self$render()
                          }
                        } else {
                          # There are no actions to undo.
                          print("No action to undo")
                        }
                      },
                      # Uses the actions list (img_history) to redo the last
                      # undone action.
                      redo = function() {
                        # If there are actions to redo
                        if (self$actions < length(self$img_history)) {
                          # Increment by one action, then apply it
                          self$actions <- self$actions + 1
                          self$applyAction(self$img_history[self$actions])
                          
                          # TODO: IDEA. Lazy loading. Don't actually apply the
                          # action UNTIL we're done redoing.
                          
                          # TODO: See if this autodisplay should be applied to
                          # the applyAction function instead.
                          if (self$autodisplay) {
                            self$render()
                          }
                        } else {
                          # No actions to redo.
                          print("No action to redo")
                        }
                      },
                      
                      # plot output of image
                      render = function() {
                        if (!is.null(self$current_image)) {
                          display(self$current_image, method = "raster")
                        }
                      },
                      # Function to write the current state of the program to 
                      # file.
                      save = function(file = self$autosave_filename) {
                        # Generated action matrix done in O(1) time.
                        action_matrix <- matrix(NA, 
                                                nrow=length(self$img_history), 
                                                ncol=7)
                        # Fill in the history data
                        i = 1
                        for (item in self$img_history) {
                          history <- item$get_action()
                          # TODO: Map function perhaps?
                          action_matrix[i, ] <- c(history[1], history[2], 
                                                  history[3], history[4], 
                                                  history[5], history[6], 
                                                  history[7])
                          i = i + 1
                        }
                        # Save the current action number
                        actions <- self$actions
                        # Save the current image as well
                        img <- imageData(self$local_img)
                        # Save everything to file.
                        base::save(action_matrix, actions, img, file=file)
                      },
                      # Counterpart to the save function, will load from
                      # previous save file.
                      load = function(file = self$autosave_filename) {
                        base::load(file)
                        # Generate the image history.
                        self$img_history = c()
                        
                        self$local_img <- Image(img)
                        
                        # Not sure if this fixes the issue -- Had
                        # some weird color issues like loading in
                        # black and white. This seems to have fixed the
                        # issue.
                        colorMode(self$local_img) = Color
                        
                        # FIll in the action matrix
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
                        
                        # Apply the latest action
                        self$applyAction(self$img_history[self$actions])
                        
                        # TODO: See if this should go in applyAction instead.
                        if (self$autodisplay) {
                          self$render()
                        }
                      }, 
                      
                      # Returns the size of the current image.
                      # Needed for Shiny to determine the max values of
                      # the sliders. 
                      size = function() {
                        dim(self$current_image)
                      },
                      # Function that handles the automatic render toggle.
                      # Default autodisplay is off. 
                      toggle_render = function() {
                        self$autodisplay = 1 - self$autodisplay;
                        if (self$autodisplay) {
                          self$render()
                        }
                      }
                    )
)

#' Wrapper to load an image from a cold boot. 
#' 
#' @param filename The filename of a file previously generated by shinyimg's $save function. 
#' @examples
#' local_tiger = shinyimg$new("Tigerwater_edit2.jpg")
#' local_tiger$save("tiger.si")
#' # Restart R 
#' reloaded_tiger = shinyload("tiger.si")
#' @export
shinyload = function(filename) {
  shinyimg$new(NULL, filename)
}

#' The ShinyImg GUI editor
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords data
#' @return Object of \code{\link{R6Class}} with manipulation functions.
#' @format \code{\link{R6Class}} object.
#' @examples
#' local_tiger = shinyimg$new("Tigerwater_edit2.jpg")
#' editor_instance = shinygui$new()
#' editor_instance$load(tiger)
#' # The original image can also be provided:
#' editor_instance$load("Tigerwater_edit2.jpg")
#' #' @section Methods:
#' \describe{
#'   \item{Documentation}{The user should create an instance to facilitate the gui editing of an image.}
#'   \item{\code{new()}}{Default constructor. No arguments required.}
#'
#'   \item{\code{load()}}{Loads a ShinyImage or raw image in order to process.}
#'   }
#'
shinygui <- R6Class("shinygui",
                    # TODO: Figure out if the ShinyGui part needs to be mutable.
                    lock_objects = FALSE,
                    public = list(
                      initialize = function() {
                        # TODO: See if there are any variables that can be initialized here.
                        # I.e., Settings for the ShinyGui.
                        # TODO: Render button? 
                      },
                      load = function(img) {
                        
                        # If the argument from load is NOT a shinyimg, then
                        # create the shinyimg object using the image as the
                        # source image. 
                        if (class(img)[1] != "shinyimg") {
                          img = shinyimg$new(img)
                        }
                        
                        
                        if (!require("shiny")) {
                          cat("shiny is not installed. Please install it first.")
                          return
                        }
                        
                        if (!require("shinydashboard")) {
                          cat("shinydashboard is not installed. Please install it first.")
                          return
                        }
                        
                        library("shinydashboard")
                        library("shiny")
                        
                        server <- function(input, output, session) {
                          
                          # TODO: Figure out how to make image more "reactive"
                          # as it does not work for certain circumstances/ does 
                          # not work intermittently. 
                          current_image <- reactive({
                            current_image <- img$current_image
                          })
                          
                          # Set the intial slider values. This takes ~1s to startup.
                          # TODO: Optimize (somehow).
                          updateSliderInput(session, "brightness", value = img$brightness * 10)
                          updateSliderInput(session, "contrast", value = (1 - img$contrast) * 10)
                          
                          # TODO: Use this startup segment to try and optimize some of the 
                          # loading times by offloading some of the additional processing until
                          # AFTER the app starts up. 
                          observe({
                            # cat("Startup complete.")
                          })
                          
                          # If the browser window becomes closed, stop the app as well. 
                          observeEvent(input$close, {
                            stopApp()
                          })
                          
                          # The image plot displayed by Shiny uses "plot2" as the rendered
                          # image.
                          output$plot2 <- renderPlot({
                            display(current_image(), method = "raster")
                          })
                          
                          # Sets the brightness based on the slider value. 
                          # TODO: The output image rendering does not get updated unless
                          # explicitly called (using a function with the renderplot in
                          # it will not actually update the image).
                          observeEvent(input$brightness, {
                            img$set_brightness(input$brightness / 10)
                            output$plot2 <- renderPlot({
                              display(img$current_image, method = "raster")
                            })
                          })
                          # Sets the contrast based on the slider value. 
                          # TODO: The output image rendering does not get updated unless
                          # explicitly called (using a function with the renderplot in
                          # it will not actually update the image).
                          observeEvent(input$contrast, {
                            actualContrast = (1 + input$contrast / 10)
                            img$set_contrast(actualContrast)
                            output$plot2 <- renderPlot({
                              display(img$current_image, method = "raster")
                            })
                          })
                          
                          # Updates the x1 (top left) slider based on error checking
                          observeEvent(input$x1, {
                            # If the x1 coord is greater or equal to x2, reset it to
                            # 1 px width.
                            if (input$x1 >= input$x2) {
                              updateSliderInput(session, "x1", value = input$x2 - 1)
                              return
                            }
                            
                            # Do the actual cropping and display
                            img$cropxy(input$x1, input$x2, input$y1, input$y2)
                            output$plot2 <- renderPlot({
                              display(img$current_image, method = "raster")
                            })
                          })
                          
                          # Updates the y1 (top left) slider based on error checking
                          observeEvent(input$y1, {
                            # If the y1 coord is greater or equal to y2, reset it to
                            # 1 px height.
                            if (input$y1 >= input$y2) {
                              updateSliderInput(session, "y1", value = input$y2 - 1)
                              return
                            }
                            
                            # Do the actual cropping and display
                            img$cropxy(input$x1, input$x2, input$y1, input$y2)      
                            output$plot2 <- renderPlot({
                              display(img$current_image, method = "raster")
                            })
                          })
                          
                          # Updates the x2 (bottom right) slider based on error checking
                          observeEvent(input$x2, {
                            # If the x2 slider is less than x1, reset it to 1 px.
                            if (input$x2 <= input$x1) {
                              updateSliderInput(session, "x2", value = input$x1 + 1)
                              return
                            }
                            # Do the actual cropping and display
                            img$cropxy(input$x1, input$x2, input$y1, input$y2)
                            output$plot2 <- renderPlot({
                              display(img$current_image, method = "raster")
                            })
                          })
                          
                          # Updates the x2 (bottom right) slider based on error checking
                          observeEvent(input$y2, {
                            # If the y2 slider is less than y1, reset it to 1 px.
                            if (input$y2 <= input$y1) {
                              updateSliderInput(session, "y2", value = input$y1 + 1)
                              return
                            }
                            
                            # Do the actual cropping and display
                            img$cropxy(input$x1, input$x2, input$y1, input$y2)
                            output$plot2 <- renderPlot({
                              display(img$current_image, method = "raster")
                            })
                          })
                          
                          # Double click cropping
                          observeEvent(input$plot1_dblclick, {
                            brush <- input$plot1_brush
                            # If we have a brush equipped
                            if (!is.null(brush)) {
                              startx = max(0, brush$xmin)
                              starty = max(0, brush$ymin)
                              xbound = input$x2 - input$x1
                              ybound = input$y2 - input$y1
                              endx = brush$xmax
                              endy = brush$ymax
                              
                              # Calculating actual bounds for the new subimage
                              if (endx > xbound)
                                endx = xbound
                              if (endy > ybound)
                                endy = ybound
                              
                              # Perform the actual cropping
                              img$cropxy(input$x1 + startx, input$x1 + endx, 
                                         input$y1 + starty, input$y1 + endy)
                              
                              # Calculate the offsets for updating the sliders
                              savedx1 = input$x1 + startx
                              savedx2 = input$x1 + endx
                              savedy1 = input$y1 + starty
                              savedy2 = input$y1 + endy
                              # Update the bounds on the GUI
                              updateSliderInput(session, "x1", value = savedx1)
                              updateSliderInput(session, "x2", value = savedx2)
                              updateSliderInput(session, "y1", value = savedy1)
                              updateSliderInput(session, "y2", value = savedy2)
                            }
                          })
                          
                          # Set the output image
                          output$img <- img$getimg()
                        }
                        
                        ui <- dashboardPage(
                          dashboardHeader(title = "ShinyImg GUI"),
                          dashboardSidebar(
                            # The sliders on the sidebar
                            sliderInput("brightness", "Image Brightness", -10, 10, 0),
                            sliderInput("contrast", "Image Contrast", -10, 10, 0),
                            sliderInput("x1", "X1 Crop", 0, img$size()[1], 0),
                            sliderInput("x2", "X2 Crop", 0, img$size()[1], img$size()[1]),
                            sliderInput("y1", "Y1 Crop", 0, img$size()[2], 0),
                            sliderInput("y2", "Y2 Crop", 0, img$size()[2], img$size()[2])
                          ),
                          dashboardBody(
                            # Boxes need to be put in a row (or column)
                            fluidRow(
                              box(width = 12, 
                                  background = "black", 
                                  plotOutput("plot2", 
                                             height = img$size()[2]/2, width = img$size()[1]/2,
                                             dblclick = "plot1_dblclick",
                                             brush = brushOpts(
                                               id = "plot1_brush",
                                               resetOnNew = TRUE
                                             )
                                  ))
                            )
                          )
                        )
                        cat("ShinyImg GUI will now start running.\n")
                        cat("Please use the close button in the GUI to stop the server.")
                        shinyApp(ui = ui, server = server)
                      }))