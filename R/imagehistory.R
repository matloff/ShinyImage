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
#' siaction$new(0.1, 1, 0, crop, 1, 0, 0)
#' @field brightness Stores address of your lightning server.
#' @field contrast Stores id of your current session on the server.
#' @field gamma Stores url of the last visualization created by this object.
#' @field crop A double nested sequence of crops c\(c\(x1, y1\), c\(x2, y2\)\).
#' @field blur stores value of blur
#' @field rotate stores value of rotate 
#' @field grayscale stores value of colormode (1 if grayscale, 0 if color)
#' #' @section Methods:
#' \describe{
#'   \item{Documentation}{The user should not need to create an action object. This is a class used exclusively by a shinyimg to keep track of a set of changes.}
#'   \item{\code{new(brightness, contrast, gamma, crop)}}{This method is used to create object of this class with the appropriate parameters.}
#'
#'   \item{\code{get_action()}}{This method returns a c() list of the input parameters.}
#' }  
#'

siaction <- R6Class("siaction",
                    # Make this action mutable. TODO: Make it so that
                    # it doesn't need to be
                    lock_objects = FALSE,
                    public = list(
                      # Initialize all the values of this action
                      initialize = function(brightness, contrast, gamma, crop, blur, rotate, grayscale) {
                        private$brightness <- brightness
                        private$contrast <- contrast
                        private$gamma <- gamma
                        private$crop <- crop
                        private$blur <- blur
                        private$rotate <- rotate
                        private$grayscale <- grayscale
                      },
                      # Get the c()'d properties of this particular action
                      get_action = function() {
                        return (c(private$brightness, 
                                  private$contrast, 
                                  private$gamma, 
                                  private$crop, 
                                  private$blur, 
                                  private$rotate, 
                                  private$grayscale))
                      }
                    ),
                    private = list(
                      # Properties of this action
                      brightness = 0,
                      contrast = 0,
                      gamma = 0,
                      crop = NULL,
                      blur = 0, 
                      rotate = 0, 
                      grayscale = 0
                    )
)

#' An EBImage wrapper with integrated history tracking.
#'
#' @docType class
#' @importFrom EBImage display imageData Image colorMode readImage 
#' writeImage gblur combine channel rotate
#' @importFrom R6 R6Class
#' @export
#' @keywords data
#' @return Object of \code{\link{R6Class}} with manipulation functions.
#' @format \code{\link{R6Class}} object.
#' @examples
#' 
#' small_tiger = shinyimg$new(system.file("images","tiger_small.jpg",package="ShinyImage"))
#'
#' small_tiger$add_brightness() # Adds brightness to image
#'
#' small_tiger$add_contrast() #Adds contrast to image 
#' 
#' small_tiger$undo() # Undoes the brightness addition
#'
#' small_tiger$redo() # Redoes the brightness addition
#' 
#'\dontrun{
#'  
#' small_tiger$add_gamma() #Adds Gamma correction
#'
#' small_tiger$add_blur() #Adds blur to image 
#' 
#' small_tiger$add_rotate() #Adds rotation by 1 degree
#' 
#' small_tiger$save('save.si') # Saves the current state. The filename is optional.
#' 
#' small_tiger$load('save.si') # Loads from a previously saved state. The filename is optional. 
#' #Requires a previously instantiated shinyimg instance (argument provided to new can be null).
#' 
#'}
#'
#' @section Methods:
#' \describe{
#'   \item{Documentation}{The user should not need to create an action object. This is a class used exclusively by a shinyimg to keep track of a set of changes.}
#'   \item{\code{set_autodisplay()}}{Turns on automatic rendering of image. User must still initially call render() to display image}
#'   \item{\code{set_autodisplay_OFF()}}{Turns off automatic image rendering}
#'   \item{\code{new(img)}}{Default constructor. \code{img} can be either a URL or a location of a local image.}
#'   \item{\code{undo()}}{Undoes the last change done to this image. When the original image state is reached, no more undos are possible.}
#'   \item{\code{redo()}}{Redos the next action after an undo has been performed. Will no longer redo if there are no more undos to redo.}
#'   \item{\code{shinyUndo()}}{Undoes the last change done to this image without autorendering; used by Shiny. 
#'      When the original image state is reached, no more undos are possible.}
#'   \item{\code{redo()}}{Redos the next action after an undo has been performed without autorendering; used by Shiny. 
#'      Will no longer redo if there are no more undos to redo.}
#'   \item{\code{toggle_ll()}}{Returns status of lazy loading.}
#'   \item{\code{copy()}}{Returns a copy of the image.}
#'   \item{\code{add_brightness()}}{Adds brightness to the image.}
#'   \item{\code{remove_brightness()}}{Removes brightness (darkens) to the image.}
#'   \item{\code{add_contrast()}}{Adds contrast to the image.}
#'   \item{\code{remove_contrast()}}{Removes contrast from the image.}
#'   \item{\code{add_gamma()}}{Adds gamma correction to the image.}
#'   \item{\code{remove_gamma()}}{Remoevs gamma correction from the image.}
#'   \item{\code{add_blur()}}{Adds blur to the entire photo.}
#'   \item{\code{remove_blur()}}{Removes blur from the entire photo.}
#'   \item{\code{add_rotate()}}{Rotates image to the right.}
#'   \item{\code{remove_rotate()}}{Rotates image to the left.}
#'   \item{\code{set_brightness()}}{Sets the brightness of the image by number inputted.}
#'   \item{\code{set_contrast()}}{Sets the contrast of the image by number inputted.}
#'   \item{\code{set_gamma()}}{Sets the gamma correction of the image by number inputted.}
#'   \item{\code{set_blur()}}{Sets the blur of the image by number inputted.}
#'   \item{\code{set_rotate()}}{Sets the degree of rotation of the image by number inputted.}
#'   \item{\code{set_grayscale((num))}}{Sets the image to grayscale if 1 is inputted; Reverts the image back to colormode if 0 is inputted}
#'   \item{\code{crop()}}{Uses locator to get corners of an image. Automatically finds min and max coordinates. 
#'     After two points are selected, a cropping selection can be create in order to crop the image to the desired size.}
#'   \item{\code{cropxy()}}{Performs same action as crop but it is used by Shiny and the parameters are different.}
#'   \item{\code{get_raw()}}{Gets the raw matrix slices of the current image.}
#'   \item{\code{gethistory()}}{Returns a copy of the members of the shinyimg object stored in myhistory.}
#'   \item{\code{get_brightness()}}{Returns a copy of the value stored for brightness.}
#'   \item{\code{get_contrast()}}{Returns a copy of the value stored for contrast.}
#'   \item{\code{get_gamma()}}{Returns a copy of the value stored for gamma correction.}
#'   \item{\code{get_blur()}}{Returns a copy of the value stored for blur.}
#'   \item{\code{get_rotate()}}{Returns a copy of the value stored for rotation.}
#'   \item{\code{get_color()}}{Returns a copy of the value stored for grayscale/colormode.}
#'   \item{\code{get_imghistory()}}{Returns a copy of the list of image histories.}
#'   \item{\code{get_actions()}}{Returns a copy of the list of the input parameters.}
#'   \item{\code{checkRedo()}}{Returns a bool value to check the status of available Redoes; used by Shiny.}
#'   \item{\code{save(filepath)}}{Saves the current state to be resumed later. \code{filepath} has a default value of 'workspace.si'}
#'   \item{\code{saveImage(filepath)}}{Saves a jpg of the image.}
#'   \item{\code{load(filepath)}}{Loads a previously saved state. \code{filepath} has a default value of 'workspace.si'}
#'   \item{\code{size()}}{Returns the current image dimentions.}
#'   \item{\code{render()}}{Renders the current image.}
#'   \item{\code{toggle_render()}}{Toggles the automatic rendering after making a change. By default, this option is off.}
#'   }
#'   
shinyimg <- R6Class("shinyimg",
                    lock_objects = FALSE,
                    public = list(
                      # Constructor of the shinyimg class
                      initialize = function(inputImage = NULL, 
                                            autosave_filename = NULL) {
                        self$set_default()
                        private$startup(inputImage, autosave_filename)
                      },
                      # Resets this object's values to the default ones.
                      set_default = function() {
                        # Default brightness
                        private$myhistory = c(0,0,0,0,0,0,0,0,0,0)
                        private$brightness = 0
                        # Default Contrast
                        private$contrast = 1
                        # Default Gamma 
                        private$gamma = 1
                        # Default Blur
                        private$blur = 0
                        # Default Rotate
                        private$rotate = 0
                        # Default grayscale 
                        private$grayscale = 0 
                        # CURRENT Number of actions. Can be less than the
                        # Actual number of actions due to undos.
                        private$actions = 0
                        # Crop coordinates
                        private$xy1 = c(0, 0)
                        private$xy2 = NULL
                        # Crop offsets 
                        # (relative to top left, which is 0, 0)
                        private$xoffset = 0
                        private$yoffset = 0
                        # List of image histories
                        private$img_history = c()
                        # Variable to store the source image
                        private$local_img = NULL
                        # Variable to store the current image to display
                        private$current_image = NULL
                        # Option to have the output automatically rendered
                        # autodisplay is off till function is called
                        private$autodisplay = 0
                        # The filename used for the autosave in 
                        # case of crashes
                        #private$autosave_filename = "workspace.si"
                        #heere
                        private$autosave_filename = 
                          paste(format(Sys.time(), "%b%d%H%M%S"), "-", "workspace.si",sep="")
                        # Determines if we lazy load
                        private$lazy_load = 0
                        # The number of lazy actions we have done so far.
                        private$lazy_actions = 0
                        # bool value to determine if user can undo 
                      },
                      set_autodisplay = function() 
                      {
                        private$autodisplay = 1
                        #TODO: need to add self$render
                        #currently, unable to display image after calling set_autodisplay()
                      },
                      #turns off autodisplay
                      set_autodisplay_OFF = function()
                      {
                        private$autodisplay = 0
                      },
                      # Outputs the image as a plot
                      render = function() {
                        # Reset the lazy actions
                        private$lazy_actions = 0;
                        
                        # Apply all pending actions
                        private$applyAction(private$img_history[private$actions])
                        
                        # If we actually have an image currently, try to display it. 
                        if (!is.null(private$current_image)) {
                          display(private$current_image, method = "raster")
                        }
                      },
                      # Function to write the current state of the program to 
                      # file.
                      save = function(file = private$autosave_filename) {
                        # Generated action matrix done in O(1) time.
                        action_matrix <- matrix(NA, 
                                                nrow=length(private$img_history), 
                                                ncol=10)
                        # Fill in the history data
                        i = 1
                        for (item in private$img_history) {
                          history <- item$get_action()
                          # TODO: Map function perhaps?
                          action_matrix[i, ] <- c(history[1], history[2], 
                                                  history[3], history[4], 
                                                  history[5], history[6], 
                                                  history[7], history[8],
                                                  history[9], history[10])
                          i = i + 1
                        }
                        # Save the current action number
                        actions <- private$actions
                        # Save the current image as well
                        img <- imageData(private$local_img)
                        # Save everything to file.
                        base::save(action_matrix, actions, img, file=file)
                      },
                      # Counterpart to the save function, will load from
                      # previous save file.
                      load = function(file = private$autosave_filename) {
                        base::load(file)
                        # Generate the image history.
                        private$img_history = c()
                        
                        private$local_img <- Image(img)
                        
                        # Not sure if this fixes the issue -- Had
                        # some weird color issues like loading in
                        # black and white. This seems to have fixed the
                        # issue.
                        colorMode(private$local_img) = Color
                        
                        # FIll in the action matrix
                        for (i in 1:dim(action_matrix)[1]) {
                          private$add_action(action_matrix[i, 1],
                                             action_matrix[i, 2],
                                             action_matrix[i, 3],
                                             action_matrix[i, 4],
                                             action_matrix[i, 5],
                                             action_matrix[i, 6],
                                             action_matrix[i, 7],
                                             action_matrix[i, 8],
                                             action_matrix[i, 9],
                                             action_matrix[i, 10]
                          )
                        }
                        private$actions <- actions
                        
                        # Apply the latest action
                        private$applyAction(
                          private$img_history[private$actions])
                        
                        # TODO: See if this should go in applyAction 
                        # instead.
                        if (private$autodisplay) {
                          self$render()
                        }
                      },
                      # Uses the actions list (img_history) to undo the last
                      # done action. DOES NOT PRUNE THE LIST AT THIS POINT. 
                      undo = function() {
                        # If there are more actions to undo besides the 
                        # original
                        # image (aka action #1)
                        if (private$actions != 1) {
                          # Step back by one action
                          private$actions <- private$actions - 1
                          
                          # Apply the action.
                          private$applyAction(
                            private$img_history[private$actions])
                          
                          # TODO: IDEA. Lazy loading. Don't actually apply 
                          # the action UNTIL we're done undoing.
                          
                          # TODO: See if this autodisplay should be applied
                          # to the applyAction function instead.
                          if (private$autodisplay) {
                            self$render()
                          }
                        } else {
                          # There are no actions to undo.
                          print("No action to undo")
                        }
                      },
                      # undo function for Shiny app
                      shinyUndo = function() {

                        # same as undo
                        # but got rid of auto render
                        # and print statement
                        # cant call the above bc those two lines
                        # intefere with the shiny app 
                        if (private$actions != 1) {
                          # Step back by one action
                          private$actions <- private$actions - 1
                          
                          # Apply the action.
                          private$applyAction(
                            private$img_history[private$actions])
                          
                          return(1)
                        } else {
                          # There are no actions to undo.
                          return(0)
                        }
                      },
                      # Uses the actions list (img_history) to redo the last
                      # undone action.
                      redo = function() {
                        # If there are actions to redo
                        if (private$actions < length(private$img_history)) {
                          # Increment by one action, then apply it
                          private$actions <- private$actions + 1
                          private$applyAction(
                            private$img_history[private$actions])
                          
                          # TODO: IDEA. Lazy loading. Don't actually apply the
                          # action UNTIL we're done redoing.
                          
                          # TODO: See if this autodisplay should be applied to
                          # the applyAction function instead.
                          if (private$autodisplay) {
                            self$render()
                          }
                        } else {
                          # No actions to redo.
                          print("No action to redo")
                        }
                      },
                      # Redo action for Shiny app
                      shinyRedo = function() {
                        # same as Redo 
                        # without auto render and print statement
                        # utilized by Shiny
                        # If there are actions to redo
                        if (private$actions < length(private$img_history)) {
                          # Increment by one action, then apply it
                          private$actions <- private$actions + 1
                          private$applyAction(
                            private$img_history[private$actions])
                        
                          return(1)
                        } else {
                          # No actions to redo.
                          return(0)
                        }
                      },
                      # Returns status of lazy loading
                      toggle_ll = function() {
                        private$lazy_load <- 1 - private$lazy_load;
                        if (private$lazy_load == 1) {
                          if (private$lazy_actions != 0) {
                            private$applyAction(
                              private$img_history[private$actions])
                          }
                          cat("Lazy loading on")
                        } else {
                          cat("Lazy loading off")
                        }
                      },
                      # Returns a copy of this image. 
                      # One copy's changes will not affect the other.
                      copy = function() {
                        #TODO: Different options for cloning, 
                        # like collapsing history
                        return (self$clone())
                      },
                      # Adjusts brightness by 0.1. This is a good increment
                      # but a variable brightness function should be added.
                      add_brightness = function() {
                        # Adds 0.1 brightness.
                        private$mutator(1, 0.1)
                      },
                      
                      # Adjusts brightness by -0.1. This is a good decrement
                      # but a variable brightness function should be added.
                      remove_brightness = function() {
                        # removes 0.1 brightness.
                        private$mutator(1, -0.1)
                      },
                      
                      # Adjusts contrast by 0.1. This is a good increment
                      # but a variable contrast function should be added.
                      add_contrast = function() {
                        # Adds 0.1 contrast.
                        private$mutator(3, 0.1)
                      },
                      
                      # Adjusts contrast by -0.1. This is a good increment
                      # but a variable contrast function should be added.
                      remove_contrast = function() {
                        # removes 0.1 contrast.
                        private$mutator(3, -0.1)
                      },
                      
                      # Adjusts gamma by 0.5
                      add_gamma = function() {
                        private$mutator(5, 0.5)
                      },
                      
                      # Adjusts gamma by -0.5
                      remove_gamma = function() {
                        private$mutator(5, -0.5)
                      },

                      # Adjusts blur by 1
                      add_blur = function() {
                        private$mutator(7, 1)
                      }, 

                      # Adjusts blur by -1
                      remove_blur = function() {
                        private$mutator(7, -1)
                      }, 

                      # Adjusts rotate by 1 degree
                      add_rotate = function() {
                        private$mutator(9, 1)
                      }, 

                      # Adjusts rotate by -1 degree
                      remove_rotate = function() {
                        private$mutator(9, -1)
                      },

                      set_brightness = function(brightness) {
                        # Sets brightness.
                        private$mutator(2, brightness)
                      },
                      
                      set_contrast = function(contrast) {
                        # Sets brightness.
                        private$mutator(4, contrast)
                      },

                      set_gamma = function(gamma) {
                        private$mutator(6, gamma)
                        # Sets gamma correction
                      },

                      set_blur = function(blur) {
                        # Sets blur
                        private$mutator(8, blur)
                      }, 

                      set_rotate = function(rotate) {
                        # Sets rotation of image
                        private$mutator(10, rotate)
                      },

                      set_grayscale = function(grayscale) {
                        # Sets image to grayscale if argument is 1
                        # Else image is colormode
                        # Can revert image to colormode if argument is 0
                        private$mutator(11, grayscale)
                      },
                      
                      # The command line cropper uses locator to have the
                      # user locate the two corners of the subimage. 
                      crop = function() {

                        #possibly create a special instance for rotation
                        print("Select the two opposite corners 
                              of a rectangle on the plot.")
                        location = locator(2)
                        x1 = min(location$x[1], location$x[2])
                        y1 = min(location$y[1], location$y[2])
                        x2 = max(location$x[1], location$x[2])
                        y2 = max(location$y[1], location$y[2])
                        #comment and print here --
                        private$current_image <<- 
                          private$current_image[x1:x2,y1:y2,]
                        
                        # In order to maintain a correct cropping, 
                        # we need to know how much of
                        # the original image has already been cropped.
                        xdiff = x2 - x1
                        ydiff = y2 - y1
                        
                        # The offset is needed to maintain the ABSOLUTE 
                        # crop data.
                        private$xoffset = private$xoffset + x1
                        private$yoffset = private$yoffset + y1
                        
                        # Create the absolute crop data using the offsets
                        # and new area.
                        private$xy1 = c(private$xoffset, private$yoffset)
                        private$xy2 = c(private$xoffset + xdiff, 
                                        private$yoffset + ydiff)
                        private$add_action()
                      },
                      # The function used by Shiny to crop using absolute 
                      # coordinates. 
                      cropxy = function(x1, x2, y1, y2) {
                        #same as crop but used by shiny
                        private$current_image <<- 
                          private$current_image[x1:x2,y1:y2,]
                        
                        # In order to maintain a correct cropping, 
                        # we need to know how much of
                        # the original image has already been cropped.
                        xdiff = x2 - x1
                        ydiff = y2 - y1
                        
                        # The offset is needed to maintain the ABSOLUTE 
                        # crop data.
                        private$xoffset = private$xoffset + x1
                        private$yoffset = private$yoffset + y1
                        
                        # Create the absolute crop data using the offsets
                        # and new area.
                        private$xy1 = c(private$xoffset, private$yoffset)
                        private$xy2 = c(private$xoffset + xdiff, 
                                        private$yoffset + ydiff)
                        private$add_action()
                      },
                      # Returns the size of the current image.
                      # Needed for Shiny to determine the max values of
                      # the sliders. 
                      size = function() {
                        dim(private$current_image)
                      },
                      # Function that handles the automatic render toggle.
                      # Default autodisplay is off. 
                      toggle_render = function() {
                        private$autodisplay = 1 - private$autodisplay;
                        if (private$autodisplay) {
                          self$render()
                        }
                      },
                      # Gets the raw matrix slices of the current image
                      get_raw = function() {
                        return (imageData(private$current_img))
                      }, 
                      # saves image as a jpeg 
                      saveImage = function(file) {
                        if(missing(file))
                        {
                          writeImage(private$current_image, files = "temp.jpg")
                        }
                        else
                          writeImage(private$current_image, file)
                      }, 
                      # returns a copy of the members of the shinyimg object
                      gethistory = function() {
                      	return(private$myhistory)
                      }, 
                      # returns a copy of the brightness value
                      get_brightness = function() {
                        return (private$brightness)
                      },
                      # returns a copy of the contrast value
                      get_contrast = function() {
                        return (private$contrast)
                      },
                      # returns a copy of the gamma value
                      get_gamma = function() {
                        return (private$gamma)
                      },
                      # returns a copy of the blur value
                      get_blur = function() {
                        return (private$blur)
                      }, 
                      # returns a copy of the rotate value
                      get_rotate = function() {
                        return(private$rotate)
                      },
                      # returns a copy of the grayscale value
                      # indicates colormode
                      get_color = function() {
                      	return(private$grayscale)
                  	  },
                      # returns a copy of the list of image histories
                      get_imghistory = function() { 
                        return(private$img_history)
                      }, 
                      # returns a copy of the list of input parameters
                      get_actions = function() {
                        return(private$actions)
                      }, 
                      # returns a copy of the status of redoes available
                      checkRedo = function() {
                        if (private$actions < length(private$img_history))
                          return(TRUE)
                        else return(FALSE)
                      }
                      #Uses a matrix as the image. Can be used to reintegrate
                      # a get_raw generated matrix.
                      # Disabled as this feature could be abused. 
                      #import_matrix = function(m) {
                      #	private$set_default()
                      #	private$startup(NULL, NULL, m)
                      #}
                    ),
                    private = list(
                      # The following are the members of the shinyimg obj.
                      # Default brightness
                      myhistory = c(0,0,0,0,0,0,0,0,0,0),
                      #names(myhistory) <- c(1,2,3,4,5,6,7,8,9,10),
                      brightness = 0,
                      # Default Contrast
                      contrast = 1,
                      gamma = 1, 
                      blur = 0, 
                      rotate = 0, 
                      grayscale = 0, 
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
                      #autosave_filename = "workspace.si",
                      autosave_filename = 
                        paste(format(Sys.time(), "%b%d%H%M%S"), "-", "workspace.si",sep=""),
                      # Startup function. Can take a image, and/or
                      # the autosave filename.
                      lazy_load = 0,
                      # Determines if we apply the transfomation 
                      # immediately or not. 
                      # Defaults to off.
                      lazy_actions = 0,
                      
                      # The following are the private functions
                      mutator = function(actionID, amount) {
                        
                        # Add a lazy action count. 
                        private$lazy_actions <- private$lazy_actions + 1
                        
                        switch(actionID,
                               # ActionID 1, brightness adjustment
                               private$brightness <- 
                                 private$brightness + amount,
                               
                               # ActionID 2, brightness setting
                               private$brightness <- amount,
                               
                               #  ActionID 3, contrast adjustment
                               private$contrast <- 
                                 private$contrast + amount,
                               
                               #  ActionID 4, contrast setting
                               private$contrast <- amount,

                               # ActionID 5, gamm adjustment
                               private$gamma <- 
                                 private$gamma + amount, 

                               # ActionID 6, gamma setting
                               private$gamma <- amount,

                               # Action ID 7, blur adjustment
                               private$blur <- 
                                 private$blur + amount, 

                               # Action ID 8, blur setting
                               private$blur <- amount,

                               # Action ID 9, rotate adjustment
                               private$rotate <- 
                                 private$rotate + amount, 

                               # Action ID 10, rotate setting
                               private$rotate <- amount,

                               # Action ID 11, grayscale setting
                               private$grayscale <- amount
                        )
                        
                        private$add_action()
                      },
                      
                      # The main workhorse function for scribing an action.
                      # crop parameters refer to the top left x, top left y,
                      # bottom right x, bottom right y respectively. 
                      add_action = function(bright = private$brightness, 
                                            cont = private$contrast, 
                                            gam = private$gamma, 
                                            crop1x = private$xy1[1],
                                            crop1y = private$xy1[2], 
                                            crop2x = private$xy2[1], 
                                            crop2y = private$xy2[2],
                                            blurring = private$blur,
                                            rotation = private$rotate, 
                                            colorMode = private$grayscale
                                            
                      ) {
                        
                        # If we are not at the most recent image, we need 
                        # to prune the extra actions. 
                        if (private$actions < 
                            length(private$img_history)) {
                          private$img_history <- 
                            private$img_history[1:private$actions]
                        }
                        
                        # Use the siaction constructor to create a 
                        # new action and add it to the img_history list.
                        private$img_history <-
                          c(private$img_history, siaction$new(bright, 
                                                              cont, 
                                                              gam, 
                                                              c(
                                                                c(crop1x,crop1y), 
                                                                c(crop2x, crop2y)
                                                              ),
                                                              blurring, 
                                                              rotation, 
                                                              colorMode
                                                              ))
                        # Add one to the action counter because we just 
                        # added an action to the action list
                        private$actions <- private$actions + 1
                        
                        # If the autodisplay flag is on, render the 
                        # changes.
                        if (private$autodisplay) {
                          self$render()
                        }
                      },
                      
                      # This function generates a modified image
                      # from the original (local_img)
                      applyAction = function(action) {
                        # Unpack the action variable
                        dataframe = action[[1]]
                        # Use the action's getter to return the c()'d args
                        args = dataframe$get_action()
                        private$brightness = args[1]
                        private$contrast = args[2]
                        private$gamma = args[3]
                        private$xy1 = c(args[4], args[5])
                        private$xy2 = c(args[6], args[7])   
                        private$blur = args[8]
                        private$rotate = args[9]
                        private$grayscale = args[10]

                        # args[8] is blurring
                        if (args[8] > 0)
                        {
                          private$current_image <- 
                            gblur(private$local_img, sigma = args[8])
                        } 

                        #need to fix blur back to original image
                        if (args[8] <= 0)
                        {
                          private$current_image <- private$local_img
                        }      

                        # args[2] is contrast
                        private$current_image <- 
                          private$current_image * args[2]

                        # args[1] is brightness
                        private$current_image <- 
                          private$current_image + args[1]

                        # args[3] is gamma
                        private$current_image <- 
                          private$current_image ^ args[3]
 
                        # args[4] through args[7] are ABSOLUTE 
                        # crop locations.
                        private$current_image <- private$current_image[
                          args[4]:args[6], args[5]:args[7], 
                          ]

                        # args[10] is colormode
                        if (args[10] == 1)
                          private$current_image <- channel(private$current_image, "gray")
       
                        #args[11] is rotation
                        private$current_image <- rotate(private$current_image, args[9])

                        private$myhistory <- args

                      },
                      # The matr argument imports a matrix as the image.
                      # The remaining two arguments are supplied by the 
                      # constructors for shinyimg.
                      startup = function(inputImage, autosave_filename, 
                                         matr = NULL) {
                        # Set the autosave filename if it is not null
                        if (!is.null(autosave_filename))
                          private$autosave_filename <- autosave_filename
                        
                        if (!is.null(inputImage)) {
                          # Here the user passed in an argument for 
                          # inputImage We use the readImage functionality
                          # to read in the image to form an EBImage. This
                          # may be changed at a later time. 
                          private$local_img <- readImage(inputImage)
                          # Here we set the current image to the original 
                          # image. Multiplying by one essentially copies 
                          # the image. The reason this works is that the
                          # multiplication function when applied to an 
                          # image changes contrast. In this case, 1 is 
                          # the default contrast, and thus we are 
                          # essentially making a copy by not changing 
                          # the contrast but telling it to make another
                          # image. 
                          private$current_image <- private$local_img * 1
                          
                          # Here we set the xy2 coordinate, which is the
                          # lower right coordinate of the image. 
                          private$xy2 <- c(dim(private$local_img)[1], 
                                           dim(private$local_img)[2])
                          
                          # Add the "base" action, which is the original
                          # image. 
                          private$add_action()
                        } else if (!is.null(matr)) {
                          
                          # TODO: Possible that m is not actually a matrix.
                          # Could error. 
                          result = tryCatch({
                            private$local_img <- Image(matr)
                          }, warning = function(w) {
                            return
                          }, error = function(e) {
                            return
                          }, finally = {
                            
                          });
                          
                          # Here we set the current image to the original 
                          # image. Multiplying by one essentially copies 
                          # the image. The reason this works is that the
                          # multiplication function when applied to an 
                          # image changes contrast. In this case, 1 is 
                          # the default contrast, and thus we are 
                          # essentially making a copy by not changing 
                          # the contrast but telling it to make another 
                          # image. 
                          private$current_image <- private$local_img * 1
                          
                          # Here we set the xy2 coordinate, which is the 
                          # lower right coordinate of the image. 
                          private$xy2 <- c(dim(private$local_img)[1], 
                                           dim(private$local_img)[2])
                          
                          # Add the "base" action, which is the original 
                          # image. 
                          private$add_action()
                        } else {
                          # TODO: Maybe some sort of error message?
                          result = tryCatch({
                            private$load()
                          }, warning = function(w) {
                            
                          }, error = function(e) {
                            
                          }, finally = {
                            
                          });
                        }
                      }
                    )
)

#' Wrapper to load an image from a cold boot. 
#' 
#' @param filename The filename of a file previously generated by shinyimg's $save function. 
#' @examples
#' \dontrun{
#' 
#' local_tiger = shinyimg$new(system.file("images","sample.jpg",package="ShinyImage"))
#' # sample.jpg is titled A tiger in the water
#' #  By Bob Jagendorf 
#' #  [CC BY 2.0 (http://creativecommons.org/licenses/by/2.0)], 
#' #  via Wikimedia Commons
#' local_tiger$save("tiger.si")
#' # Restart R 
#' reloaded_tiger = shinyload("tiger.si")
#'
#' }
#' @export
shinyload = function(filename) {
  shinyimg$new(NULL, filename)
}
