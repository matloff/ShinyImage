library(EBImage)                             #Include EBImage Lib

#' An action represents the changes that have
#' been done to a certain file. Currently, it
#' contains brightness, contrast, gamma, and
#' crop information.
#' 
#' @param bright the current brightness value.
#' @param cont the current contrast value
#' @param gam the gamma value
#' @param crops the coordinates of crops
#' 
#' @return A concatenated sequence of action values.
#' @examples
#' Should not be used by the user.
#' 
action = function(bright, cont, gam, crops) {
  brightness = bright
  contrast = cont
  gamma = gam
  crop = crops

  # [1] brightness adjustment
  # [2] contrast adjustment
  # [3] gamma adjustment
  # [4] crop x1
  # [5] crop y1
  # [6] crop x2
  # [7] crop y2
  get_action = function() {
    return (c(brightness, contrast, gamma, crop))
  }

  # create object
  return(list(get_action = get_action))
}



#' An EBImage wrapper with integrated history tracking.
#' 
#' @param inputImage A URL or the local path of an image
#' @return A ShinyImg object with manipulation functions
#' @examples
#' local_tiger = shinyimg("Tigerwater_edit2.jpg")
#' web_tiger = shinyimg("https://upload.wikimedia.org/wikipedia/commons/1/1c/Tigerwater_edit2.jpg")
#'
shinyimg = function(inputImage) {
  # base image
  local_img = readImage(inputImage)
  # makes a copy to display
  current_image = local_img * 1

  # image controls
  brightness = 0
  contrast = 1
  actions = 0
  xy1 = c(0, 0)
  xy2 = c(dim(local_img)[1], dim(local_img)[2])
  img_history = c()

  # add an action to the history
  add_action = function() {
    if (actions < length(img_history))
      img_history <<- img_history[1:actions]
    img_history <<-
      c(img_history, action(brightness, contrast, 0, c(xy1, xy2)))
    actions <<- actions + 1
    render()
  }

  # apply changes previously done or jump to specific action
  applyAction = function(action) {
    args = action$get_action()
    current_image <<- local_img * 1
    current_image <<- current_image + args[1]
    current_image <<-
      current_image[args[4]:args[6], args[5]:args[7], ]
  }

  #' Undoes the last change done to this image. 
  #' When the original image state is reached,
  #' no more undos are possible.
  #' @examples
  #' local_tiger = shinyimg(\"Tigerwater_edit2.jpg\")
  #' local_tiger$add_brightness()
  #' local_tiger$undo() # Undoes the brightness addition
  undo = function() {
    if (actions != 1) {
      actions <<- actions - 1
      applyAction(img_history[actions])
      render()
    } else {
      print("No action to undo")
    }

  }

  #' Redos an undo'd action. Will no longer redo if there are no
  #' more undos to redo.
  #' @examples
  #' local_tiger = shinyimg("Tigerwater_edit2.jpg")
  #' local_tiger$add_brightness()
  #' local_tiger$undo() # Undoes the brightness addition
  #' local_tiger$redo() # Redoes the brightness addition
  redo = function() {
    if (actions < length(img_history)) {
      actions <<- actions + 1
      applyAction(img_history[actions])

      render()
    } else {
      print("No action to redo")
    }
  }

  #' Returns a Shiny compatible image
  #' @return a image that can be used in Shiny
  #' @examples
  #' local_tiger = shinyimg("Tigerwater_edit2.jpg")
  #' local_tiger$add_brightness()
  #' shiny_tiger = local_tiger$getimg() #Now usable by Shiny
  #' 
  
  getimg = function() {
    return (renderPlot({
      display(current_image, method = "raster")
    }))
  }


  # plot output of image
  render = function() {
    display(current_image, method = "raster")
  }

  #' Adds brightness to the image
  #' @examples
  #' local_tiger = shinyimg("Tigerwater_edit2.jpg")
  #' local_tiger$add_brightness()
  # TODO: clamp brightness to set amount
  add_brightness = function() {
    #img_history = c(img_history, action()   )
    current_image <<- current_image + 0.1
    brightness <<- brightness + 0.1
    add_action()
  }

  #' Removes brightness (darkens) to the image
  #' @examples
  #' local_tiger = shinyimg("Tigerwater_edit2.jpg")
  #' local_tiger$remove_brightness()
  # TODO: clamp brightness to set amount
  remove_brightness = function() {
    current_image <<- current_image - 0.1
    contrast <<- contrast - 0.1
    add_action()
  }

  #' Adds contrast to the image
  #' @examples
  #' local_tiger = shinyimg("Tigerwater_edit2.jpg")
  #' local_tiger$add_contrast()
  add_contrast = function() {
    current_image <<- current_image * 1.1
    contrast <<- contrast * 1.1
    add_action()
  }

  #' Removes contrast from the image
  #' @examples
  #' local_tiger = shinyimg("Tigerwater_edit2.jpg")
  #' local_tiger$remove_contrast()
  remove_contrast = function() {
    current_image <<- current_image * 0.9
    contrast <<- contrast * 0.9
    add_action()
  }

  # use locator to get corners of an image. Automatically finds min and max coordinates.
  #' After two points are selected, a cropping selection can be create
  #' in order to crop the image to the desired size.
  #' @examples
  #' local_tiger = shinyimg("Tigerwater_edit2.jpg")
  #' local_tiger$crop()
  crop = function() {
    print("Select the two opposite corners of a rectangle on the plot.")
    location = locator(2)
    x1 = min(location$x[1], location$x[2])
    y1 = min(location$y[1], location$y[2])
    x2 = max(location$x[1], location$x[2])
    y2 = max(location$y[1], location$y[2])
    xy1 = c(x1, y1)
    xy2 = c(x2, y2)
    current_image <<- current_image[x1:x2, y1:y2, ]
    add_action()
  }

  # add original image
  add_action()

  return(
    list(
      render = render,
      add_brightness = add_brightness,
      remove_brightness = remove_brightness,
      crop = crop,
      undo = undo,
      redo = redo,
      getimg = getimg
    )
  )
}

#' A Shiny application that requires a ShinyImg to edit
#' (most likely will be changed in the future)
#' 
#' @param img A ShinyImg to edit using the GUI editor
#' @examples
#' local_tiger = shinyimg("Tigerwater_edit2.jpg")
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

  server <- function(input, output) {
    output$distPlot <- renderPlot({
      hist(rnorm(input$obs), col = 'darkgray', border = 'white')
    })

    observeEvent(input$close, {
      stopApp()
    })
    output$img <- img$getimg()

    output$info <- renderText({
      paste0("x=", input$plot_click$x, "\ny=", input$plot_click$y)
    })

  }

  ui <- fluidPage(plotOutput("img", click = "plot_click"),
                  verbatimTextOutput("info"))
  cat("ShinyImg GUI will now start running.\n")
  cat("Please use the close button in the GUI to stop the server.")
  shinyApp(ui = ui, server = server)
}

start_gui(
  shinyimg(
    "https://upload.wikimedia.org/wikipedia/commons/1/1c/Tigerwater_edit2.jpg"
  )
)
