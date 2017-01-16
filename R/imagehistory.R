

source("http://bioconductor.org/biocLite.R") #Install package
biocLite("EBImage")
library(EBImage)                             #Include EBImage Lib

# action object, can be extended to add more operations
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
  return(list(
    get_action=get_action
  ))
}


# img object, requires an input source for an image
img = function(inputImage) {
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
    img_history <<- c(img_history, action(brightness, contrast, 0, c(xy1, xy2)))
    actions <<- actions + 1
    render()
  }

  # apply changes previously done or jump to specific action
  applyAction = function(action) {
    args = action$get_action()
    current_image <<- local_img * 1
    current_image <<- current_image + args[1]
    current_image <<- current_image[args[4]:args[6], args[5]:args[7],]
  }

  # undo last move
  undo = function() {
    if (actions != 1) {
      actions <<- actions - 1
      applyAction(img_history[actions])
      render()
    } else {
      print("No action to undo")
    }

  }

  # redo last move
  redo = function() {
    if (actions < length(img_history)) {
      actions <<- actions + 1
      applyAction(img_history[actions])

      render()
    } else {
      print("No action to redo")
    }

  }


  # plot output of image
  render= function() {
    display(current_image, method="raster")
  }

  # adjust brightness
  # TODO: clamp brightness to set amount
  add_brightness = function() {
    #img_history = c(img_history, action()   )
    current_image <<- current_image + 0.1
    brightness <<- brightness + 0.1
    add_action()
  }

  # darken the image
  # TODO: clamp brightness to set amount
  remove_brightness = function() {
    current_image <<- current_image - 0.1
    brightness <<- brightness - 0.1
    add_action()
  }

  # add contrast
  add_contrast = function() {
    # To do
  }

  # remove contrast
  remove_contrast = function() {
    # To do
  }

  # use locator to get corners of an image. Automatically finds min and max coordinates.
  crop = function() {
    print("Select the two opposite corners of a rectangle on the plot.")
    location = locator(2)
    x1 = min(location$x[1], location$x[2])
    y1 = min(location$y[1], location$y[2])
    x2 = max(location$x[1], location$x[2])
    y2 = max(location$y[1], location$y[2])
    xy1 = c(x1, y1)
    xy2 = c(x2, y2)
    current_image <<- current_image[x1:x2, y1:y2,]
    add_action()
  }

  # add original image
  add_action()

  return(list(
    render=render,
    add_brightness=add_brightness,
    remove_brightness=remove_brightness,
    crop=crop,
    undo=undo,
    redo=redo
  ))
}
