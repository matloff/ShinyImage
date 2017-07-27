library(shiny)
library(EBImage)
library(shinyjs)

validate((need(file.exists("imagehistory.R"), "Please input imagehistory.R into the same directory that contains updatedShiny.R")))
source('imagehistory.R')

#version 18
#synchronizes shinyimg and img

if (!require("shiny")) {
	cat("shiny is not installed. Please install it first.")
}

if (!require("EBImage")) {
	cat("EBImage is not installed. Please install it first.")
}

#needed to hide and show the keep button 
#if clicked accidentally, it causes errors
if (!require("shinyjs")) {
	cat("shinyjs is not installed. Please install it first.")
}

# ______________________ start of UI _____________________
ui <- fluidPage( 
  shinyjs::useShinyjs(),

  titlePanel("Shiny Image"),

  tabsetPanel(
    tabPanel("Image Editor",

      sidebarLayout(
        sidebarPanel(
          radioButtons("radio", label = ("Sample or Upload Image"), 
            choices = list("Sample" = 1, "Upload Image" = 2, "Upload Link" = 3, "Upload Image Log" = 4), selected = 1),
          conditionalPanel(
            condition = "input.radio == 2",
            fileInput(inputId = 'file1',
              label = 'Upload Image',
              placeholder = 'JPEG, PNG, and TIFF are supported',
              accept = c(
                "image/jpeg",
                "image/x-png",
                "image/tiff",
                ".jpg",
                ".png",
                ".tiff"))
          ),
          conditionalPanel(
            condition = "input.radio == 3",
            textInput(inputId = "url1", 
              label = 'Upload URL', 
              placeholder = 'JPEG, PNG, and TIFF are supported',
              value = '')
          ),
          #********UPDATE*******
          conditionalPanel(
          	condition = "input.radio == 4", 
          	fileInput(inputId = 'file2', 
          		label = 'Upload .si file', 
          		placeholder = "Must be a ShinyImg Object", 
          		accept = c(
          			"file/si", 
          			".si"))
          ),
          radioButtons("color", label = ("Color Mode"), choices = list("Color" = 1, "Greyscale" = 2), selected = 1),
          sliderInput("bright", "Increase/Decrease Brightness:", min = -10, max = 10, value = 0, step = 0.1),
          sliderInput("contrast", "Increase/Decrease Contrast:", min = -10, max = 10, value = 0, step = 0.1), 
          sliderInput("gamma", "Increase/Decrease Gamma Correction", min = 0, max = 50, value = 1, step = 0.5),
          sliderInput("rotation", "Rotate Image", min = 0, max = 360, value = 0, step = 1),
          sliderInput("blurring", "Blur Image", min = 0, max = 20, value = 0, step = 1),
          tags$head(tags$style(HTML('#button1{background-color:red}'))),
          tags$head(tags$style(HTML('#button2{background-color:red}'))),
          actionButton("button1", "Undo"), 
          actionButton("button2", "Redo"), 
          actionButton("button3", "Reset"), 
          textOutput("dimetext"), #dimensions of the picture 
          textOutput("help"),
          textOutput("idReader")
        ),
        mainPanel(
          HTML(
            paste(
              h3('Image Editor', align = "center"),
              plotOutput("plot1",
                click = "plot_click",
                dblclick = "plot_dblclick",
                hover = "plot_hover",
                brush = "plot_brush"
              ),
              '<br/>',
              h6('What the shinyimg object looks like; temporary (will become the main plot or in sync with the mainplot)'),
              plotOutput("plot3"),
              '<br/>',
              column(6, downloadButton("download1", label = "Download Image")),
              #column(6, downloadButton("download3", label = "Download Image Log")),
              #*******UPDATE******
              column(6, actionButton("download4", label = "Download Image Log")),
              '<br/>',
              tags$style(type='text/css', "#download1 { display: block; width:100%; margin-left: auto; margin-right:auto;}"),
              #tags$style(type='text/css', "#download3 { display:block; width:100%; margin-left: auto; margin-right:auto;}"),
              #*********UPDATE********
              tags$style(type='text/css', "#download4 { display:block; width:100%; margin-left: auto; margin-right:auto;}"),

              '<br/>',

              h4('Preview Crop', align = "center"),
              h6('Click and drag where you would like to crop the photo. To save the cropped version, press keep', align = "center"),
              '<br/>',
              #textOutput("txt1"),
              plotOutput("plot2"),
              tags$style(type='text/css', "#keep { display:block; width:10%; margin-left: auto; margin-right:auto;}"),
              '<br/>',
              shinyjs::hidden(
                actionButton("keep", label = "Keep")
              ),
              '<br/>',

              verbatimTextOutput("info")
            )
          )
        )
      )
    ), 
    #*****UPGRADE******
    tabPanel("View Image Log", 
      sidebarLayout(
        sidebarPanel(
          fileInput(inputId = 'file3', 
          label = 'Upload .si file', 
          placeholder = "Must be a ShinyImg Object", 
          accept = c(
            "file/si", 
            ".si"))
        ), 
        mainPanel(
          h3("Image Log"), 
          verbatimTextOutput("ImageLog")
        )
      )
    ) #end of tabpanel 2
  ) 
)

# ______________________ start of server _____________________
server <- function(input, output, session) {
  options(shiny.maxRequestSize=30*1024^2) #file can be up to 30 mb; default is 5 mb

  #defining values for those that aren't included in shiny img
  imageFile <- reactiveValues(
    img_origin = NULL, 
    current = NULL, 
    bright = 0, 
    contrast = 0, 
    gamma = 1, 
    rotation = 0, 
    blurring = 0)
  #**********UPGRADE**********
  shinyImageFile <- reactiveValues(shiny_img_origin = NULL)

  #currently the last valid file is displayed till either of the observeEvent conditions are met
  #e.g. user needs to click radio1, input a new file, or input a new URl
  #until they do so the last button clicked will be the original previous image
  #meaning that the brightness, contrast, gamma correction, and crop will not save

  #if they click radio button1 (sample), they will get the imageFile will be the color chart
  #TODO -- if other radio buttons are clicked; display nothing
  #currently everything is displayed

  observe({
  	if(input$radio == 1)
  	{
    	imageFile$img_origin <- readImage('https://s-media-cache-ak0.pinimg.com/736x/62/c7/59/62c75942f58a0579f384fccc499a54f3--flower-crowns-flower-girls.jpg')
    	 #**********UPGRADE**********

    	output$plot3 <- renderPlot(shinyImageFile$shiny_img_origin <- shinyimg$new('https://s-media-cache-ak0.pinimg.com/736x/62/c7/59/62c75942f58a0579f384fccc499a54f3--flower-crowns-flower-girls.jpg'))
	  }
  })

#********UPGRADE**********

#//////// CDOE FOR RADIO BUTTONS /////////////
  #when user uploads file
  #the datapath is different from the one needed to properly recognize photo
  #so this function renames the file 
  renameUpload <- function(inFile) {
    if(is.null(inFile))
      return(NULL)

    oldNames = inFile$datapath
    newNames = file.path(dirname(inFile$datapath), inFile$name)
    file.rename(from = oldNames, to = newNames)
    inFile$datapath <- newNames

    return(inFile$datapath)
  }

  #if they enter a new file, their file will become the new imageFile
  observeEvent(input$file1, {
    imageFile$img_origin <- readImage(renameUpload(input$file1))
    #**********UPGRADE**********
    output$plot3 <- renderPlot(shinyImageFile$shiny_img_origin <- shinyimg$new(renameUpload(input$file1)))
    output$plot1 <- renderPlot(display(imageFile$img_origin, method = "raster"))

  })

  #if they enter a new url, their url will become the new new imageFile
  observeEvent(input$url1, {
    validate(need(input$url1 != "", "Must type in a valid jpeg, png, or tiff"))
    if (is.null(input$url1))
      imageFile$img_origin <- NULL
    else 
    {
    	imageFile$img_origin <- readImage(input$url1)
    	#**********UPGRADE**********
    	output$plot3 <- renderPlot(shinyImageFile$shiny_img_origin <- shinyimg$new(input$url1))
    }
    output$plot1 <- renderPlot(display(imageFile$img_origin, method = "raster"))

  })

  #if user uploads an image log, they will see the picture with previous settings
  #TODO: sliders need to update as well 
  observeEvent(input$file2, {
    output$plot3 <- renderPlot(shinyImageFile$shiny_img_origin <- shinyload(renameUpload(input$file2)))
    #output$plot3 <- renderPlot(shinyImageFile$shiny_img_origin$render())
  })

#//////// END OF CODE FOR RADIO BUTTONS /////////////

#//////// CODE FOR CROPPING AND PLOTS /////////////

  #prompts shiny to look at recursive crop
  observe({
    recursiveCrop()
  })

  #only executes when keep is clicked 
  recursiveCrop <- eventReactive(input$keep,{
    imageFcn()
    isolate({
    	imageFile$img_origin <<- cropped(imageFile$current)
    	p <- input$plot_brush #**********UPGRADE**********
    	output$plot3 <- renderPlot(shinyImageFile$shiny_img_origin$cropxy(p$xmin,p$xmax,p$ymin,p$ymax)) #**********UPGRADE**********
    })
    session$resetBrush("plot_brush")
    #shinyjs::hide("keep")
    updateSliderInput(session, "rotation", value = 0)
    output$plot1 <- renderPlot(display(imageFile$img_origin, method = "raster"))

    #output$plot3 <- renderPlot(shinyImageFile$shiny_img_origin$render()) #**********UPGRADE**********
  })
  
  #hides the keep button in the instance in which the user highlighted the plot
  #then clicks on the side so that the brush plot disappears
  #if user clicks keep without a valid brush, there will be errors
  #so we need to hide the button
  observeEvent(is.null(input$plot_brush), {
      shinyjs::hide("keep")
  })
  
  #returns image that is recursively updated
  imageFcn <- reactive ({
    return(imageFile$current)
  })
  
  #function that crops the image based on the plot brush 
  cropped <- function(image)
  {
    p <- input$plot_brush
    validate(need(p != 'NULL', "Highlight a portion of the photo to see a cropped version!"))
    validate(need(p$xmax <= dim(image)[1], "Highlighted portion is out of bounds on the x-axis of your image 1"))
    validate(need(p$ymax <= dim(image)[2], "Highlighted portion is out of bounds on the y-axis of your image 1"))
    validate(need(p$xmin >= 0, "Highlighted portion is out of bounds on the x-axis of your image 2"))
    validate(need(p$ymin >= 0, "Highlighted portion is out of bounds on the y-axis of your image 2"))

    return(image[p$xmin:p$xmax,p$ymin:p$ymax,])
  }

  output$plot1 <- renderPlot({
    display(imageFile$current, method = "raster")
  })

  #displays a cropped image of plot1's imageFile 
  output$plot2 <- renderPlot({
    p <- input$plot_brush
    validate(need(p != 'NULL', "Highlight a portion of the photo to see a cropped version!"))
    validate(need(p$xmax <= dim(imageFile$current)[1], "Highlighted portion is out of bounds on the x-axis of your image 1"))
    validate(need(p$ymax <= dim(imageFile$current)[2], "Highlighted portion is out of bounds on the y-axis of your image 1"))
    validate(need(p$xmin >= 0, "Highlighted portion is out of bounds on the x-axis of your image 2"))
    validate(need(p$ymin >= 0, "Highlighted portion is out of bounds on the y-axis of your image 2"))

    display(cropped(imageFile$current), method = "raster")

    shinyjs::show("keep")
  })

  #**********UPGRADE**********
  output$plot3 <- renderPlot({
  	shinyImageFile$shiny_img_origin$render()
  })

  #**********UPGRADE**********

  observe({
    #blur image --> rotate --> color correct --> render image with a filter
    if (input$blurring > 0) 
      blurredImage <- gblur(imageFile$img_origin, sigma = input$blurring)

    if (input$blurring == 0)
      blurredImage <- imageFile$img_origin
    
    #rotate blurred image
    rotatedImage <- rotate(blurredImage, input$rotation)

    #color correct rotated blurred image
    correctedImage <- rotatedImage ^ input$gamma * (1 + (input$contrast / 10)) + (input$bright / 10)

    #change how the image is rendered at the end
    if (input$color == 2)
      colorMode(correctedImage) <- Grayscale
    else 
      colorMode(correctedImage) <- Color

    imageFile$current <- correctedImage

    output$plot1 <- renderPlot(display(imageFile$current, method = "raster"))
  })

  #need these observeEvents for future uses of undo/redo
  observeEvent(input$bright, {
    #shiny img -- will be able to change the object 
    output$plot3 <- renderPlot(shinyImageFile$shiny_img_origin$set_brightness(input$bright / 10))
    #output$plot3 <- renderPlot(shinyImageFile$shiny_img_origin$render())

    #regular img
    imageFile$bright <- input$bright / 10
  })

  observeEvent(input$contrast, {
    #shiny img
    actualContrast = 1 + (input$contrast / 10)
    output$plot3 <- renderPlot(shinyImageFile$shiny_img_origin$set_contrast(actualContrast))
    #output$plot3 <- renderPlot(shinyImageFile$shiny_img_origin$render())

    #regular img
    imageFile$contrast <- 1 + (input$contrast / 10)
  })

  observeEvent(input$gamma, {
    imageFile$gamma <- input$gamma 
  })

  observeEvent(input$rotation, {
    imageFile$rotation <- input$rotation 
  })

  observeEvent(input$blurring, {
    imageFile$blurring <- input$blurring 
  })


#//////// END OF CODE FOR CROPPING AND PLOTS /////////////

#//////// CODE FOR RESET /////////////

  #if user clicks button3 (reset), then we will make imageFile become the original file 
  #also resets brightness, contrast, and gamma correction
  #and resets plot_brush 
  observeEvent(input$button3, {
    if(input$radio == 1)
    {
      imageFile$img_origin <- readImage('https://s-media-cache-ak0.pinimg.com/736x/62/c7/59/62c75942f58a0579f384fccc499a54f3--flower-crowns-flower-girls.jpg')   
      #**********UPGRADE**********
      output$plot3 <- renderPlot(shinyImageFile$shiny_img_origin <- shinyimg$new('https://s-media-cache-ak0.pinimg.com/736x/62/c7/59/62c75942f58a0579f384fccc499a54f3--flower-crowns-flower-girls.jpg'))   
    }
    if(input$radio == 2)
    {
      imageFile$img_origin <- readImage(renameUpload(input$file1))
      #**********UPGRADE**********
      output$plot3 <- renderPlot(shinyImageFile$shiny_img_origin <- shinyimg$new(renameUpload(input$file1)))
    }
    if(input$radio == 3)
    {
      imageFile$img_origin <- readImage(input$url1)
      #**********UPGRADE**********
      output$plot3 <- renderPlot(shinyImageFile$shiny_img_origin <- shinyimg$new(input$url1))
    }

    imageFile$bright = 0
    imageFile$contrast = 0
    imageFile$gamma = 1
    imageFile$rotation = 0 
    imageFile$blurring = 0 

    updateSliderInput(session, "bright", value = 0)
    updateSliderInput(session, "contrast", value = 0)
    updateSliderInput(session, "gamma", value = 1)
    updateSliderInput(session, "rotation", value = 0)
    updateSliderInput(session, "blurring", value = 0)
    updateRadioButtons(session, "color", selected = 1)
    session$resetBrush("plot_brush")
  })

    observe({
    #if user clicks a new radio button, uploads new file, or url
    #the sliders will change
    #and the brush will default 
    input$file1
    input$url1
    input$radio
    
    updateSliderInput(session, "bright", value = 0)
    updateSliderInput(session, "contrast", value = 0)
    updateSliderInput(session, "gamma", value = 1)
    updateSliderInput(session, "rotation", value = 0)
    updateSliderInput(session, "blurring", value = 0)
    updateRadioButtons(session, "color", selected = 1)
    session$resetBrush("plot_brush")

    #assuming this wouldn't be an issue for the shinyimg object
    #since the original image is the default
  })

#//////// END OF CODE FOR RESET /////////////

#//////// CODE FOR UNDO AND REDO /////////////

  #**********UPGRADE**********
  #undo button
  observeEvent(input$button1, {

  	#slider inputs are not changing correctly becuase image history isn't affected by undo/redo
  	#updateSliderInput(session, "bright", value = tail(shinyImageFile$shiny_img_origin$img_history, n = 1)[[1]]$brightness)
    #updateSliderInput(session, "contrast", value = tail(shinyImageFile$shiny_img_origin$img_history, n = 1)[[1]]$contrast)
    #updateSliderInput(session, "gamma", value = 1)
    output$plot3 <- renderPlot(
   if (!is.null(shinyImageFile$shiny_img_origin$undo())) {
    showModal(modalDialog(
      title = "Important message", 
      "No more actions to undo!"))
    }
    )
    #output$plot3 <- renderPlot(shinyImageFile$shiny_img_origin$undo())

    #output$plot3 <- renderPlot(shinyImageFile$shiny_img_origin$render())
  })

  #**********UPGRADE**********
  #redo button 
  observeEvent(input$button2, {
    output$plot3 <- renderPlot(
    if (!is.null(shinyImageFile$shiny_img_origin$redo())) {
    showModal(modalDialog(
      title = "Important message", 
      "No more actions to redo!"))
    }
    )

  	#output$plot3 <- renderPlot(shinyImageFile$shiny_img_origin$redo())


  	#slider inputs are not changing correctly becuase image history isn't affected by undo/redo
  	#updateSliderInput(session, "bright", value = tail(shinyImageFile$shiny_img_origin$img_history, n = 1)[[1]]$brightness)
    #updateSliderInput(session, "contrast", value = tail(shinyImageFile$shiny_img_origin$img_history, n = 1)[[1]]$contrast)
    #updateSliderInput(session, "gamma", value = 1)
    #output$plot3 <- renderPlot(shinyImageFile$shiny_img_origin$render())

  })

  #can use observe shinyImageFile$brightnes... to find how to change the sliderinputs 

  #observe({
    #can update sliderinput using shiny img or regular img 
    #updateSliderInput(session, "bright", value = shinyImageFile$shiny_img_origin$brightness * 10)
    #updateSliderInput(session, "contrast", value = (1 - shinyImageFile$shiny_img_origin$contrast) * 10)
    #updateSliderInput(session, "bright", value = imageFile$bright * 10)
    #updateSliderInput(session, "contrast", value = (imageFile$contrast - 1) * 10)
    #updateSliderInput(session, "gamma", value = imageFile$gamma)
    #updateSliderInput(session, "rotation", value = imageFile$rotation)
    #updateSliderInput(session, "blurring", value = imageFile$blurring)
  #})

  #DO UNDO/REDO 


#//////// END OF CODE FOR UNDO AND REDO /////////////

#//////// CODE FOR HELPFUL TEXTS /////////////

  #creates an ID for the image log 
  imgID <- reactive({
    if(input$radio == 1)
      id <- paste0("id1-sample")
    if(input$radio == 2)
      id <- paste0("id2-", input$file1[[1]])
    if(input$radio == 3)
      id <- paste0("id3-", input$url1)
    if(input$radio == 4)
      id <- paste("id4-prevSaved")
  return(id)
  })

  #textbox for user to see what the image ID is
  output$idReader <- renderText({
    paste0("ID: ", imgID())
  })

  #helpful text to show the user the crop function 
  #output$txt1 <- renderText({
  #    "Click and drag where you would like to crop the photo. To save the cropped version, press keep."
  #})

  #creates the textbox below plot2 about the plot_brush details and etc
  output$info <- renderText({
    xy_str <- function(e) {
      if(is.null(e)) return("NULL\n")
      paste0("x=", round(e$x, 1), " y=", round(e$y, 1), "\n")
    }
    xy_range_str <- function(e) {
      if(is.null(e)) return("NULL\n")
      paste0("xmin=", round(e$xmin, 1), " xmax=", round(e$xmax, 1), 
             " ymin=", round(e$ymin, 1), " ymax=", round(e$ymax, 1))
    }

    paste0(
      "click: ", xy_str(input$plot_click),
      "dblclick: ", xy_str(input$plot_dblclick),
      "hover: ", xy_str(input$plot_hover),
      "brush: ", xy_range_str(input$plot_brush)
    )
  })

#//////// END OF CODE FOR HELPFUL TEXTS /////////////

#//////// CODE FOR DOWNLOAD BUTTONS /////////////

  #_________________DOWNLOAD ____________________
  #TODO: if user uploads a .png file, should return same type of file
  #currently only returns jpeg because all three radio buttons dont have an image type

  #allows user to download plot1 - imageFile
  output$download1 <- downloadHandler('temp.jpeg', function(file) {
    #writeImage(imageFile$img_origin ^ input$gamma * input$contrast + input$bright, file)
    writeImage(imageFile$current, file)

  })

  #creates helpful text on sidebar about the dimensions 
  output$dimetext <- renderText({
    paste0("x axis: ", dime1(), " y axis: ", dime2())
  })

  #creates x dimensions for image log
  dime1 <- reactive({
    dim(imageFile$img_origin)[1]
  })

  #creates y dimensions for image log
  dime2 <- reactive({
    dim(imageFile$img_origin)[2]
  })  

#*******UPDATE*********
  observeEvent(input$download4, {
  	shinyImageFile$shiny_img_origin$save()
  	#creates a pop up window 
  	showModal(modalDialog(
  		title = "Important message", 
  		"Check your current directory for workplace.si for Image History!"))
  })

#//////// END OF CODE FOR DOWNLOAD BUTTONS /////////////

#//////// CODE FOR IMAGE LOG VIEWER /////////////

#--------------SECOND PANEL: shows user image log-------------
#*******UPDATE************
  observeEvent(input$file3, {
    shinyImageFile$shiny_img_origin <- shinyload(renameUpload(input$file2))
    output$ImageLog <- renderPrint({shinyImageFile$shiny_img_origin$img_history})
  })

  #TODO: include image log of current image
#//////// END OF CODE FOR IMAGE LOG VIEWER /////////////

} #end of server
shinyApp(ui, server)


#-------- LINKS THAT I'M STILL USING TO HELP ME CREATE THE SHINY GUI ----------
#links to creating action button 
#https://shiny.rstudio.com/articles/action-buttons.html

#links to using ebimage
#https://www.r-bloggers.com/r-image-analysis-using-ebimage/

#ebimage documentation 
#https://www.bioconductor.org/packages/devel/bioc/manuals/EBImage/man/EBImage.pdf

#plotoutput options
#https://shiny.rstudio.com/reference/shiny/latest/plotOutput.html

#links to create brushing
#https://shiny.rstudio.com/articles/plot-interaction.html

#dynamic ui to fix upload button 
#https://shiny.rstudio.com/articles/dynamic-ui.html

#helps with error messages
#https://shiny.rstudio.com/articles/validation.html

#persistent data storage
#http://shiny.rstudio.com/articles/persistent-data-storage.html
#http://deanattali.com/2015/06/14/mimicking-google-form-shiny/#save

#download button -- input
#https://stackoverflow.com/questions/43663352/r-count-shiny-download-button-clicks

#themes
#https://shiny.rstudio.com/gallery/shiny-theme-selector.html

#recursion
#https://stackoverflow.com/questions/37128528/self-referencing-reactive-variables-in-shiny-r
#https://groups.google.com/forum/#!topic/shiny-discuss/8ouy0eS15Jc
#https://stackoverflow.com/questions/29716868/r-shiny-how-to-get-an-reactive-data-frame-updated-each-time-pressing-an-actionb
